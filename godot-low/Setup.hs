{-# LANGUAGE OverloadedStrings #-}

-- | Custom setup for godot-low.
--
-- Before building (and before haddock / repl), generates the low-level
-- GDExtension bindings from the @gdextension_interface.json@ that the
-- godot-sys dependency installs next to its C headers:
--
-- * @Godot.GDExtension.Types@ (an @.hsc@ file, so struct sizes and offsets come
--   from the real C header via hsc2hs): aliases, handles, enums, structs and
--   function pointer types with @wrapper@ / @dynamic@ imports.
-- * @Godot.GDExtension.Interface@: a record with every interface function, and
--   'loadInterface' to fill it in from Godot's @get_proc_address@.
--
-- and the Godot class API (@Godot.Api.*@) from the @extension_api.json@ that
-- godot-sys installs alongside it:
--
-- * @Godot.Api.Builtin.Types@ / @Godot.Api.Builtin.\<Name\>@: builtin types
--   (Vector3, String, Array, ...) and their constructors, methods, operators.
-- * @Godot.Api.Classes@ / @Godot.Api.Class.\<Name\>@: engine classes (Node,
--   Node3D, ...), their hierarchy, enums, constants and methods.
-- * @Godot.Api.GlobalEnums@, @Godot.Api.Utility@: global enums and functions.
--
-- Classes that the build options godot-sys built Godot with remove
-- (@disable_3d@, ...) are left out, using the SCons arguments and the table of
-- the classes each option removes that godot-sys installs alongside.
--
-- The Godot.Api module list depends on the API, so configure adds it to the
-- library (exposed + autogen modules), and the later steps set it again, in
-- case godot-sys was rebuilt with other options since.
--
-- Files are written into the package's autogen directory, and only when
-- their contents change.
module Main (main) where

import Control.Monad (filterM, when)
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Char (isAlphaNum, isSpace, isUpper, toLower, toUpper)
import Data.List (intercalate, isPrefixOf, isSuffixOf, nub)
import Data.String (fromString)
import Distribution.PackageDescription (BuildInfo (..), Library (..), PackageDescription (..))
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Distribution.InstalledPackageInfo as IPI
import Distribution.Simple
import Distribution.Simple.BuildPaths (autogenComponentModulesDir)
import Distribution.Types.ComponentName (ComponentName (..))
import Distribution.Types.LibraryName (LibraryName (..))
import qualified Distribution.Compat.Graph as Graph
import Distribution.Types.ComponentLocalBuildInfo (ComponentLocalBuildInfo (..))
import Distribution.Types.ExposedModule (ExposedModule (..))
import Distribution.Types.LocalBuildInfo (componentNameCLBIs)
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo (..), interpretSymbolicPathLBI)
import Distribution.Simple.PackageIndex (lookupPackageName)
import Distribution.Simple.Setup
  ( buildVerbosity
  , fromFlagOrDefault
  , haddockVerbosity
  , replVerbosity
  )
import Distribution.Simple.Utils (info, notice)
import Distribution.Verbosity (Verbosity, normal)
import Distribution.Pretty (prettyShow)
import System.Directory
import System.FilePath

main :: IO ()
main =
  defaultMainWithHooks
    simpleUserHooks
      { confHook = \args flags -> do
          lbi <- confHook simpleUserHooks args flags
          x <- readEngineApi normal lbi
          pure (setApiModules x lbi)
      , -- The module list is computed again at each step after configure,
        -- because godot-sys can be rebuilt with other build options (which
        -- remove classes) without godot-low being configured again.
        buildHook = \_ lbi hooks flags -> do
          x <- generate (fromFlagOrDefault normal (buildVerbosity flags)) lbi
          withApiModules x lbi $ \pd lbi' -> buildHook simpleUserHooks pd lbi' hooks flags
      , haddockHook = \_ lbi hooks flags -> do
          x <- generate (fromFlagOrDefault normal (haddockVerbosity flags)) lbi
          withApiModules x lbi $ \pd lbi' -> haddockHook simpleUserHooks pd lbi' hooks flags
      , replHook = \_ lbi hooks flags args -> do
          x <- generate (fromFlagOrDefault normal (replVerbosity flags)) lbi
          withApiModules x lbi $ \pd lbi' -> replHook simpleUserHooks pd lbi' hooks flags args
      , copyHook = \_ lbi hooks flags -> do
          x <- readEngineApi normal lbi
          withApiModules x lbi $ \pd lbi' -> copyHook simpleUserHooks pd lbi' hooks flags
      , instHook = \_ lbi hooks flags -> do
          x <- readEngineApi normal lbi
          withApiModules x lbi $ \pd lbi' -> instHook simpleUserHooks pd lbi' hooks flags
      , regHook = \_ lbi hooks flags -> do
          x <- readEngineApi normal lbi
          withApiModules x lbi $ \pd lbi' -> regHook simpleUserHooks pd lbi' hooks flags
      }

withApiModules :: ExtApi -> LocalBuildInfo -> (PackageDescription -> LocalBuildInfo -> IO a) -> IO a
withApiModules x lbi k = let lbi' = setApiModules x lbi in k (localPkgDescr lbi') lbi'

--------------------------------------------------------------------------------
-- Driver

-- | Generate the bindings, and return the API they were generated from.
generate :: Verbosity -> LocalBuildInfo -> IO ExtApi
generate verb lbi = do
  json <- findInterfaceJson lbi
  info verb ("godot-low: generating bindings from " ++ json)
  api <- either (fail . (("godot-low: cannot parse " ++ json ++ ": ") ++)) pure =<< eitherDecodeFileStrict json
  libAutogen <- libraryAutogenDir lbi
  let outDir = libAutogen </> "Godot" </> "GDExtension"
      env = mkEnv api
  createDirectoryIfMissing True outDir
  w1 <- writeIfChanged (outDir </> "Types.hsc") (genTypes env api)
  w2 <- writeIfChanged (outDir </> "Interface.hs") (genInterface env api)
  when (w1 || w2) $ notice verb ("godot-low: generated GDExtension bindings in " ++ outDir)

  x <- readEngineApi verb lbi
  let aenv = mkAEnv x
      root = libAutogen
      file comps = root </> joinPath comps <.> "hs"
      outputs =
        [ (["Godot", "Api", "Builtin", "Types"], genBuiltinTypes aenv)
        , (["Godot", "Api", "Classes"], genClasses aenv)
        , (["Godot", "Api", "GlobalEnums"], genGlobalEnums aenv x)
        , (["Godot", "Api", "Utility"], genUtility aenv x)
        ]
          ++ [(["Godot", "Api", "Builtin", bcName b], genBuiltinModule aenv b) | b <- Map.elems (aBuiltins aenv)]
          ++ [(["Godot", "Api", "Class", clName c], genClassModule aenv c) | c <- Map.elems (aClasses aenv)]
  changed <- fmap or . mapM id $
    [ do
        createDirectoryIfMissing True (takeDirectory (file comps))
        writeIfChanged (file comps) contents
    | (comps, contents) <- outputs
    ]
  when changed $
    notice verb ("godot-low: generated " ++ show (length outputs) ++ " Godot API modules (" ++ xVersion x ++ ")")
  -- Remove the modules of classes that are no longer generated (because
  -- godot-sys was built with other build options).
  let classDir = root </> "Godot" </> "Api" </> "Class"
      current = [file comps | (comps, _) <- outputs]
  old <- map (classDir </>) <$> listDirectory classDir
  mapM_ removeFile [f | f <- old, f `notElem` current]
  pure x

-- | The library component's autogen directory. Generated modules must not go
-- in the package-wide one, or other components (the test suite) would compile
-- their own copies instead of using the library.
libraryAutogenDir :: LocalBuildInfo -> IO FilePath
libraryAutogenDir lbi = case componentNameCLBIs lbi (CLibName LMainLibName) of
  clbi : _ -> pure (interpretSymbolicPathLBI lbi (autogenComponentModulesDir lbi clbi))
  [] -> fail "godot-low: no library component"

readExtApi :: FilePath -> IO ExtApi
readExtApi path =
  either (fail . (("godot-low: cannot parse " ++ path ++ ": ") ++)) pure =<< eitherDecodeFileStrict path

-- | godot-sys's @extension_api.json@, which describes a full Godot, with the
-- classes that the build options godot-sys built Godot with remove marked as
-- not in the engine. godot-sys installs, next to it, the arguments it ran
-- SCons with and which classes Godot's @disable_*@ build options remove.
readEngineApi :: Verbosity -> LocalBuildInfo -> IO ExtApi
readEngineApi verb lbi = do
  apiJson <- findGodotSysFile lbi "extension_api.json"
  info verb ("godot-low: generating Godot API bindings from " ++ apiJson)
  x <- readExtApi apiJson
  args <- lines <$> (readFile =<< findGodotSysFile lbi "scons-arguments.txt")
  optionsJson <- findGodotSysFile lbi "build_option_classes.json"
  options <- either (fail . (("godot-low: cannot parse " ++ optionsJson ++ ": ") ++)) pure =<< eitherDecodeFileStrict optionsJson
  let on = enabledOptions options args
      removed = [c | (c, sets) <- Map.toList (boClasses options), any (all (`elem` on)) sets]
  when (not (null on)) $
    info verb ("godot-low: leaving out the classes that " ++ unwords on ++ " remove")
  pure (withoutClasses removed x)

-- | Mark the given classes and their subclasses as not in the engine.
withoutClasses :: [String] -> ExtApi -> ExtApi
withoutClasses roots x = x {xClasses = [c {clInEngine = not (removed (clName c))} | c <- xClasses x]}
  where
    parents = Map.fromList [(clName c, clInherits c) | c <- xClasses x]
    removed n = n `elem` roots || maybe False removed (Map.lookup n parents >>= id)

-- | godot-sys's @build_option_classes.json@.
data BuildOptionClasses = BuildOptionClasses
  { boImplies :: Map.Map String [String]
  -- ^ build options that turn on others (@disable_3d@ turns on
  -- @disable_physics_3d@, ...)
  , boClasses :: Map.Map String [[String]]
  -- ^ for each class a build option can remove, the sets of options that
  -- remove it: it is removed when all the options of one set are on
  }

instance FromJSON BuildOptionClasses where
  parseJSON = withObject "build_option_classes" $ \o ->
    BuildOptionClasses <$> o .: "implies" <*> o .: "classes"

-- | The @disable_*@ build options that the given SCons arguments turn on,
-- with those they imply.
enabledOptions :: BuildOptionClasses -> [String] -> [String]
enabledOptions bo args = close [k | (k, v) <- Map.toList settings, sconsBool v == Just True]
  where
    -- As in SCons, a later argument overrides an earlier one.
    settings = Map.fromList [(k, drop 1 v) | a <- args, let (k, v) = break (== '=') a, not (null v)]
    sconsBool v
      | map toLower v `elem` ["y", "yes", "t", "true", "1", "on", "all"] = Just True
      | map toLower v `elem` ["n", "no", "f", "false", "0", "off", "none"] = Just False
      | otherwise = Nothing
    close os =
      let os' = nub (os ++ concatMap (\o -> Map.findWithDefault [] o (boImplies bo)) os)
       in if length os' == length os then os else close os'

-- | Set the generated Godot.Api modules of the library to those of the given
-- API (replacing any set before).
setApiModules :: ExtApi -> LocalBuildInfo -> LocalBuildInfo
setApiModules x lbi =
  let mods = map (fromString . intercalate ".") (apiModuleNames x)
      pkg = localPkgDescr lbi
      -- Those set before: the library's autogen modules under Godot.Api.
      -- (Godot.Api and Godot.Api.Internal are written by hand.)
      autogen = maybe [] (autogenModules . libBuildInfo) (library pkg)
      generated m = m `elem` autogen && "Godot.Api." `isPrefixOf` prettyShow m
      addTo l =
        l
          { exposedModules = filter (not . generated) (exposedModules l) ++ mods
          , libBuildInfo = (libBuildInfo l) {autogenModules = filter (not . generated) (autogenModules (libBuildInfo l)) ++ mods}
          }
      -- Registration uses the exposed modules recorded in the library's
      -- ComponentLocalBuildInfo, so update those too.
      addToClbi clbi = case clbi of
        LibComponentLocalBuildInfo {} ->
          clbi
            { componentExposedModules =
                filter (not . generated . exposedName) (componentExposedModules clbi) ++ [ExposedModule m Nothing | m <- mods]
            }
        _ -> clbi
   in lbi
      { localPkgDescr = pkg {library = fmap addTo (library pkg)}
      , componentGraph = Graph.fromDistinctList (map addToClbi (Graph.toList (componentGraph lbi)))
      , componentNameMap = Map.map (map addToClbi) (componentNameMap lbi)
      }

-- | Locate gdextension_interface.json in the include dirs of godot-sys.
findInterfaceJson :: LocalBuildInfo -> IO FilePath
findInterfaceJson lbi = findGodotSysFile lbi "gdextension_interface.json"

-- | Locate a file installed in the include dirs of godot-sys.
findGodotSysFile :: LocalBuildInfo -> FilePath -> IO FilePath
findGodotSysFile lbi name = do
  let pkgs = concatMap snd (lookupPackageName (installedPkgs lbi) (mkPackageName "godot-sys"))
      candidates = nub [d </> name | p <- pkgs, d <- IPI.includeDirs p]
  found <- filterM doesFileExist candidates
  case found of
    f : _ -> pure f
    [] ->
      fail $
        "godot-low: " ++ name ++ " not found in godot-sys include dirs: "
          ++ show candidates

writeIfChanged :: FilePath -> String -> IO Bool
writeIfChanged path contents = do
  exists <- doesFileExist path
  old <- if exists then Just <$> readFileStrict path else pure Nothing
  if old == Just contents
    then pure False
    else writeFile path contents >> pure True
  where
    readFileStrict p = do
      s <- readFile p
      length s `seq` pure s

--------------------------------------------------------------------------------
-- JSON model (see gdextension_interface.schema.json in the Godot source)

data Api = Api
  { apiTypes :: [TypeDef]
  , apiInterface :: [Fn]
  }

data TypeDef = TypeDef
  { tdName :: String
  , tdKind :: String
  , tdDescription :: [String]
  , tdDeprecated :: Maybe Deprecation
  , tdValues :: [EnumValue]
  , tdIsBitfield :: Bool
  , tdMembers :: [Member]
  , tdAliasOf :: Maybe String
  , tdArguments :: [Arg]
  , tdReturn :: Maybe Ret
  , tdParent :: Maybe String
  , tdIsConst :: Bool
  , tdIsUninitialized :: Bool
  }

data EnumValue = EnumValue {evName :: String, evValue :: Integer, evDescription :: [String]}

data Member = Member {mName :: String, mType :: String, mDescription :: [String]}

data Arg = Arg {argName :: Maybe String, argType :: String, argDescription :: [String]}

data Ret = Ret {retType :: String, retDescription :: [String]}

data Fn = Fn
  { fnName :: String
  , fnArgs :: [Arg]
  , fnReturn :: Maybe Ret
  , fnDescription :: [String]
  , fnSince :: Maybe String
  , fnDeprecated :: Maybe Deprecation
  , fnSee :: [String]
  , fnLegacyTypeName :: Maybe String
  }

data Deprecation = Deprecation
  { depSince :: Maybe String
  , depReplaceWith :: Maybe String
  , depMessage :: Maybe String
  }

instance FromJSON Api where
  parseJSON = withObject "Api" $ \o -> Api <$> o .: "types" <*> o .: "interface"

instance FromJSON TypeDef where
  parseJSON = withObject "TypeDef" $ \o ->
    TypeDef
      <$> o .: "name"
      <*> o .: "kind"
      <*> o .:? "description" .!= []
      <*> o .:? "deprecated"
      <*> o .:? "values" .!= []
      <*> o .:? "is_bitfield" .!= False
      <*> o .:? "members" .!= []
      <*> o .:? "type"
      <*> o .:? "arguments" .!= []
      <*> o .:? "return_value"
      <*> o .:? "parent"
      <*> o .:? "is_const" .!= False
      <*> o .:? "is_uninitialized" .!= False

instance FromJSON EnumValue where
  parseJSON = withObject "EnumValue" $ \o ->
    EnumValue <$> o .: "name" <*> o .: "value" <*> o .:? "description" .!= []

instance FromJSON Member where
  parseJSON = withObject "Member" $ \o ->
    Member <$> o .: "name" <*> o .: "type" <*> o .:? "description" .!= []

instance FromJSON Arg where
  parseJSON = withObject "Arg" $ \o ->
    Arg <$> o .:? "name" <*> o .: "type" <*> o .:? "description" .!= []

instance FromJSON Ret where
  parseJSON = withObject "Ret" $ \o ->
    Ret <$> o .: "type" <*> o .:? "description" .!= []

instance FromJSON Fn where
  parseJSON = withObject "Fn" $ \o ->
    Fn
      <$> o .: "name"
      <*> o .:? "arguments" .!= []
      <*> o .:? "return_value"
      <*> o .:? "description" .!= []
      <*> o .:? "since"
      <*> o .:? "deprecated"
      <*> o .:? "see" .!= []
      <*> o .:? "legacy_type_name"

instance FromJSON Deprecation where
  parseJSON = withObject "Deprecation" $ \o ->
    Deprecation <$> o .:? "since" <*> o .:? "replace_with" <*> o .:? "message"

--------------------------------------------------------------------------------
-- C type -> Haskell type

newtype Env = Env {envKinds :: Map.Map String String}

mkEnv :: Api -> Env
mkEnv api = Env (Map.fromList [(tdName t, tdKind t) | t <- apiTypes api])

primitives :: Map.Map String String
primitives =
  Map.fromList
    [ ("void", "()")
    , ("bool", "CBool")
    , ("char", "CChar")
    , ("int", "CInt")
    , ("float", "CFloat")
    , ("double", "CDouble")
    , ("size_t", "CSize")
    , ("wchar_t", "CWchar")
    , ("char16_t", "Word16")
    , ("char32_t", "Word32")
    , ("int8_t", "Int8")
    , ("int16_t", "Int16")
    , ("int32_t", "Int32")
    , ("int64_t", "Int64")
    , ("uint8_t", "Word8")
    , ("uint16_t", "Word16")
    , ("uint32_t", "Word32")
    , ("uint64_t", "Word64")
    ]

hsType :: Env -> String -> String
hsType env = go . trim
  where
    go t
      | "const " `isPrefixOf` t = go (trim (drop 6 t))
      | "*" `isSuffixOf` t = "Ptr " ++ paren (go (trim (init t)))
      | Just h <- Map.lookup t primitives = h
      | Map.member t (envKinds env) = t
      | otherwise = error ("godot-low: unknown C type " ++ show t)

-- | Type used in a foreign import: structs must not be passed by value.
ffiType :: Env -> String -> String
ffiType env t
  | Map.lookup (trim t) (envKinds env) == Just "struct" =
      error ("godot-low: struct passed by value is not supported: " ++ t)
  | otherwise = hsType env t

fnSig :: Env -> [Arg] -> Maybe Ret -> String
fnSig env args ret =
  intercalate " -> " $
    map (ffiType env . argType) args
      ++ ["IO " ++ paren (maybe "()" (ffiType env . retType) ret)]

paren :: String -> String
paren s = if ' ' `elem` s then "(" ++ s ++ ")" else s

trim :: String -> String
trim = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')

--------------------------------------------------------------------------------
-- Naming

-- | GDExtensionCallError -> gdExtensionCallError
lowerFirst :: String -> String
lowerFirst s = case span isUpper s of
  (ups, rest@(_ : _)) | length ups > 1 -> map toLower (init ups) ++ [last ups] ++ rest
  (ups, rest) -> map toLower ups ++ rest

-- | get_godot_version2 -> GetGodotVersion2
pascal :: String -> String
pascal = concatMap cap . splitOn '_'
  where
    cap (c : cs) = toUpper c : cs
    cap [] = []

splitOn :: Char -> String -> [String]
splitOn c s = case break (== c) s of
  (a, _ : rest) -> a : splitOn c rest
  (a, []) -> [a]

fieldName :: TypeDef -> Member -> String
fieldName t m = lowerFirst (tdName t) ++ "_" ++ mName m

interfaceTypeName :: Fn -> String
interfaceTypeName f = fromMaybe ("GDExtensionInterface" ++ pascal (fnName f)) (fnLegacyTypeName f)

haskellKeywords :: [String]
haskellKeywords =
  [ "case", "class", "data", "default", "deriving", "do", "else", "foreign", "if", "import"
  , "in", "infix", "infixl", "infixr", "instance", "let", "module", "newtype", "of", "then"
  , "type", "where", "pattern", "forall"
  ]

interfaceFieldName :: Fn -> String
interfaceFieldName f
  | fnName f `elem` haskellKeywords = fnName f ++ "_"
  | otherwise = fnName f

--------------------------------------------------------------------------------
-- Documentation

-- | Escape text for an .hsc file: hsc2hs treats @#@ specially everywhere.
type Escape = String -> String

hscEscape :: Escape
hscEscape = concatMap (\c -> if c == '#' then "##" else [c])

noEscape :: Escape
noEscape = id

-- | Haddock comment lines (indented by @ind@).
docLines :: Escape -> String -> [String] -> [String]
docLines _ _ [] = []
docLines esc ind (l : ls) =
  (ind ++ "-- | " ++ fmt l) : [ind ++ (if null x then "--" else "-- " ++ fmt x) | x <- ls]
  where
    fmt = esc . backticksToCode

-- | Markdown @`code`@ spans (used in Godot's descriptions) to Haddock @\@code\@@.
backticksToCode :: String -> String
backticksToCode s = case break (== '`') s of
  (pre, '`' : rest) | (code, '`' : post) <- break (== '`') rest ->
    pre ++ "@" ++ concatMap escAt code ++ "@" ++ backticksToCode post
  _ -> s
  where
    escAt '@' = "\\@"
    escAt c = [c]

deprecationText :: Deprecation -> String
deprecationText d =
  unwords . filter (not . null) $
    [ maybe "Deprecated." (\v -> "Deprecated since Godot " ++ v ++ ".") (depSince d)
    , maybe "" (\r -> "Use " ++ r ++ " instead.") (depReplaceWith d)
    , fromMaybe "" (depMessage d)
    ]

deprecatedPragma :: Escape -> String -> Maybe Deprecation -> [String]
deprecatedPragma _ _ Nothing = []
deprecatedPragma esc name (Just d) = ["{-# DEPRECATED " ++ name ++ " " ++ esc (show (deprecationText d)) ++ " #-}"]

fnDoc :: Fn -> [String]
fnDoc f =
  fnDescription f
    ++ concat [["", "Arguments:", ""] | not (null (fnArgs f))]
    ++ [ "* @" ++ fromMaybe "_" (argName a) ++ "@ (@" ++ argType a ++ "@)"
           ++ (if null (argDescription a) then "" else ": " ++ unwords (argDescription a))
       | a <- fnArgs f
       ]
    ++ concat
      [ ["", "Returns (@" ++ retType r ++ "@)" ++ (if null (retDescription r) then "." else ": " ++ unwords (retDescription r))]
      | Just r <- [fnReturn f]
      ]
    ++ concat [["", "See: " ++ intercalate ", " (fnSee f)] | not (null (fnSee f))]
    ++ concat [["", deprecationText d] | Just d <- [fnDeprecated f]]
    ++ concat [["", "@since " ++ s] | Just s <- [fnSince f]]

--------------------------------------------------------------------------------
-- Godot.GDExtension.Types

genTypes :: Env -> Api -> String
genTypes env api =
  unlines $
    [ "{-# LANGUAGE DerivingStrategies #-}"
    , "{-# LANGUAGE ForeignFunctionInterface #-}"
    , "{-# LANGUAGE GeneralizedNewtypeDeriving #-}"
    , "{-# LANGUAGE PatternSynonyms #-}"
    , "{-# OPTIONS_GHC -Wno-deprecations -Wno-unused-imports #-}"
    , ""
    , "-- | GDExtension types, generated by godot-low's Setup.hs from"
    , "-- @gdextension_interface.json@. Do not edit."
    , "--"
    , "-- * Handles (@GDExtension*Ptr@) are newtypes over @Ptr ()@. Const and"
    , "--   uninitialized variants are separate newtypes; convert with"
    , "--   'Data.Coerce.coerce'."
    , "-- * Enums are newtypes over the C enum's integer type, with pattern synonyms"
    , "--   for the values."
    , "-- * Structs are records with 'Storable' instances (layout from the C header)."
    , "-- * Function pointer types are 'FunPtr' synonyms, with @wrapX@ (Haskell"
    , "--   function to C function pointer) and @unwrapX@ (call a C function pointer)."
    , "module Godot.GDExtension.Types where"
    , ""
    , "#include <libgodot.h>"
    , ""
    , "import Data.Bits (Bits)"
    , "import Data.Int"
    , "import Data.Word"
    , "import Foreign.C.Types"
    , "import Foreign.Ptr"
    , "import Foreign.Storable"
    , ""
    ]
      ++ section "Aliases" (concatMap genAlias (ofKind "alias"))
      ++ section "Handles" (concatMap genHandle (ofKind "handle"))
      ++ section "Enums" (concatMap genCEnum (ofKind "enum"))
      ++ section "Structs" (concatMap genStruct (ofKind "struct"))
      ++ section "Function pointer types" (concatMap genFunctionType (ofKind "function"))
  where
    ofKind k = [t | t <- apiTypes api, tdKind t == k]
    section title body = ["-" ++ replicate 79 '-', "-- " ++ title, ""] ++ body
    esc = hscEscape
    docs t = docLines esc "" (tdDescription t)
    depr t = deprecatedPragma esc (tdName t) (tdDeprecated t)

    genAlias t =
      docs t
        ++ ["type " ++ tdName t ++ " = " ++ hsType env (fromMaybe (error "alias without type") (tdAliasOf t))]
        ++ depr t
        ++ [""]

    genHandle t =
      docLines esc "" (tdDescription t ++ handleNote t)
        ++ [ "newtype " ++ tdName t ++ " = " ++ tdName t ++ " (Ptr ())"
           , "  deriving stock (Show)"
           , "  deriving newtype (Eq, Ord, Storable)"
           ]
        ++ depr t
        ++ [""]

    handleNote t = case tdParent t of
      Nothing -> []
      Just p ->
        ["" | not (null (tdDescription t))]
          ++ [ (if tdIsConst t then "Const variant" else if tdIsUninitialized t then "Uninitialized variant" else "Variant")
                 ++ " of '" ++ p ++ "'."
             ]

    genCEnum t =
      let n = tdName t
       in docs t
            ++ [ "newtype " ++ n ++ " = " ++ n ++ " #{type " ++ n ++ "}"
               , "  deriving stock (Show)"
               , "  deriving newtype (Eq, Ord, Num, Storable" ++ (if tdIsBitfield t then ", Bits)" else ")")
               ]
            ++ depr t
            ++ [""]
            ++ concat
              [ docLines esc "" (evDescription v)
                  ++ [ "pattern " ++ evName v ++ " :: " ++ n
                     , "pattern " ++ evName v ++ " = " ++ n ++ " " ++ showLit (evValue v)
                     , ""
                     ]
              | v <- tdValues t
              ]

    showLit i = if i < 0 then "(" ++ show i ++ ")" else show i

    genStruct t =
      let n = tdName t
          ms = tdMembers t
          field m = fieldName t m
       in docs t
            ++ ["data " ++ n ++ " = " ++ n]
            ++ concat
              [ docLines esc "    " (mDescription m)
                  ++ ["  " ++ (if i == 0 then "{ " else ", ") ++ field m ++ " :: !" ++ paren (hsType env (mType m))]
              | (i, m) <- zip [0 :: Int ..] ms
              ]
            ++ ["  }" | not (null ms)]
            ++ [ "  deriving stock (Eq, Show)"
               , ""
               , "instance Storable " ++ n ++ " where"
               , "  sizeOf _ = #{size " ++ n ++ "}"
               , "  alignment _ = #{alignment " ++ n ++ "}"
               , "  peek p ="
               , "    pure " ++ n
               ]
            ++ ["      <*> #{peek " ++ n ++ ", " ++ mName m ++ "} p" | m <- ms]
            ++ ["  poke p v = do"]
            ++ ["    #{poke " ++ n ++ ", " ++ mName m ++ "} p (" ++ field m ++ " v)" | m <- ms]
            ++ ["    pure ()"]
            ++ depr t
            ++ [""]

    genFunctionType t =
      let n = tdName t
          sig = fnSig env (tdArguments t) (tdReturn t)
       in docLines esc "" (tdDescription t ++ argDocs (tdArguments t))
            ++ [ "type " ++ n ++ " = FunPtr (" ++ sig ++ ")"
               ]
            ++ depr t
            ++ [ ""
               , "-- | Turn a Haskell function into a '" ++ n ++ "'. Free it with 'freeHaskellFunPtr'."
               , "foreign import ccall \"wrapper\""
               , "  wrap" ++ n ++ " :: (" ++ sig ++ ") -> IO " ++ n
               , ""
               , "-- | Call a '" ++ n ++ "'."
               , "foreign import ccall \"dynamic\""
               , "  unwrap" ++ n ++ " :: " ++ n ++ " -> " ++ sig
               , ""
               ]

    argDocs args =
      concat [["", "Arguments: " ++ intercalate ", " (map argLabel args)] | any (/= Nothing) (map argName args)]
    argLabel a = "@" ++ fromMaybe "_" (argName a) ++ " :: " ++ argType a ++ "@"

--------------------------------------------------------------------------------
-- Godot.GDExtension.Interface

genInterface :: Env -> Api -> String
genInterface env api =
  unlines $
    [ "{-# LANGUAGE ForeignFunctionInterface #-}"
    , "{-# OPTIONS_GHC -Wno-deprecations #-}"
    , ""
    , "-- | The GDExtension interface functions, generated by godot-low's Setup.hs"
    , "-- from @gdextension_interface.json@. Do not edit."
    , "--"
    , "-- Godot hands a 'GDExtensionInterfaceGetProcAddress' to the extension's"
    , "-- initialization function; 'loadInterface' resolves every function through"
    , "-- it into a 'GDExtensionInterface' record."
    , "module Godot.GDExtension.Interface"
    , "  ( GDExtensionInterface (..)"
    , "  , loadInterface"
    , "  , getProcAddress"
    , "    -- * Interface function pointer types"
    ]
      ++ concat [["  , " ++ interfaceTypeName f, "  , unwrap" ++ interfaceTypeName f] | f <- fns]
      ++ [ "  ) where"
         , ""
         , "import Foreign.C.String (withCString)"
         , "import Foreign.C.Types"
         , "import Data.Int"
         , "import Data.Word"
         , "import Foreign.Ptr"
         , "import Godot.GDExtension.Types"
         , ""
         , "-- | All GDExtension interface functions."
         , "data GDExtensionInterface = GDExtensionInterface"
         ]
      ++ concat
        [ docLines noEscape "    " (fnDoc f)
            ++ ["  " ++ (if i == 0 then "{ " else ", ") ++ interfaceFieldName f ++ " :: " ++ fnSig env (fnArgs f) (fnReturn f)]
        | (i, f) <- zip [0 :: Int ..] fns
        ]
      ++ ["  }", ""]
      ++ concatMap (\f -> deprecatedPragma noEscape (interfaceFieldName f) (fnDeprecated f)) fns
      ++ [ ""
         , "-- | Look up one interface function by its GDExtension name."
         , "getProcAddress :: GDExtensionInterfaceGetProcAddress -> String -> IO GDExtensionInterfaceFunctionPtr"
         , "getProcAddress gpa name = withCString name (unwrapGDExtensionInterfaceGetProcAddress gpa)"
         , ""
         , "-- | Resolve every interface function through Godot's @get_proc_address@."
         , "-- Functions this Godot build does not provide raise an error when called."
         , "loadInterface :: GDExtensionInterfaceGetProcAddress -> IO GDExtensionInterface"
         , "loadInterface gpa = do"
         ]
      ++ ["  p" ++ show i ++ " <- getProcAddress gpa " ++ show (fnName f) | (i, f) <- zip [0 :: Int ..] fns]
      ++ ["  pure GDExtensionInterface"]
      ++ [ "    " ++ (if i == 0 then "{ " else ", ") ++ interfaceFieldName f ++ " = resolve "
             ++ show (fnName f) ++ " unwrap" ++ interfaceTypeName f ++ " p" ++ show i
         | (i, f) <- zip [0 :: Int ..] fns
         ]
      ++ [ "    }"
         , ""
         , "resolve :: String -> (FunPtr a -> b) -> GDExtensionInterfaceFunctionPtr -> b"
         , "resolve name unwrap p"
         , "  | p == nullFunPtr = error (\"Godot.GDExtension.Interface: \" ++ name ++ \" is not provided by this Godot build\")"
         , "  | otherwise = unwrap (castFunPtr p)"
         , ""
         ]
      ++ concatMap genInterfaceType fns
  where
    fns = apiInterface api
    genInterfaceType f =
      let n = interfaceTypeName f
          sig = fnSig env (fnArgs f) (fnReturn f)
       in [ "-- | Function pointer type of '" ++ interfaceFieldName f ++ "'."
          , "type " ++ n ++ " = FunPtr (" ++ sig ++ ")"
          , ""
          , "foreign import ccall \"dynamic\""
          , "  unwrap" ++ n ++ " :: " ++ n ++ " -> " ++ sig
          , ""
          ]

--------------------------------------------------------------------------------
-- extension_api.json model (the Godot class API)

data ExtApi = ExtApi
  { xVersion :: String
  , xSizes :: Map.Map String Int
  , xOffsets :: Map.Map String [MemberOffset]
  , xGlobalEnums :: [ApiEnum]
  , xUtility :: [ApiFn]
  , xBuiltins :: [BuiltinClass]
  , xClasses :: [ApiClass]
  , xSingletons :: [String]
  -- ^ class names of the engine singletons
  }

data MemberOffset = MemberOffset {moName :: String, moOffset :: Int, moMeta :: String}

data ApiEnum = ApiEnum
  { aeName :: String
  , aeIsBitfield :: Bool
  , aeValues :: [ApiValue]
  }

data ApiValue = ApiValue {avName :: String, avValue :: Integer, avDesc :: Maybe String}

data ApiArg = ApiArg {aaName :: String, aaType :: String}

data ApiFn = ApiFn
  { afName :: String
  , afReturn :: Maybe String
  , afVararg :: Bool
  , afStatic :: Bool
  , afVirtual :: Bool
  , afHash :: Maybe Integer
  , afArgs :: [ApiArg]
  , afDesc :: Maybe String
  }

data ApiOperator = ApiOperator
  { aoName :: String
  , aoRight :: Maybe String
  , aoReturn :: String
  , aoDesc :: Maybe String
  }

data ApiCtor = ApiCtor {acIndex :: Integer, acArgs :: [ApiArg], acDesc :: Maybe String}

data BuiltinClass = BuiltinClass
  { bcName :: String
  , bcEnums :: [ApiEnum]
  , bcOperators :: [ApiOperator]
  , bcMethods :: [ApiFn]
  , bcCtors :: [ApiCtor]
  , bcHasDestructor :: Bool
  , bcBrief :: Maybe String
  , bcDesc :: Maybe String
  }

data ApiClass = ApiClass
  { clName :: String
  , clInherits :: Maybe String
  , clApiType :: String
  , clRefcounted :: Bool
  , clInstantiable :: Bool
  , clConstants :: [ApiValue]
  , clEnums :: [ApiEnum]
  , clMethods :: [ApiFn]
  , clBrief :: Maybe String
  , clDesc :: Maybe String
  , clInEngine :: Bool
  -- ^ False for the classes that godot-sys's build options remove (see
  -- 'readEngineApi')
  }

-- | The build configuration godot-sys builds: single precision, 64-bit.
buildConfiguration :: String
buildConfiguration = "float_64"

instance FromJSON ExtApi where
  parseJSON = withObject "extension_api" $ \o -> do
    header <- o .: "header"
    version <- header .: "version_full_name"
    sizes <- forConfig "builtin_class_sizes" o $ \c -> do
      xs <- c .: "sizes"
      Map.fromList <$> mapM (\x -> (,) <$> x .: "name" <*> x .: "size") xs
    offsets <- forConfig "builtin_class_member_offsets" o $ \c -> do
      xs <- c .: "classes"
      Map.fromList <$> mapM (\x -> (,) <$> x .: "name" <*> x .: "members") xs
    ExtApi version sizes offsets
      <$> o .: "global_enums"
      <*> o .: "utility_functions"
      <*> o .: "builtin_classes"
      <*> o .: "classes"
      <*> (o .: "singletons" >>= mapM (.: "type"))
    where
      forConfig :: Key -> Object -> (Object -> Parser a) -> Parser a
      forConfig key o f = do
        cfgs <- o .: key
        matches <- filterM (\c -> (== buildConfiguration) <$> c .: "build_configuration") cfgs
        case matches of
          c : _ -> f c
          [] -> fail ("no " ++ buildConfiguration ++ " configuration in " ++ show key)

instance FromJSON MemberOffset where
  parseJSON = withObject "MemberOffset" $ \o ->
    MemberOffset <$> o .: "member" <*> o .: "offset" <*> o .: "meta"

instance FromJSON ApiEnum where
  parseJSON = withObject "ApiEnum" $ \o ->
    ApiEnum <$> o .: "name" <*> o .:? "is_bitfield" .!= False <*> o .:? "values" .!= []

instance FromJSON ApiValue where
  parseJSON = withObject "ApiValue" $ \o ->
    ApiValue <$> o .: "name" <*> o .: "value" <*> o .:? "description"

instance FromJSON ApiArg where
  parseJSON = withObject "ApiArg" $ \o -> ApiArg <$> o .: "name" <*> o .: "type"

instance FromJSON ApiFn where
  parseJSON = withObject "ApiFn" $ \o -> do
    rt <- o .:? "return_type"
    rv <- o .:? "return_value"
    rvt <- maybe (pure Nothing) (.:? "type") rv
    ApiFn
      <$> o .: "name"
      <*> pure (maybe rvt Just rt)
      <*> o .:? "is_vararg" .!= False
      <*> o .:? "is_static" .!= False
      <*> o .:? "is_virtual" .!= False
      <*> o .:? "hash"
      <*> o .:? "arguments" .!= []
      <*> o .:? "description"

instance FromJSON ApiOperator where
  parseJSON = withObject "ApiOperator" $ \o ->
    ApiOperator <$> o .: "name" <*> o .:? "right_type" <*> o .: "return_type" <*> o .:? "description"

instance FromJSON ApiCtor where
  parseJSON = withObject "ApiCtor" $ \o ->
    ApiCtor <$> o .: "index" <*> o .:? "arguments" .!= [] <*> o .:? "description"

instance FromJSON BuiltinClass where
  parseJSON = withObject "BuiltinClass" $ \o ->
    BuiltinClass
      <$> o .: "name"
      <*> o .:? "enums" .!= []
      <*> o .:? "operators" .!= []
      <*> o .:? "methods" .!= []
      <*> o .:? "constructors" .!= []
      <*> o .:? "has_destructor" .!= False
      <*> o .:? "brief_description"
      <*> o .:? "description"

instance FromJSON ApiClass where
  parseJSON = withObject "ApiClass" $ \o ->
    ApiClass
      <$> o .: "name"
      <*> o .:? "inherits"
      <*> o .:? "api_type" .!= "core"
      <*> o .:? "is_refcounted" .!= False
      <*> o .:? "is_instantiable" .!= False
      <*> o .:? "constants" .!= []
      <*> o .:? "enums" .!= []
      <*> o .:? "methods" .!= []
      <*> o .:? "brief_description"
      <*> o .:? "description"
      <*> pure True

--------------------------------------------------------------------------------
-- Godot.Api: environment and naming

-- | Builtins that map to Haskell primitives rather than generated types.
primitiveBuiltins :: [String]
primitiveBuiltins = ["Nil", "bool", "int", "float"]

data AEnv = AEnv
  { aBuiltins :: Map.Map String BuiltinClass
  , aClasses :: Map.Map String ApiClass
  , aAllClassNames :: [String]
  , aGlobalEnums :: Map.Map String String
  -- ^ JSON name -> Haskell type name
  , aClassEnums :: [(String, String)]
  , aBuiltinEnums :: [(String, String)]
  , aVariantTypes :: Map.Map String Integer
  -- ^ normalized type name -> GDExtensionVariantType value
  , aOperators :: Map.Map String Integer
  -- ^ OP_ name -> GDExtensionVariantOperator value
  , aSizes :: Map.Map String Int
  , aOffsets :: Map.Map String [MemberOffset]
  , aSingletons :: [String]
  }

-- | Only the classes available in the engine godot-sys builds (no editor,
-- and none that its build options remove).
coreClasses :: ExtApi -> [ApiClass]
coreClasses x = [c | c <- xClasses x, clApiType c == "core", clInEngine c]

mkAEnv :: ExtApi -> AEnv
mkAEnv x =
  AEnv
    { aBuiltins = Map.fromList [(bcName b, b) | b <- xBuiltins x, bcName b `notElem` primitiveBuiltins]
    , aClasses = Map.fromList [(clName c, c) | c <- coreClasses x]
    , aAllClassNames = map clName (xClasses x)
    , aGlobalEnums = Map.fromList [(aeName e, globalEnumTypeName x e) | e <- xGlobalEnums x]
    , aClassEnums = [(clName c, aeName e) | c <- coreClasses x, e <- clEnums c]
    , aBuiltinEnums = [(bcName b, aeName e) | b <- xBuiltins x, e <- bcEnums b]
    , aVariantTypes = enumTable "Variant.Type" "TYPE_"
    , aOperators = enumTable "Variant.Operator" ""
    , aSizes = xSizes x
    , aOffsets = xOffsets x
    , aSingletons = xSingletons x
    }
  where
    enumTable :: String -> String -> Map.Map String Integer
    enumTable name prefix =
      Map.fromList
        [ (normalizeTypeName (drop (length prefix) (avName v)), avValue v)
        | e <- xGlobalEnums x
        , aeName e == name
        , v <- aeValues e
        ]

normalizeTypeName :: String -> String
normalizeTypeName = map toUpper . filter (/= '_')

-- | Global enum type names, avoiding clashes with class and builtin names.
globalEnumTypeName :: ExtApi -> ApiEnum -> String
globalEnumTypeName x e
  | n `elem` taken = "Global_" ++ n
  | otherwise = n
  where
    n = map (\c -> if c == '.' then '_' else c) (aeName e)
    taken = map clName (xClasses x) ++ map (hsBuiltinName . bcName) (xBuiltins x)

hsBuiltinName :: String -> String
hsBuiltinName "String" = "GString"
hsBuiltinName n = n

-- | snake_case -> camelCase, keeping a leading underscore.
camel :: String -> String
camel ('_' : rest) = '_' : camel rest
camel s = case splitOn '_' s of
  [] -> s
  (w : ws) -> w ++ concatMap cap ws
  where
    cap (c : cs) = toUpper c : cs
    cap [] = "_"

-- | A valid, unused value-level name.
safeName :: [String] -> String -> String
safeName used n0 = go base
  where
    go n = if n `elem` used then go (n ++ "'") else n
    base = if n0 `elem` haskellKeywords then n0 ++ "'" else n0

variantTypeExpr :: AEnv -> String -> String
variantTypeExpr env t
  | t == "Variant" = "(G.GDExtensionVariantType 0)"
  | otherwise = case Map.lookup (normalizeTypeName t) (aVariantTypes env) of
      Just v -> "(G.GDExtensionVariantType " ++ show v ++ ")"
      Nothing -> error ("godot-low: no variant type for " ++ t)

operatorNames :: Map.Map String String
operatorNames =
  Map.fromList
    [ ("==", "EQUAL"), ("!=", "NOT_EQUAL"), ("<", "LESS"), ("<=", "LESS_EQUAL")
    , (">", "GREATER"), (">=", "GREATER_EQUAL"), ("+", "ADD"), ("-", "SUBTRACT")
    , ("*", "MULTIPLY"), ("/", "DIVIDE"), ("unary-", "NEGATE"), ("unary+", "POSITIVE")
    , ("%", "MODULE"), ("**", "POWER"), ("<<", "SHIFT_LEFT"), (">>", "SHIFT_RIGHT")
    , ("&", "BIT_AND"), ("|", "BIT_OR"), ("^", "BIT_XOR"), ("~", "BIT_NEGATE")
    , ("and", "AND"), ("or", "OR"), ("xor", "XOR"), ("not", "NOT"), ("in", "IN")
    ]

-- | Haskell type for an API type. Class results are 'Maybe' (null objects).
apiType :: AEnv -> Bool -> String -> String
apiType env isRet t
  | '*' `elem` t = "(Ptr ())"
  | t `elem` ["Nil", "void"] = "()"
  | t == "bool" = "P.Bool"
  | t == "int" = "Int64"
  | t == "float" = "P.Double"
  | t == "Variant" = "R.Variant"
  | Just r <- stripPrefix "enum::" t = enumRef r
  | Just r <- stripPrefix "bitfield::" t = enumRef r
  | "typedarray::" `isPrefixOf` t = "B.Array"
  | "typeddictionary::" `isPrefixOf` t = "B.Dictionary"
  | Map.member t (aBuiltins env) = "B." ++ hsBuiltinName t
  | Map.member t (aClasses env) = cls t
  | t `elem` aAllClassNames env = cls "Object"
  | otherwise = error ("godot-low: unknown API type " ++ show t)
  where
    cls c = if isRet then "(P.Maybe C." ++ c ++ ")" else "C." ++ c
    enumRef r
      | Just n <- Map.lookup r (aGlobalEnums env) = "E." ++ n
      | (owner, name) <- splitLastDot r =
          if (owner, name) `elem` aClassEnums env
            then "C." ++ owner ++ "_" ++ name
            else
              if (owner, name) `elem` aBuiltinEnums env
                then "B." ++ hsBuiltinName owner ++ "_" ++ name
                else "Int64"

splitLastDot :: String -> (String, String)
splitLastDot s = case break (== '.') (reverse s) of
  (rname, '.' : rowner) -> (reverse rowner, reverse rname)
  _ -> ("", s)

stripPrefix :: String -> String -> Maybe String
stripPrefix p s = if p `isPrefixOf` s then Just (drop (length p) s) else Nothing

-- | Module names generated from extension_api.json.
apiModuleNames :: ExtApi -> [[String]]
apiModuleNames x =
  [ ["Godot", "Api", "Builtin", "Types"]
  , ["Godot", "Api", "Classes"]
  , ["Godot", "Api", "GlobalEnums"]
  , ["Godot", "Api", "Utility"]
  ]
    ++ [["Godot", "Api", "Builtin", bcName b] | b <- xBuiltins x, bcName b `notElem` primitiveBuiltins]
    ++ [["Godot", "Api", "Class", clName c] | c <- coreClasses x]

--------------------------------------------------------------------------------
-- BBCode documentation -> Haddock

-- | Haddock comment lines for a Godot BBCode description.
bbDoc :: [Maybe String] -> [String]
bbDoc parts = case concatMap bbBlocks [p | Just p <- parts, not (all isSpace p)] of
  [] -> []
  ls -> renderDoc (intercalateBlank ls)
  where
    intercalateBlank = intercalate [""]

renderDoc :: [String] -> [String]
renderDoc [] = []
renderDoc (l : ls) = ("-- | " ++ l) : [if null x then "--" else "-- " ++ x | x <- ls]

-- | Split into blocks (paragraphs and code blocks), each a list of lines.
bbBlocks :: String -> [[String]]
bbBlocks s = case breakOnTag ["[codeblocks]", "[codeblock]"] s of
  Nothing -> paragraphs s
  Just (before, tag, rest) ->
    let close = if tag == "[codeblocks]" then "[/codeblocks]" else "[/codeblock]"
        (code, after) = breakOnStr close rest
        code' = case breakOnTag ["[gdscript]"] code of
          Just (_, _, g) -> fst (breakOnStr "[/gdscript]" g)
          Nothing -> code
     in paragraphs before ++ [codeLines code'] ++ bbBlocks (drop (length close) after)
  where
    paragraphs t = [[bbInline p] | p <- lines t, not (all isSpace p)]
    codeLines c = ["> " ++ map (\ch -> if ch == '\t' then ' ' else ch) l | l <- trimBlank (lines c)]
    trimBlank = reverse . dropWhile (all isSpace) . reverse . dropWhile (all isSpace)

breakOnStr :: String -> String -> (String, String)
breakOnStr pat = go []
  where
    go acc [] = (reverse acc, [])
    go acc s@(c : cs)
      | pat `isPrefixOf` s = (reverse acc, s)
      | otherwise = go (c : acc) cs

breakOnTag :: [String] -> String -> Maybe (String, String, String)
breakOnTag tags = go []
  where
    go _ [] = Nothing
    go acc s@(c : cs) = case [t | t <- tags, t `isPrefixOf` s] of
      t : _ -> Just (reverse acc, t, drop (length t) s)
      [] -> go (c : acc) cs

-- | Inline BBCode to Haddock markup.
bbInline :: String -> String
bbInline [] = []
bbInline ('[' : rest)
  | (tag, ']' : after) <- break (== ']') rest
  , not (null tag)
  , '[' `notElem` tag =
      case words tag of
        ["code"] -> let (c, a) = breakOnStr "[/code]" after in "@" ++ escapeHaddock c ++ "@" ++ bbInline (drop 7 a)
        ["b"] -> "__" ++ bbInline after
        ["/b"] -> "__" ++ bbInline after
        ["i"] -> "/" ++ bbInline after
        ["/i"] -> "/" ++ bbInline after
        ["kbd"] -> "@" ++ bbInline after
        ["/kbd"] -> "@" ++ bbInline after
        ["br"] -> " " ++ bbInline after
        ["lb"] -> "\\[" ++ bbInline after
        ["rb"] -> "\\]" ++ bbInline after
        [kind, ref]
          | kind `elem` refKinds -> "@" ++ escapeHaddock ref ++ "@" ++ bbInline after
        [w]
          | all (\ch -> isAlphaNum ch || ch `elem` ("@._" :: String)) w && startsUpper w ->
              "@" ++ escapeHaddock w ++ "@" ++ bbInline after
          | any (`isPrefixOf` w) dropped -> bbInline after
        _
          | any (`isPrefixOf` tag) dropped -> bbInline after
          | otherwise -> "\\[" ++ escapeHaddock tag ++ "\\]" ++ bbInline after
  where
    refKinds =
      ["param", "method", "member", "signal", "constant", "enum", "annotation", "theme_item", "constructor", "operator"]
    dropped = ["url", "/url", "u", "/u", "s", "/s", "center", "/center", "color", "/color", "font", "/font"]
bbInline (c : rest) = escapeHaddock [c] ++ bbInline rest

startsUpper :: String -> Bool
startsUpper (ch : _) = isUpper ch
startsUpper [] = False

escapeHaddock :: String -> String
escapeHaddock = concatMap esc
  where
    esc '\t' = " "
    esc c
      | c `elem` ("\\/'`\"@<>*_#$[]-" :: String) = ['\\', c]
      | otherwise = [c]

--------------------------------------------------------------------------------
-- Godot.Api: shared code fragments

apiModuleHeader :: [String] -> [String] -> String -> [String]
apiModuleHeader pragmas docLines' modName =
  ["{-# LANGUAGE " ++ p ++ " #-}" | p <- pragmas]
    ++ ["{-# OPTIONS_GHC -Wno-unused-imports -Wno-deprecations -Wno-orphans #-}", ""]
    ++ docLines'
    ++ ["module " ++ modName]

apiImports :: [String]
apiImports =
  [ "import qualified Prelude as P"
  , "import Data.Int (Int32, Int64)"
  , "import Foreign.Ptr (Ptr)"
  , "import qualified Foreign.Ptr as F"
  , "import qualified Godot.GDExtension.Types as G"
  , "import qualified Godot.Api.Internal as R"
  ]

generatedNote :: String
generatedNote = "Generated by godot-low's Setup.hs from @extension_api.json@. Do not edit."

-- | Wrap @body@ (which uses @a0..an@ and @ret@) in argument/return marshalling.
marshalCall :: [String] -> String -> [String]
marshalCall argVars body =
  ["  R.withArg " ++ v ++ " P.$ \\a" ++ show i ++ " ->" | (i, v) <- zip [0 :: Int ..] argVars]
    ++ ["  R.withRet P.$ \\ret ->", "  " ++ body]

argList :: Int -> String
argList n = "[" ++ intercalate ", " ["a" ++ show i | i <- [0 .. n - 1]] ++ "]"

argsDoc :: [ApiArg] -> Maybe String -> [Maybe String]
argsDoc args ret =
  [ Just ("Arguments: " ++ intercalate ", " ["[code]" ++ aaName a ++ ": " ++ aaType a ++ "[/code]" | a <- args])
  | not (null args)
  ]
    ++ [Just ("Returns [code]" ++ r ++ "[/code].") | Just r <- [ret]]

fnSignature :: AEnv -> [String] -> [ApiArg] -> Maybe String -> String
fnSignature env pre args ret =
  intercalate " -> " (pre ++ map (apiType env False . aaType) args ++ ["P.IO " ++ apiType env True (fromMaybe "void" ret)])

--------------------------------------------------------------------------------
-- Godot.Api.GlobalEnums, enums in general

-- | An enum: newtype over Int64 (how Godot passes enums) with patterns.
genEnum :: String -> String -> ApiEnum -> [String]
genEnum typeName patPrefix e =
  [ "newtype " ++ typeName ++ " = " ++ typeName ++ " Int64"
  , "  deriving newtype (P.Eq, P.Ord, P.Show, P.Num, P.Enum, P.Real, P.Integral, S.Storable, R.PtrArg, R.PtrRet, R.ToVariant, R.FromVariant"
      ++ (if aeIsBitfield e then ", Bits.Bits)" else ")")
  , ""
  ]
    ++ concat
      [ bbDoc [avDesc v]
          ++ [ "pattern " ++ patPrefix ++ avName v ++ " :: " ++ typeName
             , "pattern " ++ patPrefix ++ avName v ++ " = " ++ typeName ++ " " ++ lit (avValue v)
             , ""
             ]
      | v <- nubOn avName (aeValues e)
      ]
  where
    lit i = if i < 0 then "(" ++ show i ++ ")" else show i

nubOn :: Eq b => (a -> b) -> [a] -> [a]
nubOn f = go []
  where
    go _ [] = []
    go seen (y : ys)
      | f y `elem` seen = go seen ys
      | otherwise = y : go (f y : seen) ys

genGlobalEnums :: AEnv -> ExtApi -> String
genGlobalEnums env x =
  unlines $
    apiModuleHeader
      ["DerivingStrategies", "GeneralizedNewtypeDeriving", "PatternSynonyms"]
      ["-- | Godot's global enums (@\\@GlobalScope@). " ++ generatedNote]
      "Godot.Api.GlobalEnums where"
      ++ enumImports
      ++ concat
        [ genEnum (aGlobalEnums env Map.! aeName e) (prefix e) e
        | e <- xGlobalEnums x
        ]
  where
    -- Value names are global in Godot; prefix only those used by several enums.
    counts = Map.fromListWith (+) [(avName v, 1 :: Int) | e <- xGlobalEnums x, v <- nubOn avName (aeValues e)]
    prefix e = if any (\v -> Map.findWithDefault 0 (avName v) counts > 1) (aeValues e) then aGlobalEnums env Map.! aeName e ++ "_" else ""

enumImports :: [String]
enumImports =
  [ ""
  , "import qualified Prelude as P"
  , "import Data.Int (Int64)"
  , "import qualified Data.Bits as Bits"
  , "import qualified Foreign.Storable as S"
  , "import qualified Godot.Api.Internal as R"
  , ""
  ]

--------------------------------------------------------------------------------
-- Godot.Api.Builtin.Types

data BuiltinRepr = Record [MemberOffset] | Opaque

builtinRepr :: AEnv -> BuiltinClass -> BuiltinRepr
builtinRepr env b = case Map.lookup (bcName b) (aOffsets env) of
  Just ms | not (bcHasDestructor b), not (null ms) -> Record ms
  _ -> Opaque

builtinSize :: AEnv -> String -> Int
builtinSize env n = fromMaybe (error ("godot-low: no size for " ++ n)) (Map.lookup n (aSizes env))

memberType :: String -> String
memberType meta = case meta of
  "float" -> "P.Float"
  "double" -> "P.Double"
  "int32" -> "Int32"
  "int64" -> "Int64"
  other -> hsBuiltinName other

genBuiltinTypes :: AEnv -> String
genBuiltinTypes env =
  unlines $
    apiModuleHeader
      ["DerivingStrategies", "GeneralizedNewtypeDeriving", "PatternSynonyms", "TypeApplications"]
      [ "-- | Godot builtin (variant) types. " ++ generatedNote
      , "--"
      , "-- Value types with a fixed layout (@Vector3@, @Color@, @Transform3D@, ...) are"
      , "-- records with 'S.Storable' instances. Others (@GString@ for Godot's @String@,"
      , "-- @Array@, @Dictionary@, ...) are opaque, owned values that are destroyed when"
      , "-- garbage collected. Functions on them are in \"Godot.Api.Builtin\".@Name@."
      ]
      "Godot.Api.Builtin.Types where"
      ++ [ ""
         , "import qualified Prelude as P"
         , "import Data.Int (Int32, Int64)"
         , "import qualified Data.Bits as Bits"
         , "import qualified Foreign.ForeignPtr as FP"
         , "import qualified Foreign.Ptr as F"
         , "import qualified Foreign.Storable as S"
         , "import qualified Godot.GDExtension.Types as G"
         , "import qualified Godot.Api.Internal as R"
         , ""
         ]
      ++ concatMap genOne (Map.elems (aBuiltins env))
  where
    genOne b =
      let n = hsBuiltinName (bcName b)
          vt = variantTypeExpr env (bcName b)
          size = builtinSize env (bcName b)
          ctor0 = "ctor0_" ++ n
       in bbDoc [bcBrief b, bcDesc b]
            ++ ( case builtinRepr env b of
                   Record ms ->
                     let field m = lowerFirst n ++ "_" ++ moName m
                      in [ "data " ++ n ++ " = " ++ n ]
                           ++ [ "  " ++ (if i == 0 then "{ " else ", ") ++ field m ++ " :: !" ++ memberType (moMeta m)
                              | (i, m) <- zip [0 :: Int ..] ms
                              ]
                           ++ [ "  }"
                              , "  deriving stock (P.Eq, P.Show)"
                              , ""
                              , "instance S.Storable " ++ n ++ " where"
                              , "  sizeOf _ = " ++ show size
                              , "  alignment _ = 4"
                              , "  peek p ="
                              , "    P.pure " ++ n
                              ]
                           ++ ["      P.<*> S.peekByteOff p " ++ show (moOffset m) | m <- ms]
                           ++ ["  poke p v = do"]
                           ++ ["    S.pokeByteOff p " ++ show (moOffset m) ++ " (" ++ field m ++ " v)" | m <- ms]
                           ++ [ ""
                              , "instance R.PtrArg " ++ n ++ " where withArg = R.storableArg"
                              , "instance R.PtrRet " ++ n ++ " where withRet = R.storableRet"
                              , "instance R.Builtin " ++ n ++ " where"
                              , "  builtinVariantType = " ++ vt
                              , "  constructWith = R.storableRet"
                              ]
                   Opaque ->
                     [ "newtype " ++ n ++ " = " ++ n ++ " (FP.ForeignPtr " ++ n ++ ")"
                     , ""
                     , "instance R.PtrArg " ++ n ++ " where withArg (" ++ n ++ " fp) = R.opaqueArg fp"
                     , "instance R.PtrRet " ++ n ++ " where"
                     , "  withRet = R.opaqueRet " ++ show size ++ " " ++ dtor b vt ++ " " ++ ctor0 ++ " " ++ n
                     , "instance R.Builtin " ++ n ++ " where"
                     , "  builtinVariantType = " ++ vt
                     , "  constructWith f = " ++ n ++ " P.<$> R.newOpaque " ++ show size ++ " " ++ dtor b vt ++ " f"
                     , ""
                     , "{-# NOINLINE " ++ ctor0 ++ " #-}"
                     , ctor0 ++ " :: G.GDExtensionUninitializedTypePtr -> F.Ptr G.GDExtensionConstTypePtr -> P.IO ()"
                     , ctor0 ++ " = R.builtinConstructor " ++ vt ++ " 0"
                     ]
               )
            ++ [ "instance R.ToVariant " ++ n ++ " where toVariant = R.toVariantVia " ++ vt
               , "instance R.FromVariant " ++ n ++ " where fromVariant = R.fromVariantVia " ++ vt
               , ""
               ]
            ++ concat [genEnum (n ++ "_" ++ aeName e) (n ++ "_") e | e <- bcEnums b]
    dtor b vt = if bcHasDestructor b then "(P.Just " ++ vt ++ ")" else "P.Nothing"

--------------------------------------------------------------------------------
-- Godot.Api.Builtin.<Name>

genBuiltinModule :: AEnv -> BuiltinClass -> String
genBuiltinModule env b =
  unlines $
    apiModuleHeader
      ["FlexibleContexts"]
      ( bbDoc
          [ Just ("Constructors, methods and operators of the Godot builtin type [code]" ++ bcName b ++ "[/code] (" ++ "[code]Godot.Api.Builtin.Types." ++ n ++ "[/code]).")
          , bcBrief b
          ]
      )
      ("Godot.Api.Builtin." ++ bcName b ++ " where")
      ++ [""]
      ++ apiImports
      ++ ["import qualified Godot.Api.Builtin.Types as B", "import qualified Godot.Api.Classes as C", "import qualified Godot.Api.GlobalEnums as E", ""]
      ++ concat ctors
      ++ concat methods
      ++ concat ops
  where
    n = hsBuiltinName (bcName b)
    vt = variantTypeExpr env (bcName b)
    self = "B." ++ n
    (ctorNames, ctors) = unzip' (foldl genCtor ([], []) (bcCtors b))
    (methodNames, methods) = unzip' (foldl genMethod (ctorNames, []) [m | m <- bcMethods b])
    (_, ops) = unzip' (foldl genOp (methodNames, []) (zip [0 :: Int ..] (bcOperators b)))
    unzip' (used, chunks) = (used, reverse chunks)

    genCtor (used, acc) c =
      let base =
            if null (acArgs c)
              then "new"
              else "newFrom" ++ concatMap (pascalType . aaType) (acArgs c)
          name = safeName used base
          cname = "ctor_" ++ show (acIndex c)
          k = length (acArgs c)
          chunk =
            bbDoc ([acDesc c] ++ argsDoc (acArgs c) Nothing)
              ++ [ name ++ " :: " ++ fnSignature env [] (acArgs c) (Just (bcName b))
                 , name ++ " " ++ unwords ["x" ++ show i | i <- [0 .. k - 1]] ++ " ="
                 ]
              ++ ["  R.withArg x" ++ show i ++ " P.$ \\a" ++ show i ++ " ->" | i <- [0 .. k - 1]]
              ++ [ "  R.constructWith P.$ \\p -> R.withArgs " ++ argList k ++ " (" ++ cname ++ " (G.GDExtensionUninitializedTypePtr p))"
                 , ""
                 , "{-# NOINLINE " ++ cname ++ " #-}"
                 , cname ++ " :: G.GDExtensionUninitializedTypePtr -> F.Ptr G.GDExtensionConstTypePtr -> P.IO ()"
                 , cname ++ " = R.builtinConstructor " ++ vt ++ " " ++ show (acIndex c)
                 , ""
                 ]
       in (name : used, chunk : acc)

    genMethod (used, acc) m =
      let name = safeName used (camel (afName m))
          cache = "bm_" ++ name'
          name' = filter (/= '\'') name ++ show (length acc)
          k = length (afArgs m)
          selfArg = if afStatic m then [] else [self]
          xs = ["x" ++ show i | i <- [0 .. k - 1]]
          selfPtr = if afStatic m then "F.nullPtr" else "s"
          sig
            | afVararg m = intercalate " -> " (selfArg ++ ["[R.Variant]", "P.IO " ++ apiType env True (fromMaybe "void" (afReturn m))])
            | otherwise = fnSignature env selfArg (afArgs m) (afReturn m)
          body
            | afVararg m =
                [ name ++ " " ++ unwords (["self" | not (afStatic m)] ++ ["args"]) ++ " ="
                ]
                  ++ ["  R.withArg self P.$ \\s ->" | not (afStatic m)]
                  ++ [ "  R.withRet P.$ \\ret ->"
                     , "  R.callBuiltinVararg " ++ cache ++ " " ++ selfPtr ++ " args ret"
                     ]
            | otherwise =
                [name ++ " " ++ unwords (["self" | not (afStatic m)] ++ xs) ++ " ="]
                  ++ ["  R.withArg self P.$ \\s ->" | not (afStatic m)]
                  ++ marshalCall xs ("R.callBuiltin " ++ cache ++ " " ++ selfPtr ++ " " ++ argList k ++ " ret")
          chunk =
            bbDoc ([afDesc m] ++ argsDoc (afArgs m) (afReturn m) ++ [Just "Vararg: pass every argument as a [code]Variant[/code]." | afVararg m])
              ++ [name ++ " :: " ++ sig]
              ++ body
              ++ [ ""
                 , "{-# NOINLINE " ++ cache ++ " #-}"
                 , cache ++ " :: R.BuiltinMethod"
                 , cache ++ " = R.builtinMethod " ++ vt ++ " " ++ show (afName m) ++ " " ++ show (fromMaybe 0 (afHash m))
                 , ""
                 ]
       in (name : used, chunk : acc)

    genOp (used, acc) (i, o) =
      let opName = fromMaybe (error ("godot-low: unknown operator " ++ aoName o)) (Map.lookup (aoName o) operatorNames)
          opValue = fromMaybe (error ("godot-low: no OP_" ++ opName)) (Map.lookup (normalizeTypeName ("OP_" ++ opName)) (aOperators env))
          name = safeName used ("op" ++ pascal (map toLower opName) ++ maybe "" pascalType (aoRight o))
          cache = "op_" ++ show i
          rightVt = maybe "(G.GDExtensionVariantType 0)" (variantTypeExpr env . operandType) (aoRight o)
          args = [ApiArg "right" r | Just r <- [aoRight o]]
          chunk =
            bbDoc ([aoDesc o] ++ [Just ("Operator [code]" ++ aoName o ++ "[/code].")])
              ++ [ name ++ " :: " ++ fnSignature env [self] args (Just (aoReturn o))
                 , name ++ " self" ++ (if null args then "" else " x0") ++ " ="
                 , "  R.withArg self P.$ \\s ->"
                 ]
              ++ ["  R.withArg x0 P.$ \\a0 ->" | not (null args)]
              ++ [ "  R.withRet P.$ \\ret ->"
                 , "  R.callOperator " ++ cache ++ " s " ++ (if null args then "F.nullPtr" else "a0") ++ " ret"
                 , ""
                 , "{-# NOINLINE " ++ cache ++ " #-}"
                 , cache ++ " :: G.GDExtensionConstTypePtr -> G.GDExtensionConstTypePtr -> G.GDExtensionTypePtr -> P.IO ()"
                 , cache ++ " = R.operatorEvaluator (G.GDExtensionVariantOperator " ++ show opValue ++ ") " ++ vt ++ " " ++ rightVt
                 , ""
                 ]
       in (name : used, chunk : acc)

    operandType t
      | t `elem` aAllClassNames env = "Object"
      | otherwise = case t of
          "bool" -> "BOOL"
          "int" -> "INT"
          "float" -> "FLOAT"
          _ -> t

-- | Type name as a PascalCase name component (for overload names).
pascalType :: String -> String
pascalType t = case t of
  "bool" -> "Bool"
  "int" -> "Int"
  "float" -> "Float"
  _ -> concatMap cap (splitOn '_' (map (\c -> if isAlphaNum c then c else '_') (afterColons t)))
  where
    cap (c : cs) = toUpper c : cs
    cap [] = []
    afterColons s = case breakOnStr "::" s of
      (_, ':' : ':' : r) -> r
      _ -> s

--------------------------------------------------------------------------------
-- Godot.Api.Classes

classAncestors :: AEnv -> ApiClass -> [String]
classAncestors env c = clName c : maybe [] up (clInherits c)
  where
    up p = case Map.lookup p (aClasses env) of
      Just pc -> classAncestors env pc
      Nothing -> []

genClasses :: AEnv -> String
genClasses env =
  unlines $
    apiModuleHeader
      ["DerivingStrategies", "GeneralizedNewtypeDeriving", "MultiParamTypeClasses", "PatternSynonyms"]
      [ "-- | Godot engine classes. " ++ generatedNote
      , "--"
      , "-- Each class is a handle to a Godot object. @'R.Inherits' a b@ holds when"
      , "-- @a@ is @b@ or a subclass, so methods of @b@ (in \"Godot.Api.Class\".@b@)"
      , "-- accept an @a@; use 'R.upcast' to convert explicitly and 'R.cast' to"
      , "-- downcast. Class enums are named @Class_Enum@ with patterns"
      , "-- @Class_VALUE@."
      ]
      "Godot.Api.Classes where"
      ++ [ ""
         , "import qualified Prelude as P"
         , "import Data.Int (Int64)"
         , "import qualified Data.Bits as Bits"
         , "import qualified Foreign.Storable as S"
         , "import qualified Godot.GDExtension.Types as G"
         , "import qualified Godot.Api.Internal as R"
         , ""
         ]
      ++ concatMap genOne (Map.elems (aClasses env))
  where
    refNote =
      "[i]RefCounted:[/i] these bindings do not manage references. Objects returned by "
        ++ "methods or created with [code]construct[/code] carry a reference you must release "
        ++ "with [code]RefCounted.unreference[/code]."
    genOne c =
      let n = clName c
          obj = "(G.GDExtensionVariantType " ++ show (aVariantTypes env Map.! "OBJECT") ++ ")"
       in bbDoc ([clBrief c, clDesc c] ++ [Just refNote | clRefcounted c])
            ++ [ "newtype " ++ n ++ " = " ++ n ++ " G.GDExtensionObjectPtr"
               , "  deriving stock (P.Show)"
               , "  deriving newtype (P.Eq, P.Ord)"
               , ""
               , "instance R.GodotClass " ++ n ++ " where"
               , "  godotClassName = " ++ show n
               , "  toObjectPtr (" ++ n ++ " p) = p"
               , "  unsafeFromObjectPtr = " ++ n
               , "instance R.PtrArg " ++ n ++ " where withArg = R.objectArg"
               , "instance R.ToVariant " ++ n ++ " where toVariant = R.toVariantVia " ++ obj
               ]
            ++ ["instance R.Inherits " ++ n ++ " " ++ a | a <- classAncestors env c]
            ++ [""]
            ++ concat [genEnum (n ++ "_" ++ aeName e) (n ++ "_") e | e <- clEnums c]

--------------------------------------------------------------------------------
-- Godot.Api.Class.<Name>

genClassModule :: AEnv -> ApiClass -> String
genClassModule env c =
  unlines $
    apiModuleHeader
      ["FlexibleContexts", "PatternSynonyms", "TypeApplications"]
      ( bbDoc
          ( Just ("Methods of the Godot class [code]" ++ n ++ "[/code] ([code]Godot.Api.Classes." ++ n ++ "[/code])" ++ inheritsNote ++ ".")
              : [clBrief c, clDesc c]
          )
      )
      ("Godot.Api.Class." ++ n)
      ++ ["  ( " ++ intercalate "\n  , " exports ++ "\n  ) where" | not (null exports)]
      ++ ["  where" | null exports]
      ++ [""]
      ++ apiImports
      ++ ["import qualified Godot.Api.Builtin.Types as B", "import qualified Godot.Api.Classes as C", "import qualified Godot.Api.GlobalEnums as E", ""]
      ++ concat extras
      ++ concat constants
      ++ concat methodChunks
  where
    n = clName c
    inheritsNote = maybe "" (\p -> ", which inherits [code]" ++ p ++ "[/code]") (clInherits c)
    methods = [m | m <- clMethods c, not (afVirtual m)]
    -- Extra helpers: construct (instantiable classes), singleton.
    (extraNames, extras) =
      unzip $
        [ ( "construct"
          , [ "-- | Create a new @" ++ n ++ "@ (see 'R.construct')."
            , "construct :: P.IO C." ++ n
            , "construct = R.construct @C." ++ n
            , ""
            ]
          )
        | clInstantiable c
        , "construct" `notElem` methodBaseNames
        ]
          ++ [ ( "singleton"
               , [ "-- | The @" ++ n ++ "@ singleton (see 'R.singleton')."
                 , "singleton :: P.IO C." ++ n
                 , "singleton = R.singleton @C." ++ n
                 , ""
                 ]
               )
             | isSingletonClass
             , "singleton" `notElem` methodBaseNames
             ]
    isSingletonClass = n `elem` singletonClassNames
    singletonClassNames = aSingletons env
    methodBaseNames = map (camel . afName) methods
    constants =
      [ bbDoc [avDesc v]
          ++ [ "pattern " ++ avName v ++ " :: Int64"
             , "pattern " ++ avName v ++ " = " ++ (if avValue v < 0 then "(" ++ show (avValue v) ++ ")" else show (avValue v))
             , ""
             ]
      | v <- validConstants
      ]
    validConstants = [v | v <- nubOn avName (clConstants c), startsUpper (avName v)]
    (methodNames, methodChunks) =
      let (used, chunks) = foldl step (extraNames, []) methods
       in (take (length methods) used, reverse chunks)
    step (used, acc) m =
      let name = safeName used (camel (afName m))
       in (name : used, genClassMethod env c m name (length acc) : acc)
    exports =
      extraNames
        ++ ["pattern " ++ avName v | v <- validConstants]
        ++ reverse methodNames

genClassMethod :: AEnv -> ApiClass -> ApiFn -> String -> Int -> [String]
genClassMethod env c m name idx =
  bbDoc ([afDesc m] ++ argsDoc (afArgs m) (afReturn m) ++ notes)
    ++ [name ++ " :: " ++ sig]
    ++ body
    ++ [ ""
       , "{-# NOINLINE " ++ cache ++ " #-}"
       , cache ++ " :: G.GDExtensionMethodBindPtr"
       , cache ++ " = R.methodBind " ++ show (clName c) ++ " " ++ show (afName m) ++ " " ++ show (fromMaybe 0 (afHash m))
       , ""
       ]
  where
    cache = "mb_" ++ show idx
    k = length (afArgs m)
    xs = ["x" ++ show i | i <- [0 .. k - 1]]
    selfTy = if afStatic m then [] else ["self"]
    ctx = if afStatic m then "" else "R.Inherits self C." ++ clName c ++ " => "
    obj = if afStatic m then "R.nullObjectPtr" else "(R.toObjectPtr self)"
    notes =
      [Just "Vararg: pass every argument (including the fixed ones above) as a [code]Variant[/code]." | afVararg m]
        ++ [Just "Static method." | afStatic m]
    sig
      | afVararg m = ctx ++ intercalate " -> " (selfTy ++ ["[R.Variant]", "P.IO R.Variant"])
      | otherwise = ctx ++ fnSignature env selfTy (afArgs m) (afReturn m)
    body
      | afVararg m =
          [ name ++ " " ++ unwords (selfTy ++ ["args"]) ++ " ="
          , "  R.varargCall " ++ cache ++ " " ++ obj ++ " args"
          ]
      | otherwise =
          [name ++ " " ++ unwords (selfTy ++ xs) ++ " ="]
            ++ marshalCall xs ("R.ptrcall " ++ cache ++ " " ++ obj ++ " " ++ argList k ++ " ret")

--------------------------------------------------------------------------------
-- Godot.Api.Utility

genUtility :: AEnv -> ExtApi -> String
genUtility env x =
  unlines $
    apiModuleHeader
      []
      [ "-- | Godot's global utility functions (@\\@GlobalScope@: @sin@, @print@, ...)."
      , "-- " ++ generatedNote
      , "--"
      , "-- Import qualified: many names overlap with the Prelude."
      ]
      "Godot.Api.Utility where"
      ++ [""]
      ++ apiImports
      ++ ["import qualified Godot.Api.Builtin.Types as B", "import qualified Godot.Api.Classes as C", "import qualified Godot.Api.GlobalEnums as E", ""]
      ++ concat (reverse chunks)
  where
    (_, chunks) = foldl step ([], []) (zip [0 :: Int ..] (xUtility x))
    step (used, acc) (i, f) =
      let name = safeName used (camel (afName f))
          cache = "uf_" ++ show i
          k = length (afArgs f)
          xs = ["x" ++ show j | j <- [0 .. k - 1]]
          sig
            | afVararg f = "[R.Variant] -> P.IO " ++ apiType env True (fromMaybe "void" (afReturn f))
            | otherwise = fnSignature env [] (afArgs f) (afReturn f)
          body
            | afVararg f = [name ++ " args =", "  R.withRet P.$ \\ret ->", "  R.callUtilityVararg " ++ cache ++ " args ret"]
            | otherwise = [name ++ " " ++ unwords xs ++ " ="] ++ marshalCall xs ("R.callUtility " ++ cache ++ " " ++ argList k ++ " ret")
          chunk =
            bbDoc ([afDesc f] ++ argsDoc (afArgs f) (afReturn f) ++ [Just "Vararg: pass every argument as a [code]Variant[/code]." | afVararg f])
              ++ [name ++ " :: " ++ sig]
              ++ body
              ++ [ ""
                 , "{-# NOINLINE " ++ cache ++ " #-}"
                 , cache ++ " :: R.UtilityFunction"
                 , cache ++ " = R.utilityFunction " ++ show (afName f) ++ " " ++ show (fromMaybe 0 (afHash f))
                 , ""
                 ]
       in (name : used, chunk : acc)
