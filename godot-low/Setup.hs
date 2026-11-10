{-# LANGUAGE OverloadedStrings #-}

-- | Custom setup for godot-low.
--
-- Before building (and before haddock / repl), generates the low-level
-- GDExtension bindings from the @gdextension_interface.json@ that the
-- godot-sys dependency installs next to its C headers:
--
-- * @Godot.Low.Types@ (an @.hsc@ file, so struct sizes and offsets come
--   from the real C header via hsc2hs): aliases, handles, enums, structs and
--   function pointer types with @wrapper@ / @dynamic@ imports.
-- * @Godot.Low.Interface@: a record with every interface function, and
--   'loadInterface' to fill it in from Godot's @get_proc_address@.
--
-- Files are written into the package's autogen directory, and only when
-- their contents change.
module Main (main) where

import Control.Monad (filterM, when)
import Data.Aeson
import Data.Char (isUpper, toLower, toUpper)
import Data.List (intercalate, isPrefixOf, isSuffixOf, nub)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Distribution.InstalledPackageInfo as IPI
import Distribution.Simple
import Distribution.Simple.BuildPaths (autogenPackageModulesDir)
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
import System.Directory
import System.FilePath

main :: IO ()
main =
  defaultMainWithHooks
    simpleUserHooks
      { buildHook = \pd lbi hooks flags -> do
          generate (fromFlagOrDefault normal (buildVerbosity flags)) lbi
          buildHook simpleUserHooks pd lbi hooks flags
      , haddockHook = \pd lbi hooks flags -> do
          generate (fromFlagOrDefault normal (haddockVerbosity flags)) lbi
          haddockHook simpleUserHooks pd lbi hooks flags
      , replHook = \pd lbi hooks flags args -> do
          generate (fromFlagOrDefault normal (replVerbosity flags)) lbi
          replHook simpleUserHooks pd lbi hooks flags args
      }

--------------------------------------------------------------------------------
-- Driver

generate :: Verbosity -> LocalBuildInfo -> IO ()
generate verb lbi = do
  json <- findInterfaceJson lbi
  info verb ("godot-low: generating bindings from " ++ json)
  api <- either (fail . (("godot-low: cannot parse " ++ json ++ ": ") ++)) pure =<< eitherDecodeFileStrict json
  let outDir = interpretSymbolicPathLBI lbi (autogenPackageModulesDir lbi) </> "Godot" </> "Low"
      env = mkEnv api
  createDirectoryIfMissing True outDir
  w1 <- writeIfChanged (outDir </> "Types.hsc") (genTypes env api)
  w2 <- writeIfChanged (outDir </> "Interface.hs") (genInterface env api)
  when (w1 || w2) $ notice verb ("godot-low: generated bindings in " ++ outDir)

-- | Locate gdextension_interface.json in the include dirs of godot-sys.
findInterfaceJson :: LocalBuildInfo -> IO FilePath
findInterfaceJson lbi = do
  let pkgs = concatMap snd (lookupPackageName (installedPkgs lbi) (mkPackageName "godot-sys"))
      candidates = nub [d </> "gdextension_interface.json" | p <- pkgs, d <- IPI.includeDirs p]
  found <- filterM doesFileExist candidates
  case found of
    f : _ -> pure f
    [] ->
      fail $
        "godot-low: gdextension_interface.json not found in godot-sys include dirs: "
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
-- Godot.Low.Types

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
    , "module Godot.Low.Types where"
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
      ++ section "Enums" (concatMap genEnum (ofKind "enum"))
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

    genEnum t =
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
-- Godot.Low.Interface

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
    , "module Godot.Low.Interface"
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
         , "import Godot.Low.Types"
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
         , "  | p == nullFunPtr = error (\"Godot.Low.Interface: \" ++ name ++ \" is not provided by this Godot build\")"
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
