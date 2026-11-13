{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

-- | The option forms of ghcup's TUI (install with options, compile GHC,
-- compile HLS) as dialogs. Each field is checked with the parser the TUI
-- uses, and its error is shown under it; OK is enabled once all are valid.
module GHCup.GUI.Forms
  ( State,
    Fields,
    noFields,
    FormResult (..),
    installForm,
    compileGHCForm,
    compileHLSForm,
  )
where

import Control.Monad (unless)
import Data.Bifunctor (bimap, first)
import Data.Char (isSpace)
import Data.IORef
import Data.List (dropWhileEnd)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Versions (Version, prettyVer, version)
import GHCup.GUI.Actions
import qualified GHCup.Input.Parsers as Parsers
import GHCup.Types (BuildSystem (..), ToolVersion (..), VersionReq (..))
import Godot.ImUI
import System.FilePath (isPathSeparator)
import Text.Read (readEither)
import URI.ByteString (URI)

-- | A piece of state and its setter, as 'state' returns them.
type State a = (a, a -> IO ())

-- | What has been entered in a form, kept while it is closed (as the TUI
-- does), so that a typo can be fixed and the form submitted again.
data Fields = Fields
  { texts :: Map.Map String String,
    checks :: Map.Map String Bool,
    choices :: Map.Map String Int
  }
  deriving (Eq)

noFields :: Fields
noFields = Fields Map.empty Map.empty Map.empty

-- | A form this frame.
data FormResult a
  = -- | still open
    Editing
  | -- | closed without submitting
    Cancelled'
  | Submitted a

-- | A form being drawn: what was entered at the start of the frame, and what
-- is entered now.
data Form = Form Fields (IORef Fields)

-- | A form dialog: the body draws the fields (in a two-column grid) and
-- returns the options, or why they are not valid.
formDialog :: String -> String -> (String, String) -> State Fields -> (Form -> UI (Either String a)) -> UI (FormResult a)
formDialog title intro (okLabel, okHelp) (fields, setFields) body = do
  r <- dialog title $ do
    paragraph intro
    ref <- liftIO (newIORef fields)
    result <- gridWith gridOpts {minSize = (560, 0)} 2 (body (Form fields ref))
    fields' <- liftIO (readIORef ref)
    unless (fields' == fields) (liftIO (setFields fields'))
    either (\e -> unless (null e) (errorText e)) (const (pure ())) result
    hbox $ do
      spacer
      cancel <- button "Cancel"
      ok <- buttonWith buttonOpts {enabled = either (const False) (const True) result, tooltip = okHelp} okLabel
      pure $ case result of
        Right a | ok -> Just (Just a)
        _ | cancel -> Just Nothing
        _ -> Nothing
  pure $ case r of
    Nothing -> Cancelled' -- closed by the window manager
    Just Nothing -> Editing
    Just (Just Nothing) -> Cancelled'
    Just (Just (Just a)) -> Submitted a

errorText :: String -> UI ()
errorText e = richText ("[color=#e06c75]" ++ concatMap escape e ++ "[/color]")
  where
    escape '[' = "[lb]"
    escape c = [c]

-- | A text field: its label (with the help as tooltip), and the input with
-- the parser's error under it.
textField :: Form -> String -> String -> String -> (String -> Either String a) -> UI (Either String a)
textField (Form fields ref) name help hint parse = do
  labelWith labelOpts {tooltip = help} name
  vbox $ do
    let value = Map.findWithDefault "" name (texts fields)
    value' <- textInputWith textInputOpts {placeholder = hint, tooltip = help, minSize = (360, 0)} value
    liftIO (modifyIORef' ref (\f -> f {texts = Map.insert name value' (texts f)}))
    let r = parse value'
    -- The error is shown here, so the form does not show it again.
    either errorText (const (pure ())) r
    pure (first (const "") r)

checkField :: Form -> String -> String -> UI Bool
checkField (Form fields ref) name help = do
  labelWith labelOpts {tooltip = help} name
  on <- checkboxWith checkboxOpts {tooltip = help} "" (Map.findWithDefault False name (checks fields))
  liftIO (modifyIORef' ref (\f -> f {checks = Map.insert name on (checks f)}))
  pure on

choiceField :: Form -> String -> String -> [String] -> UI Int
choiceField (Form fields ref) name help options = do
  labelWith labelOpts {tooltip = help} name
  i <- dropdownWith dropdownOpts {tooltip = help} options (Map.findWithDefault 0 name (choices fields))
  liftIO (modifyIORef' ref (\f -> f {choices = Map.insert name i (choices f)}))
  pure i

-- | 'Nothing' for an empty field, otherwise the parse of what is in it.
optional :: (String -> Either String a) -> String -> Either String (Maybe a)
optional p s = case trim s of
  "" -> Right Nothing
  s' -> Just <$> p s'

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

absolutePath :: String -> Either String FilePath
absolutePath = Parsers.absolutePathParser

-- | A version, or an absolute path if it has a path separator.
versionOrPath :: String -> Either String (Either Version FilePath)
versionOrPath s
  | any isPathSeparator s = Right <$> absolutePath s
  | otherwise = bimap (const "Not a valid version") Left (version (T.pack s))

-- | A patch directory (absolute path), or patch URIs (space separated).
patchesParser :: String -> Either String (Either FilePath [URI])
patchesParser s = case absolutePath s of
  Right p -> Right (Left p)
  Left _ -> Right <$> traverse Parsers.uriParser (words s)

jobsParser :: String -> Either String Int
jobsParser = first (const "Invalid value. Must be an integer") . readEither

-- | The TUI's "Install" in a version's context menu.
installForm :: String -> State Fields -> UI (FormResult InstallOptions)
installForm what fields =
  formDialog
    "Install with options"
    ("Install " ++ what ++ " with the options below. (Hover over a field for help.)")
    ("Install", "Install with the options above")
    fields
    $ \f -> do
      url <- textField f "url" "Install the specified version from this bindist" "" (optional Parsers.uriParser)
      set <- checkField f "set" "Set as active version after install"
      ver <- textField f "version" "Specify a custom version" "the selected version" (optional Parsers.ghcVersionEither')
      targets <- textField f "install-targets" "Overwrite install targets (space separated list)" "" (optional (Right . T.pack))
      isolated <-
        textField f "isolated" "Install in an isolated absolute directory instead of the default one" "" (optional absolutePath)
      force <- checkField f "force" "Force install (THIS IS UNSAFE, only use it in Dockerfiles or CI)"
      confArgs <- textField f "CONFIGURE_ARGS" "Additional arguments to bindist configure" "" (Right . map T.pack . words)
      pure $ do
        opts <-
          InstallOptions
            <$> url
            <*> pure set
            <*> ver
            <*> isolated
            <*> pure force
            <*> confArgs
            <*> targets
        case (instSet opts, isolateDir opts) of
          (True, Just _) -> Left "Cannot set active when doing an isolated install"
          _ -> Right opts

-- | The TUI's "Compile GHC": takes the GHC versions installed, to bootstrap
-- with.
compileGHCForm :: String -> [Version] -> State Fields -> UI (FormResult CompileGHCOptions)
compileGHCForm what installed fields =
  formDialog
    "Compile GHC"
    ("Compile " ++ what ++ " from source with the options below. (Hover over a field for help.)")
    ("Compile", "Compile GHC from source with the options above")
    fields
    $ \f -> do
      bootstrap <- ghcChoice f "bootstrap-ghc" "The GHC version (or full path) to bootstrap with (must be installed)" True
      hadrian <- ghcChoice f "hadrian-ghc" "The GHC version (or full path) that will be used to compile hadrian (must be installed)" False
      jobs' <- textField f "jobs" "How many jobs to use for make" "" (optional jobsParser)
      set <- checkField f "set" "Set as active version after install"
      flavour <- textField f "flavour" "Set the compile build flavour (this value depends on the build system type: 'make' vs 'hadrian')" "" (optional Right)
      docs' <- textField f "docs" "Disable some or all of the docs (e.g. 'none' or 'no-sphinx')" "" (optional Right)
      confArgs <- textField f "CONFIGURE_ARGS" "Additional arguments to bindist configure" "" (Right . map T.pack . words)
      config <- textField f "build config" "Absolute path to build config file (make build system only)" "" (optional absolutePath)
      patches' <- textField f "patches" "Either a URI to a patch (https/http/file) or absolute path to patch directory" "" (optional patchesParser)
      cross <- textField f "cross target" "Build cross-compiler for this platform" "" (optional (Right . T.pack))
      system <-
        choiceField
          f
          "build system"
          "Select the build system"
          ["Auto select (prefer hadrian if available, and build config is not specified)", "hadrian", "make"]
      overwrite <-
        textField
          f
          "overwrite-version"
          "Allows to overwrite the finally installed VERSION with a different one. Allows to specify patterns: %v (version), %b (branch name), %h (short commit hash), %H (long commit hash), %g ('git describe' output)"
          ""
          (optional Parsers.overWriteVersionParser)
      isolated <- textField f "isolated" "Install in an isolated absolute directory instead of the default one" "" (optional absolutePath)
      ref' <- textField f "git-ref" "The git commit/branch/ref to build from" "" (optional Right)
      targets <- textField f "install-targets" "Overwrite install targets (space separated list)" "" (optional (Right . T.pack))
      let buildSystem' = [Nothing, Just Hadrian, Just Make] !! system
      pure $ do
        bootstrap' <- bootstrap >>= maybe (Left "") Right
        opts <-
          CompileGHCOptions bootstrap'
            <$> hadrian
            <*> jobs'
            <*> config
            <*> patches'
            <*> cross
            <*> confArgs
            <*> pure set
            <*> overwrite
            <*> flavour
            <*> docs'
            <*> pure buildSystem'
            <*> isolated
            <*> ref'
            <*> targets
        case (setCompile opts, ghcIsolateDir opts, buildConfig opts, buildSystem opts) of
          (True, Just _, _, _) -> Left "Cannot set active when doing an isolated install"
          (_, _, Just _, Just Hadrian) -> Left "Build config can be specified only for make build system"
          _ -> Right opts
  where
    -- An installed GHC, or another version or path typed in.
    ghcChoice :: Form -> String -> String -> Bool -> UI (Either String (Maybe (Either Version FilePath)))
    ghcChoice f name help required = do
      let other = if required then "Other version or path" else "Other version or path (or the bootstrap GHC)"
          options = map (T.unpack . prettyVer) installed ++ [other]
      i <- if null installed then pure (length installed) else choiceField f name help options
      if i < length installed
        then pure (Right (Just (Left (installed !! i))))
        else do
          let name' = if null installed then name else name ++ " (other)"
          textField f name' help "a version, or an absolute path" $ \s -> case trim s of
            "" | required -> Left "No version selected / no path specified"
            _ -> optional versionOrPath s

-- | The TUI's "Compile HLS": takes the GHC versions installed, to compile
-- for.
compileHLSForm :: String -> [Version] -> State Fields -> UI (FormResult CompileHLSOptions)
compileHLSForm what installed fields =
  formDialog
    "Compile HLS"
    ("Compile " ++ what ++ " from source with the options below. (Hover over a field for help.)")
    ("Compile", "Compile HLS from source with the options above")
    fields
    $ \f@(Form values ref) -> do
      -- Target GHCs: the installed ones checked, and others typed in.
      labelWith labelOpts {tooltip = "GHC versions to compile for"} "target GHC(s)"
      chosen <- flow $ fmap concat $ mapM
        ( \v -> do
            let name = "ghc " ++ T.unpack (prettyVer v)
            on <- checkbox (T.unpack (prettyVer v)) (Map.findWithDefault False name (checks values))
            liftIO (modifyIORef' ref (\x -> x {checks = Map.insert name on (checks x)}))
            pure [ToolVersion (VersionReq v Nothing) | on]
        )
        installed
      others <-
        textField
          f
          (if null installed then "target GHC(s)" else "other GHC(s)")
          "Space separated list of GHC versions (or tags) to compile for"
          ""
          (traverse Parsers.ghcVersionTagEither . words)
      update <- checkField f "cabal update" "Run 'cabal update' before the build"
      jobs' <- textField f "jobs" "How many jobs to use for make" "" (optional jobsParser)
      set <- checkField f "set" "Set as active version after install"
      cabalArgs' <- textField f "CABAL_ARGS" "Additional arguments to cabal install" "" (Right . map T.pack . words)
      isolated <- textField f "isolated" "Install in an isolated absolute directory instead of the default one" "" (optional Parsers.isolateParser)
      overwrite <-
        textField
          f
          "overwrite version"
          "Allows to overwrite the finally installed VERSION with a different one. Allows to specify patterns: %v (version), %b (branch name), %h (short commit hash), %H (long commit hash), %g ('git describe' output)"
          ""
          (optional Parsers.overWriteVersionParser)
      patches' <- textField f "patches" "Either a URI to a patch (https/http/file) or Absolute path to patch directory" "" (optional patchesParser)
      project <-
        textField
          f
          "cabal project"
          "If relative filepath, specifies the path to cabal.project inside the unpacked HLS tarball/checkout. Otherwise expects a full URI with https/http/file scheme."
          ""
          (optional (\s -> Right (either (const (Left s)) Right (Parsers.uriParser s))))
      projectLocal <-
        textField f "cabal project local" "URI (https/http/file) to a cabal.project.local to be used for the build. Will be copied over." "" (optional Parsers.uriParser)
      ref' <- textField f "git-ref" "The git commit/branch/ref to build from" "" (optional Right)
      pure $ do
        targets <- (chosen ++) <$> others
        opts <-
          CompileHLSOptions
            <$> jobs'
            <*> pure set
            <*> pure update
            <*> overwrite
            <*> isolated
            <*> project
            <*> projectLocal
            <*> patches'
            <*> pure targets
            <*> cabalArgs'
            <*> ref'
        case (hlsSetCompile opts, hlsIsolateDir opts, targetGHCs opts) of
          (True, Just _, _) -> Left "Cannot set active when doing an isolated install"
          (_, _, []) -> Left "Specify at least one valid target GHC"
          _ -> Right opts
