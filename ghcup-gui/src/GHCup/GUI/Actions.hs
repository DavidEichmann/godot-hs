{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | What the GUI does with a tool version, done by the ghcup library: the
-- actions of ghcup's TUI (@GHCup.Brick.Actions@), without the terminal. Each
-- returns an error message, or 'Right' on success; the details go to ghcup's
-- log.
module GHCup.GUI.Actions
  ( GHCup,

    -- * Listing
    loadVersions,

    -- * Installing
    InstallOptions (..),
    defaultInstallOptions,
    installVersion,

    -- * Setting and removing
    SetResult (..),
    setVersion,
    removeVersion,
    changelog,
    openURL,

    -- * Compiling
    CompileGHCOptions (..),
    compileGHC,
    CompileHLSOptions (..),
    compileHLS,
  )
where

import Control.Concurrent (threadDelay)
import Control.Monad (forM, forM_, void, when)
import Control.Monad.Reader
import Control.Monad.Trans.Resource (runResourceT)
import Data.Functor (($>))
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Variant.Excepts
import Data.Versions (Version, prettyVer)
import GHCup.Command.Install
import GHCup.Command.List
import GHCup.Command.Rm
import GHCup.Command.Set
import GHCup.Command.Upgrade
import qualified GHCup.Command.Compile.GHC as GHC
import qualified GHCup.Command.Compile.HLS as HLS
import GHCup.Errors
import GHCup.Input.Parsers (resolveVersion)
import GHCup.Prelude
import GHCup.Prelude.Process (exec)
import GHCup.Query.GHCupDirs (fromGHCupPath)
import GHCup.Query.Metadata
import GHCup.Types hiding (LeanAppState (..))
import System.Exit (ExitCode (..))
import System.Process (system)
import Text.PrettyPrint.HughesPJClass (prettyShow)
import URI.ByteString (URI, serializeURIRef')

-- | ghcup's commands run with its state.
type GHCup = ReaderT AppState IO

-- | Every version of every tool, as @ghcup tui@ lists them: all versions,
-- the latest nightly, and revisions that are updates.
loadVersions :: GHCup (Either String ToolListResult)
loadVersions =
  either (Left . prettyHFError) Right . veitherToEither
    <$> runE (listVersions Nothing [] ShowUpdates False NShowLatest (Nothing, Nothing))

-- | The options of "Install with options" (the TUI's advanced install).
data InstallOptions = InstallOptions
  { -- | install the version from this bindist
    instBindist :: Maybe URI,
    -- | set it as the active version after installing
    instSet :: Bool,
    -- | install this version instead of the selected one
    instVersion :: Maybe TargetVersionReq,
    -- | install in this absolute directory instead of ghcup's
    isolateDir :: Maybe FilePath,
    forceInstall :: Bool,
    -- | more arguments to the bindist's configure
    addConfArgs :: [Text],
    -- | the install targets instead of the default ones
    installTargets :: Maybe Text
  }

defaultInstallOptions :: InstallOptions
defaultInstallOptions = InstallOptions Nothing False Nothing Nothing False [] Nothing

-- | Install a tool version (for ghcup itself: upgrade it).
installVersion :: InstallOptions -> Tool -> Maybe ToolDescription -> ListResult -> GHCup (Either String ())
installVersion opts lTool td ListResult {..} = do
  AppState {ghcupInfo = GHCupInfo {_ghcupDownloads = dls}} <- ask
  let misolated = isolateDir opts
      shouldIsolate = maybe GHCupInternal IsolateDir misolated
      shouldSet = instSet opts
      extraArgs = T.unpack <$> addConfArgs opts
      targets = words . T.unpack <$> installTargets opts
      v = fromMaybe (TargetVersionReq (TargetVersion lCross lVer) (Just (fst lRev))) (instVersion opts)
      vm = getLatest dls lTool >>= \t -> getVersionMetadata t lTool dls
      preInstall = forM_ (_vmPreInstall =<< vm) $ \msg -> do
        lift $ logWarn msg
        lift $ logWarn "...waiting for 5 seconds, you can still abort..."
        liftIO $ threadDelay 5000000
      withNoVerify = local (\s -> s {settings = (settings s) {noVerify = True}})
      setAfter = when (shouldSet && isNothing misolated) (liftE $ void $ setToolVersion lTool (_tvqTargetVer v))
  r <-
    runResourceT . runE @InstallErrors $ case lTool of
      Tool "ghcup" -> do
        preInstall
        void $ liftE $ upgradeGHCup Nothing False False
      _ -> do
        preInstall
        case instBindist opts of
          Nothing ->
            liftE $ runBothE' (installTool lTool v shouldIsolate (forceInstall opts) extraArgs targets) setAfter
          Just uri ->
            liftE $
              runBothE'
                ( do
                    rev <- liftE $ maybe (fmap fst $ getDownloadInfoE' lTool v) pure (_tvqRev v)
                    withNoVerify $
                      installBindist
                        lTool
                        td
                        (DownloadInfo ((decUTF8Safe . serializeURIRef') uri) (Just $ RegexDir "ghc-.*") "" Nothing Nothing Nothing Nothing)
                        (TargetVersionRev (_tvqTargetVer v) rev)
                        shouldIsolate
                        (forceInstall opts)
                        extraArgs
                        targets
                )
                setAfter
  case r of
    VRight () -> do
      forM_ (_vmPostInstall =<< vm) logInfo
      when (lTool == Tool "ghcup") $
        logInfo "GHCup was upgraded: the ghcup command uses the new version."
      pure (Right ())
    VLeft (V (AlreadyInstalled _ _)) -> pure (Right ())
    VLeft (V NoUpdate) -> pure (Right ())
    VLeft e ->
      pure . Left $
        prettyHFError e <> "\nAlso check the logs in ~/.ghcup/logs or $XDG_STATE_HOME/ghcup/logs"

type InstallErrors =
  '[ AlreadyInstalled,
     ArchiveResult,
     UnknownArchive,
     FileDoesNotExistError,
     CopyError,
     NoDownload,
     NotInstalled,
     BuildFailed,
     TagNotFound,
     DigestError,
     ContentLengthError,
     GPGError,
     DownloadFailed,
     DirNotEmpty,
     NoUpdate,
     TarDirDoesNotExist,
     FileAlreadyExistsError,
     ProcessError,
     ToolShadowed,
     UninstallFailed,
     MergeFileTreeError,
     NoCompatiblePlatform,
     ParseError,
     UnsupportedSetupCombo,
     DistroNotFound,
     NoCompatibleArch,
     InstallSetError,
     URIParseError,
     NoInstallInfo,
     MalformedInstallInfo,
     InvalidBuildConfig
   ]

-- | What setting a version did.
data SetResult
  = SetDone
  | -- | the version is not installed (the TUI then offers to install it)
    SetNotInstalled
  | SetFailed String

-- | Make a version the active one. (Not for ghcup itself, which can only be
-- upgraded.)
setVersion :: Tool -> ListResult -> GHCup SetResult
setVersion lTool ListResult {..} =
  runE @'[ParseError, NotInstalled] (setToolVersion lTool (TargetVersion lCross lVer)) >>= \case
    VRight _ -> pure SetDone
    VLeft (V (NotInstalled _ _)) -> pure SetNotInstalled
    VLeft e -> pure (SetFailed (prettyHFError e))

-- | Uninstall a version.
removeVersion :: Tool -> ListResult -> GHCup (Either String ())
removeVersion lTool ListResult {..} = do
  AppState {ghcupInfo = GHCupInfo {_ghcupDownloads = dls}} <- ask
  let crossVer = TargetVersion lCross lVer
  runE @'[NotInstalled, UninstallFailed, ParseError, MalformedInstallInfo]
    ( case lTool of
        Tool "ghcup" -> pure Nothing
        _ -> liftE $ rmToolVersion lTool crossVer $> getVersionMetadata crossVer lTool dls
    )
    >>= \case
      VRight vi -> do
        logInfo $
          "Successfully removed "
            <> T.pack (prettyShow lTool)
            <> " "
            <> (if lTool == ghc then tVerToText crossVer else prettyVer lVer)
        forM_ (_vmPostRemove =<< vi) logInfo
        pure (Right ())
      VLeft e -> pure (Left (prettyHFError e))

-- | Open a version's changelog in the browser.
changelog :: Tool -> ListResult -> GHCup (Either String ())
changelog lTool ListResult {..} = do
  AppState {ghcupInfo = GHCupInfo {_ghcupDownloads = dls}} <- ask
  case getChangeLog dls lTool (mkTVer lVer) of
    Nothing ->
      pure . Left $
        "Could not find ChangeLog for " <> prettyShow lTool <> ", version " <> T.unpack (prettyVer lVer)
    Just uri -> openURL (T.unpack (decUTF8Safe (serializeURIRef' uri)))

-- | Open a URL in the browser (as the TUI opens changelogs).
openURL :: String -> GHCup (Either String ())
openURL url = do
  AppState {pfreq} <- ask
  r <- case _rPlatform pfreq of
    Darwin -> exec "open" [url] Nothing Nothing
    Windows -> do
      let args = "start \"\" " ++ url
      c <- liftIO $ system args
      pure $ case c of
        ExitFailure xi -> Left (NonZeroExit xi "cmd.exe" [args])
        ExitSuccess -> Right ()
    _ -> exec "xdg-open" [url] Nothing Nothing
  pure (either (Left . prettyHFError) Right r)

-- | The options of "Compile GHC".
data CompileGHCOptions = CompileGHCOptions
  { bootstrapGhc :: Either Version FilePath,
    hadrianGhc :: Maybe (Either Version FilePath),
    jobs :: Maybe Int,
    buildConfig :: Maybe FilePath,
    patches :: Maybe (Either FilePath [URI]),
    crossTarget :: Maybe Text,
    ghcConfArgs :: [Text],
    setCompile :: Bool,
    overwriteVer :: Maybe [VersionPattern],
    buildFlavour :: Maybe String,
    docs :: Maybe String,
    buildSystem :: Maybe BuildSystem,
    ghcIsolateDir :: Maybe FilePath,
    gitRef :: Maybe String,
    ghcInstallTargets :: Maybe Text
  }

-- | Compile a GHC version (or the git ref in the options) from source.
compileGHC :: CompileGHCOptions -> ListResult -> GHCup (Either String ())
compileGHC o ListResult {..} = do
  appstate <- ask
  r <-
    runResourceT . runE @CompileGHCErrors $ do
      AppState {ghcupInfo = GHCupInfo {_ghcupDownloads = dls}} <- ask
      ghcVer <- case gitRef o of
        Just ref' -> pure (GHC.GitDist (GitBranch ref' Nothing))
        Nothing -> do
          let vi = getVersionMetadata (mkTVer lVer) ghc dls
          preMessages vi
          pure (GHC.SourceDist lVer)
      targetVer <-
        liftE $
          GHC.compileGHC
            ghcVer
            (crossTarget o)
            (overwriteVer o)
            (bootstrapGhc o)
            (hadrianGhc o)
            (jobs o)
            (buildConfig o)
            (patches o)
            (T.unpack <$> ghcConfArgs o)
            (buildFlavour o)
            (buildSystem o)
            (maybe GHCupInternal IsolateDir (ghcIsolateDir o))
            (words . T.unpack <$> ghcInstallTargets o)
            (docs o)
      AppState {ghcupInfo = GHCupInfo {_ghcupDownloads = dls2}} <- ask
      when (setCompile o) (liftE . void $ setToolVersion ghc targetVer)
      pure (getVersionMetadata targetVer ghc dls2)
  case r of
    VRight vi -> do
      logInfo "GHC successfully compiled and installed"
      forM_ (_vmPostInstall =<< vi) logInfo
      pure (Right ())
    VLeft (V (AlreadyInstalled _ v)) ->
      pure (Left ("GHC ver " <> prettyShow v <> " already installed, remove it first to reinstall"))
    VLeft (V (DirNotEmpty fp)) -> pure (Left ("Install directory " <> fp <> " is not empty."))
    VLeft err@(V (BuildFailed tmpdir _)) -> pure (Left (buildFailed appstate (prettyHFError err) tmpdir))
    VLeft e -> pure (Left (prettyHFError e))

type CompileGHCErrors =
  '[ AlreadyInstalled,
     BuildFailed,
     DigestError,
     ContentLengthError,
     GPGError,
     DownloadFailed,
     GHCupSetError,
     NoDownload,
     NotFoundInPATH,
     PatchFailed,
     UnknownArchive,
     TarDirDoesNotExist,
     NotInstalled,
     DirNotEmpty,
     ArchiveResult,
     FileDoesNotExistError,
     HadrianNotFound,
     InvalidBuildConfig,
     ProcessError,
     CopyError,
     BuildFailed,
     UninstallFailed,
     MergeFileTreeError,
     URIParseError,
     ParseError,
     FileAlreadyExistsError,
     NoInstallInfo,
     MalformedInstallInfo
   ]

-- | The options of "Compile HLS".
data CompileHLSOptions = CompileHLSOptions
  { hlsJobs :: Maybe Int,
    hlsSetCompile :: Bool,
    updateCabal :: Bool,
    hlsOverwriteVer :: Maybe [VersionPattern],
    hlsIsolateDir :: Maybe FilePath,
    cabalProject :: Maybe (Either FilePath URI),
    cabalProjectLocal :: Maybe URI,
    hlsPatches :: Maybe (Either FilePath [URI]),
    targetGHCs :: [ToolVersion],
    cabalArgs :: [Text],
    hlsGitRef :: Maybe String
  }

-- | Compile an HLS version (or the git ref in the options) from source.
compileHLS :: CompileHLSOptions -> ListResult -> GHCup (Either String ())
compileHLS o ListResult {..} = do
  appstate <- ask
  r <-
    runResourceT . runE @CompileHLSErrors $ do
      AppState {ghcupInfo = GHCupInfo {_ghcupDownloads = dls}} <- ask
      hlsVer <- case hlsGitRef o of
        Just ref' -> pure (HLS.GitDist (GitBranch ref' Nothing))
        Nothing -> do
          let vi = getVersionMetadata (mkTVer lVer) hls dls
          preMessages vi
          pure (HLS.SourceDist lVer)
      ghcs' <- liftE $ forM (targetGHCs o) (\ghc' -> resolveVersion (Just ghc') GStrict ghc)
      ghcs <- forM ghcs' $ \TargetVersionReq {..} -> do
        when (isJust $ _tvTarget _tvqTargetVer) $ fail "Cannot compile HLS for a cross GHC"
        pure (_tvVersion _tvqTargetVer)
      targetVer <-
        liftE $
          HLS.compileHLS
            hlsVer
            ghcs
            (hlsJobs o)
            (hlsOverwriteVer o)
            (maybe GHCupInternal IsolateDir (hlsIsolateDir o))
            (cabalProject o)
            (cabalProjectLocal o)
            (updateCabal o)
            (hlsPatches o)
            (cabalArgs o)
      AppState {ghcupInfo = GHCupInfo {_ghcupDownloads = dls2}} <- ask
      when (hlsSetCompile o) (liftE . void $ setToolVersion hls (mkTVer targetVer))
      pure (getVersionMetadata (mkTVer targetVer) hls dls2)
  case r of
    VRight vi -> do
      logInfo "HLS successfully compiled and installed"
      forM_ (_vmPostInstall =<< vi) logInfo
      pure (Right ())
    VLeft err@(V (BuildFailed tmpdir _)) -> pure (Left (buildFailed appstate (prettyHFError err) tmpdir))
    VLeft e -> pure (Left (prettyHFError e))

type CompileHLSErrors =
  '[ AlreadyInstalled,
     BuildFailed,
     DigestError,
     ContentLengthError,
     GPGError,
     DownloadFailed,
     GHCupSetError,
     NoDownload,
     NotFoundInPATH,
     PatchFailed,
     UnknownArchive,
     TarDirDoesNotExist,
     TagNotFound,
     DayNotFound,
     NextVerNotFound,
     NoToolVersionSet,
     NotInstalled,
     DirNotEmpty,
     ArchiveResult,
     UninstallFailed,
     MergeFileTreeError,
     URIParseError,
     ParseError
   ]

-- | Log a version's messages for before installing and compiling, and give
-- the user time to read them.
preMessages :: (MonadReader AppState m, MonadIO m) => Maybe VersionMetadata -> Excepts es m ()
preMessages vi = do
  forM_ (_vmPreInstall =<< vi) $ \msg -> do
    lift $ logWarn msg
    lift $ logWarn "...waiting for 5 seconds, you can still abort..."
    liftIO $ threadDelay 5000000
  forM_ (_vmPreCompile =<< vi) $ \msg -> do
    lift $ logInfo msg
    lift $ logInfo "...waiting for 5 seconds, you can still abort..."
    liftIO $ threadDelay 5000000

-- | The error of a failed build, and where to look for why (as the TUI says).
buildFailed :: AppState -> String -> FilePath -> String
buildFailed appstate err tmpdir = case keepDirs (settings appstate) of
  Never -> err
  _ ->
    err
      <> "\nCheck the logs at "
      <> fromGHCupPath (logsDir (dirs appstate))
      <> " and the build directory "
      <> tmpdir
      <> " for more clues.\nMake sure to clean up "
      <> tmpdir
      <> " afterwards."
