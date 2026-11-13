{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Setting up ghcup's 'AppState' as the @ghcup@ executable does: its
-- directories, the user's settings (config.yaml), logging, the platform and
-- the download metadata.
module GHCup.GUI.Setup
  ( initialAppState,
    reloadMetadata,
  )
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race_)
import Control.Monad.Reader (runReaderT)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Variant.Excepts
import GHCup.Download (getDownloadsF)
import GHCup.Errors
import GHCup.Prelude.Logger (initGHCupFileLogging)
import GHCup.Query.GHCupDirs (cleanupTrash, getAllDirs, ghcupConfigFile)
import GHCup.Query.System (platformRequest)
import GHCup.Setup (ensureDirectories)
import GHCup.Types

-- | ghcup's state, with its console output going to the given function.
-- (Its file output goes to ghcup's log file, as with the @ghcup@ executable.)
initialAppState :: (Text -> IO ()) -> IO (Either String AppState)
initialAppState consoleOut = do
  ds <- getAllDirs
  ensureDirectories ds
  userConf <-
    runE @'[JSONError] ghcupConfigFile >>= \case
      VRight r -> pure r
      VLeft e -> do
        consoleOut ("Error decoding config file: " <> T.pack (prettyHFError e) <> "\n")
        pure defaultUserSettings
  let s = fromUserSettings userConf
  logfile <- runReaderT initGHCupFileLogging ds
  let logger =
        LoggerConfig
          { lcPrintDebugLvl = Just (let Verbosity i = verbose s in i),
            consoleOutter = consoleOut,
            fileOutter = T.appendFile logfile,
            fancyColors = False
          }
  platform <- case platformOverride s of
    Just p -> pure (Right p)
    Nothing ->
      runReaderT (runE @'[NoCompatiblePlatform, NoCompatibleArch, DistroNotFound] platformRequest) logger
        >>= \case
          VRight p -> pure (Right p)
          VLeft e -> pure (Left (prettyHFError e))
  case platform of
    Left e -> pure (Left e)
    Right p -> do
      let lean = LeanAppState s ds defaultKeyBindings p logger
      info <- downloads lean p
      case info of
        Left e -> pure (Left e)
        Right gi -> do
          let st = AppState s ds defaultKeyBindings gi p logger
          -- Like the ghcup executable: empty ghcup's trash, for at most 5s.
          race_ (runReaderT cleanupTrash st) (threadDelay 5000000)
          pure (Right st)

-- | The state with the download metadata fetched again (if ghcup's cache of
-- it has expired, see @meta-cache@).
reloadMetadata :: AppState -> IO (Either String AppState)
reloadMetadata st@(AppState s ds kb _ p logger) =
  -- (AppState and LeanAppState have fields of the same names, so they are
  -- taken apart by position here.)
  fmap (\gi -> st {ghcupInfo = gi}) <$> downloads (LeanAppState s ds kb p logger) p

downloads :: LeanAppState -> PlatformRequest -> IO (Either String GHCupInfo)
downloads lean p =
  runReaderT
    ( runE
        @'[ ContentLengthError,
            DigestError,
            DistroNotFound,
            DownloadFailed,
            FileDoesNotExistError,
            GPGError,
            JSONError,
            NoCompatibleArch,
            NoCompatiblePlatform,
            NoDownload,
            ParseError,
            ProcessError,
            UnsupportedSetupCombo,
            StackPlatformDetectError,
            UnsupportedMetadataFormat
          ]
        (liftE (getDownloadsF p))
    )
    lean
    >>= \case
      VRight gi -> pure (Right gi)
      VLeft e -> pure (Left (prettyHFError e))

-- | The settings from the user's config file, with ghcup's defaults for the
-- rest (as the ghcup executable merges them, without command line options).
fromUserSettings :: UserSettings -> Settings
fromUserSettings UserSettings {..} =
  Settings
    { cache = fromMaybe (cache defaultSettings) uCache,
      metaCache = fromMaybe (metaCache defaultSettings) uMetaCache,
      metaMode = fromMaybe (metaMode defaultSettings) uMetaMode,
      noVerify = fromMaybe (noVerify defaultSettings) uNoVerify,
      keepDirs = fromMaybe (keepDirs defaultSettings) uKeepDirs,
      downloader = fromMaybe Curl uDownloader,
      verbose = fromMaybe (verbose defaultSettings) uVerbose,
      urlSource = maybe (urlSource defaultSettings) fromURLSource uUrlSource,
      noNetwork = fromMaybe (noNetwork defaultSettings) uNoNetwork,
      gpgSetting = fromMaybe (gpgSetting defaultSettings) uGPGSetting,
      noColor = True,
      platformOverride = maybe (platformOverride defaultSettings) Just uPlatformOverride,
      mirrors = fromMaybe (mirrors defaultSettings) uMirrors,
      defGHCConfOptions = fromMaybe (defGHCConfOptions defaultSettings) uDefGHCConfOptions,
      pager = fromMaybe (pager defaultSettings) uPager,
      guessVersion = fromMaybe (guessVersion defaultSettings) uGuessVersion,
      buildWrapper = uBuildWrapper
    }
