{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

-- | The window: ghcup's TUI as a GUI. Tools on the left, the selected
-- tool's versions on the right with what can be done with each, and the
-- progress and log of what ghcup is doing at the bottom. The work is done by
-- the ghcup library (see "GHCup.GUI.Actions"), one job at a time, in the
-- background.
module GHCup.GUI.App
  ( LogBuffer,
    newLogBuffer,
    appendLog,
    app,
  )
where

import Control.Concurrent (forkIO)
import Control.Exception (IOException, try)
import Control.Monad (forM_, join, unless, void, when)
import Control.Monad.Reader (runReaderT)
import Data.Char (toLower)
import Data.IORef
import Data.List (findIndex, intercalate, sort)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust, isNothing, listToMaybe)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T
import Data.Versions (Version, prettyVer)
import GHCup.Command.List (ListResult (..), RevTag (..), ToolListResult)
import GHCup.GUI.Actions
import GHCup.GUI.Forms
import GHCup.GUI.Setup
import GHCup.Prelude (decUTF8Safe)
import GHCup.Prelude.Logger (logError)
import GHCup.Query.Metadata (getDownloadInfo)
import GHCup.Types
  ( AppState (..),
    DownloadInfo (..),
    Tag (..),
    TargetVersion (..),
    TargetVersionReq (..),
    Tool (..),
    ToolDescription (..),
    ghc,
    hls,
    isCompat,
    tagToString,
  )
import Godot.ImUI
import GHC.Clock (getMonotonicTime)
import System.Directory (getFileSize)
import System.Exit (ExitCode (..))
import System.FilePath (takeFileName, (<.>))
import System.Process (readProcessWithExitCode)
import URI.ByteString (serializeURIRef')

--------------------------------------------------------------------------------
-- ghcup's log

-- | What ghcup logged, last lines first (at most 'logLines'), what it is
-- doing, and the sizes of what it downloaded.
data LogBuffer = LogBuffer (IORef (Seq.Seq Text)) (IORef Step) (IORef (Map.Map String (Maybe Integer)))

-- | What ghcup is doing, from its last log line.
data Step = Step
  { -- | the line
    stepText :: String,
    -- | what it is downloading, and the file it downloads to, if it is
    -- downloading
    stepDownload :: Maybe (String, FilePath)
  }

newLogBuffer :: IO LogBuffer
newLogBuffer = LogBuffer <$> newIORef Seq.empty <*> newIORef (Step "" Nothing) <*> newIORef Map.empty

-- | ghcup's console output. (Called from the jobs' threads.)
appendLog :: LogBuffer -> Text -> IO ()
appendLog (LogBuffer ref step sizes) t = do
  let t' = stripANSI t
  atomicModifyIORef' ref (\s -> (Seq.take logLines (t' Seq.<| s), ()))
  forM_ (listToMaybe (reverse (filter (not . T.null . T.strip) (T.lines t')))) $ \line -> do
    -- "[ Info  ] downloading: <url> as file <path>"
    let message = T.strip (T.drop 1 (T.dropWhile (/= ']') line))
        download = case T.breakOn " as file " message of
          (before, path)
            | Just url <- T.stripPrefix "downloading: " before,
              not (T.null path) ->
                Just (T.unpack url, T.unpack (T.drop 9 path))
          _ -> Nothing
    atomicWriteIORef step (Step (T.unpack message) download)
    -- Ask the server for the size, to show how far the download got.
    forM_ download $ \(url, _) -> do
      new <- atomicModifyIORef' sizes (\m -> if Map.member url m then (m, False) else (Map.insert url Nothing m, True))
      when new $ void $ forkIO $ do
        size <- contentLength url
        atomicModifyIORef' sizes (\m -> (Map.insert url size m, ()))

-- | The size of a download, from an HTTP HEAD request (with curl, which ghcup
-- downloads with by default).
contentLength :: String -> IO (Maybe Integer)
contentLength url = do
  r <- try (readProcessWithExitCode "curl" ["-sIL", "--max-time", "20", url] "")
  pure $ case r of
    Right (ExitSuccess, out, _) ->
      -- After redirects, the last response's length.
      listToMaybe . reverse $
        [ n
        | l <- lines out,
          let (name, value) = break (== ':') l,
          map toLower name == "content-length",
          [(n, _)] <- [reads (drop 1 value)]
        ]
    Right _ -> Nothing
    Left (_ :: IOException) -> Nothing

-- | The size of a download, if known.
downloadSize :: LogBuffer -> String -> IO (Maybe Integer)
downloadSize (LogBuffer _ _ sizes) url = join . Map.lookup url <$> readIORef sizes

currentStep :: LogBuffer -> IO Step
currentStep (LogBuffer _ step _) = readIORef step

-- | Without terminal escape sequences (ghcup's log levels are coloured).
stripANSI :: Text -> Text
stripANSI t = case T.breakOn "\ESC[" t of
  (before, rest)
    | T.null rest -> before
    | otherwise -> before <> stripANSI (T.drop 1 (T.dropWhile (\c -> c < '@' || c > '~') (T.drop 2 rest)))

logLines :: Int
logLines = 400

-- | The log, newest lines first (so that the latest is in view).
logText :: LogBuffer -> IO String
logText (LogBuffer ref _ _) = T.unpack . T.concat . map (`T.snoc` '\n') . concatMap (reverse . T.lines) . foldr (:) [] <$> readIORef ref

--------------------------------------------------------------------------------
-- Jobs

-- | What is shown: ghcup's state, and every tool's versions.
data Model = Model
  { mState :: AppState,
    mTools :: ToolListResult,
    -- | the size of each version's bindist, by its file name (to show the
    -- progress of downloading it)
    mSizes :: Map.Map FilePath Integer
  }

model' :: AppState -> ToolListResult -> Model
model' st tools = Model st tools sizes
  where
    sizes =
      Map.fromList
        [ (fromMaybe (takeFileName (T.unpack _dlUri)) _dlOutput, size)
        | (tool, (_, results)) <- Map.toList tools,
          lr <- results,
          let tvr = TargetVersionReq (TargetVersion (lCross lr) (lVer lr)) (Just (fst (lRev lr))),
          Right (_, DownloadInfo {..}) <- [getDownloadInfo tool tvr (ghcupInfo st) (pfreq st)],
          Just size <- [_dlCSize]
        ]

-- | What a job did, for the user.
data Outcome
  = -- | nothing to report (loading the list)
    Quiet
  | Succeeded String
  | Error String
  | -- | setting the version needs it installed first
    NotInstalledYet Target

-- | A version, with its tool.
data Target = Target {tTool :: Tool, tDesc :: Maybe ToolDescription, tVersion :: ListResult}

-- | A finished job: its number, what it did, and the list after it.
data Result = Result {rJob :: Int, rOutcome :: Outcome, rModel :: Either String Model}

-- | ghcup's state and the list, from scratch.
startup :: LogBuffer -> IO (Either String Model)
startup logBuf =
  initialAppState (appendLog logBuf) >>= \case
    Left e -> pure (Left e)
    Right st -> fmap (model' st) <$> runReaderT loadVersions st

-- | The list again after a change (with the download metadata fetched again,
-- if ghcup's cache of it has expired).
reload :: Model -> IO (Either String Model)
reload m =
  reloadMetadata (mState m) >>= \case
    Left e -> pure (Left e)
    Right st -> fmap (model' st) <$> runReaderT loadVersions st

-- | Run a ghcup action as job n, then reload the list (keeping the one
-- before if that fails).
runJob :: Int -> Model -> GHCup Outcome -> IO Result
runJob n m action = do
  did <- runReaderT action (mState m)
  reload m >>= \case
    Right m' -> pure (Result n did (Right m'))
    Left e -> do
      runReaderT (logError (T.pack ("Could not reload the list: " ++ e))) (mState m)
      pure . Result n (case did of Quiet -> Error e; _ -> did) $ Right m

outcome :: String -> GHCup (Either String ()) -> GHCup Outcome
outcome done = fmap (either Error (const (Succeeded done)))

--------------------------------------------------------------------------------
-- The window

-- | A form open for a version.
data OpenForm = InstallForm Target | CompileGHCForm Target | CompileHLSForm Target

-- | The window, given ghcup's log and what restarts ghcup-gui (after 'quit').
app :: LogBuffer -> IO () -> UI ()
app logBuf restart = do
  (job, startJob, cancelJob) <- asyncState (Just (Result 0 Quiet <$> startup logBuf))
  (nextJob, setNextJob) <- state (1 :: Int)
  (seen, setSeen) <- state (0 :: Int)
  (running, setRunning) <- state ""
  (status, setStatus) <- state ""
  (selected, setSelected) <- state ghc
  (showAll, setShowAll) <- state True
  (failure, setFailure) <- state (Nothing :: Maybe String)
  (pending, setPending) <- state (Nothing :: Maybe Target)
  (details, setDetails) <- state False
  (legend, setLegend) <- state False
  (form, setForm) <- state (Nothing :: Maybe OpenForm)
  (startedAt, setStartedAt) <- state (0 :: Double)
  installFields <- state noFields
  ghcFields <- state noFields
  hlsFields <- state noFields

  let latest = latestValue job
      model = either (const Nothing) Just . rModel =<< latest
      busy = case job of
        Running _ -> True
        _ -> False
      -- Start a job (one at a time: the buttons are disabled while one runs).
      start :: String -> GHCup Outcome -> UI ()
      start title action = forM_ model $ \m -> liftIO $ do
        setStartedAt =<< getMonotonicTime
        setRunning title
        setNextJob (nextJob + 1)
        startJob (runJob nextJob m action)
      retry = liftIO $ do
        setStartedAt =<< getMonotonicTime
        setRunning "Loading"
        setNextJob (nextJob + 1)
        startJob (Result nextJob Quiet <$> startup logBuf)
      submitted :: (a -> UI ()) -> FormResult a -> UI ()
      submitted go = \case
        Editing -> pure ()
        Cancelled' -> liftIO (setForm Nothing)
        Submitted o -> liftIO (setForm Nothing) >> go o

  -- What the job that finished did (once).
  case job of
    Done r | rJob r > seen -> liftIO $ do
      setSeen (rJob r)
      case rOutcome r of
        Quiet -> setStatus ""
        Succeeded s -> setStatus s
        Error e -> setStatus ("Failed: " ++ firstLine e) >> setFailure (Just e)
        NotInstalledYet t -> setStatus "" >> setPending (Just t)
    Failed e _ | nextJob - 1 > seen -> liftIO $ do
      setSeen (nextJob - 1)
      setStatus "Failed"
      setFailure (Just (show e))
    _ -> pure ()

  -- The menus.
  menuBar $ do
    menu "File" $ do
      refresh <- menuItemWith menuItemOpts {enabled = not busy && isJust model} "Refresh"
      when refresh (start "Refreshing" (pure Quiet))
      again <- menuItemWith menuItemOpts {enabled = not busy} "Restart"
      when again (liftIO restart >> quit)
      menuSeparator
      close <- menuItem "Quit"
      when close quit
    menu "View" $ do
      showAll' <- menuCheck "Show all versions" showAll
      when (showAll' /= showAll) (liftIO (setShowAll showAll'))
    menu "Help" $ do
      l <- menuItem "Legend"
      when l (liftIO (setLegend True))

  -- The tools and their versions.
  case (model, latest) of
    (Just m, _) -> mainView m selected setSelected showAll busy start setDetails setForm
    (Nothing, Just Result {rModel = Left e}) | not busy -> startupError e retry
    (Nothing, _) | not busy, Failed e _ <- job -> startupError (show e) retry
    _ -> vboxWith vboxOpts {expand = True} (center (label "Loading the list of tools..."))

  -- The status bar and ghcup's log.
  separator
  if busy
    then do
      now <- liftIO getMonotonicTime
      step <- liftIO (currentStep logBuf)
      downloaded <- liftIO (maybe (pure Nothing) (downloadedSize . snd) (stepDownload step))
      -- The size of the download: from the metadata, or else the server.
      let file = takeFileName . snd <$> stepDownload step
      total <- case (model, file) of
        (Just m, Just f) | Just size <- Map.lookup f (mSizes m) -> pure (Just size)
        _ -> liftIO (maybe (pure Nothing) (downloadSize logBuf . fst) (stepDownload step))
      hbox $ do
        label (running ++ "...")
        spacer
        label (duration (now - startedAt))
        cancel <- buttonWith buttonOpts {tooltip = "Stop what ghcup is doing"} "Cancel"
        when cancel $ liftIO $ do
          cancelJob
          setSeen (nextJob - 1)
          setStatus (running ++ ": cancelled")
      -- How far a download got, if its size is known; otherwise just that
      -- something is going on.
      case (downloaded, total) of
        (Just bytes, Just size) | size > 0 -> do
          progressBar (min 1 (fromIntegral bytes / fromIntegral size))
          label ("Downloading " ++ fromMaybe "" file ++ ": " ++ megabytes bytes ++ " of " ++ megabytes size)
        (Just bytes, _) -> do
          progressBarWith progressBarOpts {indeterminate = True} 0
          label ("Downloading " ++ fromMaybe "" file ++ ": " ++ megabytes bytes)
        _ -> do
          progressBarWith progressBarOpts {indeterminate = True} 0
          label (shorten 120 (stepText step))
    else hbox (label status)
  collapsible "Log (newest first)" $ do
    text <- liftIO (logText logBuf)
    _ <- textAreaWith textAreaOpts {minSize = (0, 130)} text
    pure ()

  -- Dialogs.
  forM_ failure $ \e -> do
    ok <- messageBox "Error" e
    when ok (liftIO (setFailure Nothing))
  forM_ pending $ \t -> do
    answer <-
      confirm
        "Not installed"
        ("This version of " ++ toolName (tTool t) ++ " is not installed.\nWould you like to install it first?")
    forM_ answer $ \yes -> do
      liftIO (setPending Nothing)
      when yes $
        start
          ("Installing and setting " ++ targetName t)
          (outcome ("Installed and set " ++ targetName t) (installVersion defaultInstallOptions {instSet = True} (tTool t) (tDesc t) (tVersion t)))
  when details $ forM_ model $ \m -> do
    closed <- toolDetails m selected start
    when closed (liftIO (setDetails False))
  when legend $ do
    closed <- legendDialog
    when closed (liftIO (setLegend False))
  forM_ form $ \case
    InstallForm t -> installForm (targetName t) installFields >>= submitted (\o -> start ("Installing " ++ targetName t) (outcome ("Installed " ++ targetName t) (installVersion o (tTool t) (tDesc t) (tVersion t))))
    CompileGHCForm t ->
      compileGHCForm (targetName t) (installedGHCs model) ghcFields
        >>= submitted (\o -> start ("Compiling " ++ targetName t) (outcome ("Compiled " ++ targetName t) (compileGHC o (tVersion t))))
    CompileHLSForm t ->
      compileHLSForm (targetName t) (installedGHCs model) hlsFields
        >>= submitted (\o -> start ("Compiling " ++ targetName t) (outcome ("Compiled " ++ targetName t) (compileHLS o (tVersion t))))

-- | Shown when ghcup could not be set up (e.g. no network for the metadata).
startupError :: String -> UI () -> UI ()
startupError e retry = vboxWith vboxOpts {expand = True} $ center $ vbox $ do
  heading "GHCup could not start"
  paragraph e
  hbox $ do
    spacer
    again <- button "Retry"
    when again retry

-- | The tools on the left, and the selected one's versions on the right.
mainView ::
  Model ->
  Tool ->
  (Tool -> IO ()) ->
  Bool ->
  Bool ->
  (String -> GHCup Outcome -> UI ()) ->
  (Bool -> IO ()) ->
  (Maybe OpenForm -> IO ()) ->
  UI ()
mainView m selected setSelected showAll busy start setDetails setForm = do
  let tools = Map.toList (mTools m)
      ix = fromMaybe 0 (findIndex ((== selected) . fst) tools)
  unless (null tools) $ hboxWith hboxOpts {expand = True} $ do
    vboxWith vboxOpts {minSize = (160, 0)} $ do
      label "Tools"
      ix' <- listBoxWith listBoxOpts {expand = True} (map (toolName . fst) tools) ix
      when (ix' /= ix) (liftIO (setSelected (fst (tools !! ix'))))
    toolView (tools !! ix)
  where
    toolView (tool, (td, results)) = vboxWith vboxOpts {expand = True} $ do
      let recommended = listToMaybe [lr | lr <- results, Recommended `elem` lTag lr]
      hbox $ do
        heading (toolName tool ++ " versions")
        spacer
        forM_ recommended $ \lr -> do
          let t = Target tool td lr
          go <-
            buttonWith
              buttonOpts
                { enabled = not busy && not (lSet lr),
                  tooltip =
                    if lSet lr
                      then "The recommended version, " ++ versionText lr ++ ", is in use"
                      else "Install and set the recommended version, " ++ versionText lr
                }
              "Install recommended"
          when go $
            start
              ("Installing " ++ targetName t)
              (outcome ("Installed and set " ++ targetName t) (installVersion defaultInstallOptions {instSet = True} tool td lr))
        more <- button "Details"
        when more (liftIO (setDetails True))
      forM_ td (paragraph . T.unpack . _toolDescription)
      scrollWith scrollOpts {expand = True} $
        gridWith gridOpts {expand = True} 5 $ do
          forM_ ["", "Version", "Tags", "Notes", ""] label
          forM_ (filter (visible showAll) (reverse results)) $ \lr ->
            versionRow (Target tool td lr)
    versionRow t@(Target tool td lr@ListResult {..}) = do
      richTextWith richTextOpts {minSize = (32, 0)} (mark lr)
      richTextWith richTextOpts {minSize = (150, 0)} (versionMarkup lr)
      richTextWith richTextOpts {minSize = (260, 0)} (tagsMarkup lr)
      richTextWith richTextOpts {minSize = (180, 0)} (notesMarkup lr)
      hbox $ do
        let name = targetName t
            act label' tip = buttonWith buttonOpts {enabled = not busy, tooltip = tip} label'
        if tool == Tool "ghcup"
          then unless lInstalled $ do
            up <- act "Upgrade" "Upgrade GHCup to this version"
            when up (start ("Upgrading GHCup to " ++ versionText lr) (outcome ("Upgraded GHCup to " ++ versionText lr) (installVersion defaultInstallOptions tool td lr)))
          else do
            unless lInstalled $ do
              i <-
                buttonWith
                  buttonOpts
                    { enabled = not busy && not lNoBindist,
                      tooltip = if lNoBindist then "Not available for this platform" else "Install this version"
                    }
                  "Install"
              when i (start ("Installing " ++ name) (outcome ("Installed " ++ name) (installVersion defaultInstallOptions tool td lr)))
            when (lInstalled && not lSet) $ do
              s <- act "Set" "Make this the version in use"
              when s (start ("Setting " ++ name) (setOutcome t))
            when lInstalled $ do
              u <- act "Uninstall" "Remove this version"
              when u (start ("Removing " ++ name) (outcome ("Removed " ++ name) (removeVersion tool lr)))
        menuWith menuOpts {tooltip = "More actions"} "More" $ do
          let item = menuItemWith menuItemOpts {enabled = not busy}
          c <- item "Changelog"
          when c (start ("Opening the changelog of " ++ name) (outcome ("Opened the changelog of " ++ name) (changelog tool lr)))
          unless (tool == Tool "ghcup") $ do
            unless lSet $ do
              s <- item "Install and set"
              when s (start ("Installing and setting " ++ name) (outcome ("Installed and set " ++ name) (installVersion defaultInstallOptions {instSet = True} tool td lr)))
            o <- item "Install with options..."
            when o (liftIO (setForm (Just (InstallForm t))))
          when (tool == ghc) $ do
            g <- item "Compile from source..."
            when g (liftIO (setForm (Just (CompileGHCForm t))))
          when (tool == hls) $ do
            h <- item "Compile from source..."
            when h (liftIO (setForm (Just (CompileHLSForm t))))

-- | Set a version; if it is not installed, ask to install it first.
setOutcome :: Target -> GHCup Outcome
setOutcome t =
  setVersion (tTool t) (tVersion t) >>= \case
    SetDone -> pure (Succeeded ("Set " ++ targetName t))
    SetNotInstalled -> pure (NotInstalledYet t)
    SetFailed e -> pure (Error e)

-- | The TUI's tool details.
toolDetails :: Model -> Tool -> (String -> GHCup Outcome -> UI ()) -> UI Bool
toolDetails m tool start = do
  r <- dialog (toolName tool ++ " details") $ do
    case fst <$> Map.lookup tool (mTools m) of
      Just (Just ToolDescription {..}) -> grid 2 $ do
        let field name value = label name >> paragraph (fromMaybe "-- Not specified --" value)
            linkField name uri = do
              label name
              case uri of
                Nothing -> label "-- Not specified --"
                Just u -> do
                  let url = T.unpack (decUTF8Safe (serializeURIRef' u))
                  go <- link url
                  when go (start ("Opening " ++ url) (outcome ("Opened " ++ url) (openURL url)))
        field "Description:" (Just (T.unpack _toolDescription))
        linkField "Homepage:" _toolHomepage
        linkField "Repository:" _toolRepository
        field "Author:" _toolAuthor
        field "Maintainer:" _toolMaintainer
        field "Contact:" _toolContact
        field "License:" _toolLicense
      _ -> label "No details available"
    hbox (spacer >> button "Close")
  pure (r /= Just False)

-- | The TUI's tutorial: what the marks and tags mean.
legendDialog :: UI Bool
legendDialog = do
  r <- dialog "Legend" $ do
    paragraph "GHCup is a distribution channel for Haskell's tools."
    separator
    grid 2 $ do
      richText (green "✔✔") >> label "the tool is installed and in use"
      richText (green "✓") >> label "the tool is installed but not in use"
      richText (red "✗") >> label "the tool isn't installed"
    separator
    grid 2 $ do
      richText (green "recommended") >> paragraph "based on community adoption, known bugs, etc. So it makes this version the least experimental."
      richText (yellow "latest") >> paragraph "the latest distributed version of the tool"
      richText (green "hls-powered") >> paragraph "the compiler versions supported by the HLS in use (✔✔)"
      label "base-X.Y.Z.W" >> paragraph "the version of the base package that comes with the GHC version"
    hbox (spacer >> button "Close")
  pure (r /= Just False)

--------------------------------------------------------------------------------
-- Showing versions

-- | The versions the TUI shows: installed ones, and old ones only when all
-- are shown.
visible :: Bool -> ListResult -> Bool
visible showAll lr = lInstalled lr || showAll || Old `notElem` lTag lr

toolName :: Tool -> String
toolName = \case
  Tool "cabal" -> "cabal"
  Tool "ghc" -> "GHC"
  Tool "ghcup" -> "GHCup"
  Tool "hls" -> "HLS"
  Tool "stack" -> "Stack"
  Tool t -> t

targetName :: Target -> String
targetName t = toolName (tTool t) ++ " " ++ versionText (tVersion t)

-- | The version as the TUI shows it: with the cross target, and with the
-- revision if it is an update.
versionText :: ListResult -> String
versionText ListResult {..} =
  maybe "" (\c -> T.unpack c ++ "-") lCross ++ T.unpack (prettyVer lVer) ++ rev
  where
    rev = case lRev of
      (r, RevUpdate) -> "-r" ++ show r
      (r, RevOutdated) -> "-r" ++ show r
      _ -> ""

installedGHCs :: Maybe Model -> [Version]
installedGHCs model =
  [ lVer lr
  | Just m <- [model],
    Just (_, results) <- [Map.lookup ghc (mTools m)],
    lr <- results,
    lInstalled lr,
    isNothing (lCross lr)
  ]

mark :: ListResult -> String
mark lr
  | lSet lr = green "✔✔"
  | lInstalled lr = green "✓"
  | otherwise = red "✗"

versionMarkup :: ListResult -> String
versionMarkup lr@ListResult {..} = dim (hooray (escape (versionText lr)))
  where
    dim s = if lNoBindist && not lInstalled then "[color=#808080]" ++ s ++ "[/color]" else s
    hooray s
      | Latest `elem` lTag && not lInstalled && not lNoBindist = "[b]" ++ s ++ "[/b]"
      | (_, RevUpdate) <- lRev = "[b]" ++ s ++ "[/b]"
      | otherwise = s

tagsMarkup :: ListResult -> String
tagsMarkup lr = intercalate ", " [t | Just t <- map tag (sort (lTag lr))]
  where
    tag = \case
      Recommended -> Just (green "recommended")
      Latest -> Just (yellow "latest")
      Prerelease -> Just (red "prerelease")
      Nightly -> Just (red "nightly")
      LatestPrerelease -> Just (red "latest-prerelease")
      LatestNightly -> Just (red "latest-nightly")
      Experimental -> Just (red "experimental")
      GHCCompat _ -> Nothing
      Old -> Nothing
      t -> Just (escape (tagToString t))

notesMarkup :: ListResult -> String
notesMarkup ListResult {..} =
  intercalate ", " $
    [green "hls-powered" | hlsPowered]
      ++ [cyan "stray" | lStray]
      ++ [cyan (show d) | Just d <- [lReleaseDay]]
      ++ [escape (tagToString t) | t <- lTag, isCompat t]

green, yellow, red, cyan :: String -> String
green = colour "#7ec699"
yellow = colour "#e5c07b"
red = colour "#e06c75"
cyan = colour "#56b6c2"

colour :: String -> String -> String
colour c s = "[color=" ++ c ++ "]" ++ s ++ "[/color]"

-- | Text shown as BBCode.
escape :: String -> String
escape = concatMap (\c -> if c == '[' then "[lb]" else [c])

firstLine :: String -> String
firstLine = takeWhile (/= '\n')

shorten :: Int -> String -> String
shorten n s = if length s > n then take (n - 3) s ++ "..." else s

-- | How much of a download curl has written: to the file with @.tmp@, which
-- it renames when done.
downloadedSize :: FilePath -> IO (Maybe Integer)
downloadedSize path = do
  r <- try (getFileSize (path <.> "tmp"))
  case r of
    Right n -> pure (Just n)
    Left (_ :: IOException) -> either (\(_ :: IOException) -> Nothing) Just <$> try (getFileSize path)

megabytes :: Integer -> String
megabytes b = let tenths = (b * 10 + 524288) `div` 1048576 in show (tenths `div` 10) ++ "." ++ show (tenths `mod` 10) ++ " MB"

-- | Seconds as 1:05 or 12s.
duration :: Double -> String
duration secs =
  let s = floor secs :: Int
   in if s < 60 then show s ++ "s" else show (s `div` 60) ++ ":" ++ (if s `mod` 60 < 10 then "0" else "") ++ show (s `mod` 60)
