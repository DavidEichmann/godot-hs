{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | The machinery behind "Godot.ImUI", exposed for wrapping Godot controls
-- that "Godot.ImUI" does not cover yet.
--
-- The 'UI' function runs every frame, but Godot's controls are retained, so
-- each frame is reconciled against the controls created by the previous one:
-- the widgets a block of 'UI' code creates are matched, in order, with the
-- existing children of the enclosing container. A widget is reused if its
-- kind (and 'attrKey') matches, created if it is new, and freed if the block
-- no longer creates it. Properties are only sent to Godot when they change.
--
-- A new widget is typically a few lines:
--
-- @
-- import qualified Godot.Api.Class.LinkButton as LinkButton
--
-- link :: String -> UI Bool
-- link text = do
--   (w, b) <- 'widget' \"LinkButton\" 'natural' LinkButton.construct
--   'prop' w \"text\" text (\\t -> toGString t >>= LinkButton.setText b)
--   'fired' w \"pressed\"
-- @
module Godot.ImUI.Internal
  ( -- * The UI monad
    UI (..),
    Env (..),
    Scope (..),
    Attrs (..),
    defaultAttrs,
    withAttrs,
    getAttrs,
    afterFrame,
    state,
    stateRef,
    asyncState,
    AsyncState (..),
    latestValue,

    -- * Widgets
    Widget (..),
    Sizing (..),
    natural,
    widget,
    node,
    container,
    children,
    contentRoot,
    prop,
    forget,
    controlled,
    widgetState,
    fired,
    events,
    release,

    -- * Running
    WindowConfig (..),
    defaultWindow,
    Theme (..),
    runUI,
    runUIWith,
    quit,
  )
where

import Control.Concurrent (forkIO)
import Control.Concurrent.Async (Async, async, cancel, poll)
import Control.Exception (SomeException, finally)
import Control.Monad (unless, void, when)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Dynamic (Dynamic, Typeable, fromDynamic, toDyn)
import Data.IORef
import Data.Int (Int64)
import Data.List (isPrefixOf)
import qualified Data.Map.Strict as Map
import Godot.Api hiding (Theme)
import Godot.Api.Embed (Project (..), defaultProject, withGodot)
import qualified Godot.Api.Class.Control as Control
import qualified Godot.Api.Class.DisplayServer as DisplayServer
import qualified Godot.Api.Class.Engine as Engine
import qualified Godot.Api.Class.GodotInstance as GodotInstance
import qualified Godot.Api.Class.MarginContainer as MarginContainer
import qualified Godot.Api.Class.Node as Node
import qualified Godot.Api.Class.Object as Object
import qualified Godot.Api.Class.PanelContainer as PanelContainer
import qualified Godot.Api.Class.RefCounted as RefCounted
import qualified Godot.Api.Class.SceneTree as SceneTree
import qualified Godot.Api.Class.VBoxContainer as VBoxContainer
import Godot.Api.Internal (getInterface)
import qualified Godot.GDExtension as GDE
import Godot.ImUI.Theme (Theme (..), backgroundType, themeClearColor, themeResource)
import System.Environment (getProgName)

--------------------------------------------------------------------------------
-- The UI monad

-- | A description of (part of) the user interface. It is run every frame;
-- each widget in it shows itself and returns what the user did with it since
-- the previous frame. Use 'liftIO' to run arbitrary 'IO'.
newtype UI a = UI {unUI :: Env -> IO a}

instance Functor UI where
  fmap f (UI g) = UI (fmap f . g)

instance Applicative UI where
  pure a = UI (\_ -> pure a)
  UI f <*> UI a = UI (\env -> f env <*> a env)

instance Monad UI where
  UI a >>= f = UI (\env -> a env >>= \x -> unUI (f x) env)

instance MonadIO UI where
  liftIO io = UI (const io)

instance MonadFail UI where
  fail = liftIO . fail

data Env = Env
  { -- | the container being filled
    envScope :: !Scope,
    -- | the options of the widget being created
    envAttrs :: !Attrs,
    envTree :: !SceneTree,
    -- | what 'afterFrame' queued, last first
    envAfter :: !(IORef [IO ()])
  }

-- | The children of a container, while they are being reconciled.
data Scope = Scope
  { -- | the container
    scopeWidget :: !Widget,
    -- | does the container lay its children out left to right?
    scopeHorizontal :: !Bool,
    -- | last frame's children not reused yet, in tree order
    scopeOld :: !(IORef [Widget]),
    -- | this frame's children so far, last first
    scopeNew :: !(IORef [Widget]),
    scopeCount :: !(IORef Int64),
    -- | the number of 'state's created in the container so far this frame
    scopeStates :: !(IORef Int)
  }

-- | The options of a widget that 'widget' and 'node' apply to its node:
-- the @expand@, @minSize@, @tooltip@ and @key@ options of "Godot.ImUI".
-- Set them for a widget with 'withAttrs'; the widgets in a container start
-- from 'defaultAttrs'.
data Attrs = Attrs
  { attrExpand :: !Bool,
    attrMinSize :: !(Int, Int),
    attrTooltip :: !String,
    -- | distinguishes widgets of the same kind in a container
    attrKey :: !String
  }

defaultAttrs :: Attrs
defaultAttrs = Attrs False (0, 0) "" ""

-- | Create widgets with these options.
withAttrs :: Attrs -> UI a -> UI a
withAttrs attrs (UI body) = UI (\env -> body env {envAttrs = attrs})

getAttrs :: UI Attrs
getAttrs = UI (pure . envAttrs)

-- | A piece of state kept from frame to frame, like React's @useState@:
-- takes the initial value and returns the current value and a function to
-- set it. What it is set to is the value from the next frame on.
--
-- @
-- counter :: UI ()
-- counter = 'Godot.ImUI.hbox' $ do
--   (count, setCount) <- state (0 :: Int)
--   clicked <- 'Godot.ImUI.button' \"+1\"
--   when clicked (liftIO (setCount (count + 1)))
--   'Godot.ImUI.label' (show count)
-- @
--
-- The state belongs to the container it is created in (the 'Godot.ImUI.hbox'
-- above), and is matched from frame to frame by its position among the
-- states created in that container. So, as with React's hooks:
--
-- * Create a container's states in the same order every frame: not under
--   a condition that changes, unless they are in a container of their own.
-- * The state is lost when its container is no longer created, and starts
--   again from the initial value if it is created again. Use a container's
--   @key@ to keep the state of an item of a list with its item.
state :: (Typeable a) => a -> UI (a, a -> IO ())
state initial = do
  (ref, _) <- stateRef initial (\_ -> pure ())
  current <- liftIO (readIORef ref)
  pure (current, writeIORef ref)

-- | The state behind 'state', for building other kinds of state: takes the
-- initial value and what to do with the value when the state is dropped
-- (its container is no longer created, or no longer creates the state), and
-- returns the state's 'IORef' and whether it was created this frame. The
-- cleanup action is the one given when the state was created.
stateRef :: (Typeable a) => a -> (a -> IO ()) -> UI (IORef a, Bool)
stateRef initial cleanup = UI $ \env -> do
  let sc = envScope env
      w = scopeWidget sc
  i <- readIORef (scopeStates sc)
  writeIORef (scopeStates sc) (i + 1)
  props <- readIORef (wProps w)
  case Map.lookup (stateName i) props >>= fromDynamic of
    Just ref -> pure (ref, False)
    -- New, or of another type (the block changed): start again.
    Nothing -> do
      dropState w i
      ref <- newIORef initial
      modifyIORef'
        (wProps w)
        (Map.insert (stateName i) (toDyn ref) . Map.insert (cleanupName i) (toDyn (readIORef ref >>= cleanup)))
      pure (ref, True)

-- | Where some work running in the background is: see 'asyncState'.
data AsyncState a
  = -- | No work has finished (or none was started).
    None
  | -- | Work is running; the value of the last work that finished.
    Running {oldValue :: Maybe a}
  | -- | The last work that finished returned this.
    Done a
  | -- | The last work that finished threw this; the value of the work
    -- before.
    Failed SomeException (Maybe a)
  deriving (Show)

-- | The value of the last work that finished successfully, if any.
latestValue :: AsyncState a -> Maybe a
latestValue s = case s of
  None -> Nothing
  Running old -> old
  Done a -> Just a
  Failed _ old -> old

-- | Work that runs in the background, in its own thread, so that the UI
-- keeps going: like 'state', but for the result of an 'IO' action. Takes the
-- work to start when the state is created, and returns where the work is,
-- an action that starts other work (cancelling the work running, if any),
-- and an action that cancels the work running (keeping the last result).
--
-- @
-- (answer, start, cancel) <- asyncState Nothing
-- go <- 'Godot.ImUI.button' \"Ask\"
-- when go (liftIO (start (fetchAnswer question)))
-- 'Godot.ImUI.label' $ case answer of
--   'Running' _ -> \"Asking...\"
--   'Done' a -> a
--   'Failed' e _ -> \"Failed: \" ++ show e
--   'None' -> \"\"
-- @
--
-- The UI shows the result from the frame after the work finishes. The work
-- is cancelled (with an asynchronous exception) when the state is dropped,
-- like 'state' is: when its container is no longer created. The work must
-- not use Godot or the UI, which only work on the UI's thread; it returns
-- its result for the UI to show. Start and cancel work from the UI (e.g. in
-- 'liftIO' when a button is clicked). Link with @-threaded@, so that the work
-- runs alongside the UI.
asyncState :: (Typeable a) => Maybe (IO a) -> UI (AsyncState a, IO a -> IO (), IO ())
asyncState initial = do
  (ref, created) <- stateRef (Work Nothing None) cancelWork
  let stop = do
        w <- readIORef ref
        cancelWork w
        writeIORef ref w {workRunning = Nothing}
      start work = do
        stop
        a <- async work
        modifyIORef' ref (\w -> w {workRunning = Just a})
  when created $ liftIO (mapM_ start initial)
  st <- liftIO $ do
    w <- readIORef ref
    case workRunning w of
      Nothing -> pure (workSettled w)
      Just a ->
        poll a >>= \r -> case r of
          Nothing -> pure (Running (latestValue (workSettled w)))
          Just result -> do
            let finished = either (\e -> Failed e (latestValue (workSettled w))) Done result
            writeIORef ref (Work Nothing finished)
            pure finished
  pure (st, start, stop)

-- | What an 'asyncState' keeps: the work running, and where the last work
-- that finished got to.
data Work a = Work {workRunning :: Maybe (Async a), workSettled :: AsyncState a}

-- | Cancel the work running, without waiting for it to stop (work in a
-- foreign call only notices when the call returns).
cancelWork :: Work a -> IO ()
cancelWork w = mapM_ (void . forkIO . cancel) (workRunning w)

-- | An action that runs the given one once the whole UI function has run
-- for this frame, and the widgets it no longer creates are gone. (For
-- example to show a window only after the one it replaces is removed.)
afterFrame :: UI (IO () -> IO ())
afterFrame = UI (\env -> pure (\action -> modifyIORef' (envAfter env) (action :)))

--------------------------------------------------------------------------------
-- Widgets

-- | A retained Godot node (usually a control), with what was last sent to
-- it.
data Widget = Widget
  { wKind :: !String,
    wKey :: !String,
    wNode :: !Node,
    wChildren :: !(IORef [Widget]),
    -- | the last value of each 'prop', and each 'state'
    wProps :: !(IORef (Map.Map String Dynamic))
  }

newWidget :: String -> String -> Node -> IO Widget
newWidget kind key n = Widget kind key n <$> newIORef [] <*> newIORef Map.empty

-- | How a kind of widget is sized by default. The @expand@ and @minSize@
-- options override it.
data Sizing = Sizing
  { -- | take up spare horizontal space (e.g. a text field in a row)
    growsHorizontally :: !Bool,
    growsVertically :: !Bool,
    defaultMinSize :: !(Int, Int)
  }

-- | As small as the content allows.
natural :: Sizing
natural = Sizing False False (0, 0)

-- | This frame's next widget: last frame's control of the same kind if there
-- is one, otherwise a new one made by the given action. Returns the widget
-- (for 'prop', 'fired', ...) and its control.
--
-- The kind identifies the widget type and how it was set up; widgets of
-- different kinds are never reused for each other.
widget :: forall c. (Inherits c Control) => String -> Sizing -> IO c -> UI (Widget, c)
widget kind sizing create = do
  (w, c) <- node kind (upcast @Control <$> create)
  applyAttrs w c sizing
  pure (w, unsafeFromObjectPtr (toObjectPtr c))

-- | Like 'widget', for nodes that are not controls (popups, windows, ...):
-- they are not laid out by their container, so the sizing options do not
-- apply to them.
node :: forall c. (Inherits c Node) => String -> IO c -> UI (Widget, c)
node kind create = UI $ \env -> do
  let sc = envScope env
      key = attrKey (envAttrs env)
      parent = wNode (scopeWidget sc)
  old <- readIORef (scopeOld sc)
  n <- readIORef (scopeCount sc)
  w <- case break (\w -> wKind w == kind && wKey w == key) old of
    (skipped, w : rest) -> do
      writeIORef (scopeOld sc) (skipped ++ rest)
      unless (null skipped) $ Node.moveChild parent (wNode w) n
      pure w
    (_, []) -> do
      c <- upcast @Node <$> create
      w <- newWidget kind key c
      Node.addChild parent c False Node_INTERNAL_MODE_DISABLED
      unless (null old) $ Node.moveChild parent c n
      pure w
  modifyIORef' (scopeNew sc) (w :)
  writeIORef (scopeCount sc) (n + 1)
  pure (w, unsafeFromObjectPtr (toObjectPtr (wNode w)))

-- | Size and tooltip, from the widget's defaults and its options.
applyAttrs :: Widget -> Control -> Sizing -> UI ()
applyAttrs w c sizing = do
  attrs <- getAttrs
  let flags grows = if grows || attrExpand attrs then Control_SIZE_EXPAND_FILL else Control_SIZE_FILL
      minSz = if attrMinSize attrs == (0, 0) then defaultMinSize sizing else attrMinSize attrs
  prop w "imui:layout" (growsHorizontally sizing || attrExpand attrs, growsVertically sizing || attrExpand attrs, minSz) $ \_ -> do
    Control.setHSizeFlags c (flags (growsHorizontally sizing))
    Control.setVSizeFlags c (flags (growsVertically sizing))
    Control.setCustomMinimumSize c (Vector2 (fromIntegral (fst minSz)) (fromIntegral (snd minSz)))
  prop w "imui:tooltip" (attrTooltip attrs) (\t -> toGString t >>= Control.setTooltipText c)

-- | A widget whose children are the widgets created by the given block.
container ::
  forall c a.
  (Inherits c Control) =>
  String ->
  -- | lays its children out left to right?
  Bool ->
  Sizing ->
  IO c ->
  UI a ->
  UI a
container kind horizontal sizing create body = do
  (w, _ :: c) <- widget kind sizing create
  children horizontal w body

-- | Run a block, making the widgets it creates the children of the given
-- widget (replacing the ones it created last frame but not this time).
children :: Bool -> Widget -> UI a -> UI a
children horizontal w (UI body) = UI $ \env -> do
  let parent = wNode w
  sc <- Scope w horizontal <$> (newIORef =<< readIORef (wChildren w)) <*> newIORef [] <*> newIORef 0 <*> newIORef 0
  a <-
    body
      env
        { envScope = sc,
          envAttrs = defaultAttrs
        }
  unused <- readIORef (scopeOld sc)
  mapM_ dispose unused
  mapM_ (\u -> Node.removeChild parent (wNode u) >> Node.queueFree (wNode u)) unused
  writeIORef (wChildren w) . reverse =<< readIORef (scopeNew sc)
  -- Drop the states the block no longer creates.
  n <- readIORef (scopeStates sc)
  props <- readIORef (wProps w)
  let before = maybe 0 id (Map.lookup "states" props >>= fromDynamic) :: Int
  when (n /= before) $ do
    modifyIORef' (wProps w) (Map.insert "states" (toDyn n))
    mapM_ (dropState w) [n .. before - 1]
  pure a

stateName, cleanupName :: Int -> String
stateName i = "state#" ++ show i
cleanupName i = "cleanup#" ++ show i

-- | Forget a widget's state, running its cleanup.
dropState :: Widget -> Int -> IO ()
dropState w i = do
  props <- readIORef (wProps w)
  writeIORef (wProps w) (Map.delete (stateName i) (Map.delete (cleanupName i) props))
  sequence_ (Map.lookup (cleanupName i) props >>= fromDynamic :: Maybe (IO ()))

-- | Run the cleanups of the states of a widget that is going away, and of
-- the widgets in it: its children, and those it keeps in its 'widgetState'
-- (a dialog's content, a menu's popup, ...).
dispose :: Widget -> IO ()
dispose w = do
  props <- readIORef (wProps w)
  writeIORef (wProps w) (Map.filterWithKey (\k _ -> not ("cleanup#" `isPrefixOf` k)) props)
  sequence_ [c | (k, d) <- Map.toList props, "cleanup#" `isPrefixOf` k, Just c <- [fromDynamic d :: Maybe (IO ())]]
  mapM_ dispose =<< readIORef (wChildren w)
  mapM_ dispose [inner | d <- Map.elems props, Just inner <- [fromDynamic d]]

-- | Fill a window with a themed background, a margin and a vertical box,
-- and return the box as a widget whose 'children' are the window's content.
contentRoot :: Node -> IO Widget
contentRoot window = do
  background <- PanelContainer.construct
  Control.setThemeTypeVariation background =<< toStringName backgroundType
  Node.addChild window (upcast background) False Node_INTERNAL_MODE_DISABLED
  Control.setAnchorsAndOffsetsPreset background Control_PRESET_FULL_RECT Control_PRESET_MODE_MINSIZE 0
  margin <- MarginContainer.construct
  mapM_
    (\side -> toStringName ("margin_" ++ side) >>= \name -> Control.addThemeConstantOverride margin name 12)
    ["left", "top", "right", "bottom"]
  Node.addChild background (upcast margin) False Node_INTERNAL_MODE_DISABLED
  box <- VBoxContainer.construct
  Node.addChild margin (upcast box) False Node_INTERNAL_MODE_DISABLED
  newWidget "Root" "" (upcast box)

-- | Set a property of the control, unless it was last set to the same
-- value. The name only has to be unique within the widget.
prop :: (Eq v, Typeable v) => Widget -> String -> v -> (v -> IO ()) -> UI ()
prop w name v set = liftIO $ do
  props <- readIORef (wProps w)
  case Map.lookup name props >>= fromDynamic of
    Just old | old == v -> pure ()
    _ -> do
      set v
      modifyIORef' (wProps w) (Map.insert name (toDyn v))

-- | Forget what a 'prop' (or @\"value\"@, for 'controlled') was last set to,
-- so that it is sent again.
forget :: Widget -> String -> IO ()
forget w name = modifyIORef' (wProps w) (Map.delete name)

-- | Two-way binding for the value a control edits (its text, whether it is
-- checked, ...): given the application's current value and how to read and
-- write the control's, returns the value after the user's input.
--
-- If the user changed the control since the last frame, that is the new
-- value. Otherwise the control is made to show the application's value.
controlled :: (Eq v, Typeable v) => Widget -> v -> IO v -> (v -> IO ()) -> UI v
controlled w model get set = liftIO $ do
  shown <- get
  props <- readIORef (wProps w)
  let remember v = modifyIORef' (wProps w) (Map.insert "value" (toDyn v)) >> pure v
  case Map.lookup "value" props >>= fromDynamic of
    Just before | before /= shown -> remember shown -- edited by the user
    _
      | shown == model -> remember model
      -- The control may adjust what it is given (e.g. clamp it to a range).
      | otherwise -> set model >> get >>= remember

-- | Haskell-side state that lives as long as the widget, made by the given
-- action the first time. The name only has to be unique within the widget.
widgetState :: (Typeable s) => Widget -> String -> IO s -> UI s
widgetState w name create = liftIO $ do
  props <- readIORef (wProps w)
  case Map.lookup key props >>= fromDynamic of
    Just s -> pure s
    Nothing -> do
      s <- create
      modifyIORef' (wProps w) (Map.insert key (toDyn s))
      pure s
  where
    key = "state:" ++ name

-- | The times the node emitted this signal since this was last asked, oldest
-- first, each decoded from the signal's arguments by the given function
-- (which runs when the signal is emitted). Connects to the signal the first
-- time, so that always returns none.
events :: (Typeable e) => Widget -> String -> ([Variant] -> IO e) -> UI [e]
events w signal decode = do
  queue <- widgetState w ("signal:" ++ signal) $ do
    queue <- newIORef []
    name <- toStringName signal
    callback <- newCallable (\args -> decode args >>= \e -> modifyIORef' queue (e :))
    err <- Object.connect (wNode w) name callback 0
    when (err /= OK) $ fail ("Godot.ImUI: cannot connect to signal " ++ signal ++ " of a " ++ wKind w)
    pure queue
  liftIO (reverse <$> atomicModifyIORef' queue (\es -> ([], es)))

-- | Did the node emit this signal since the last time this was asked?
fired :: Widget -> String -> UI Bool
fired w signal = not . null <$> events w signal (\_ -> pure ())

-- | Give up a reference to a reference counted object, such as the ones
-- Godot's methods return. (godot-low does not count references itself.)
release :: (Inherits a RefCounted) => a -> IO ()
release object = do
  unused <- RefCounted.unreference object
  when unused $ do
    iface <- getInterface
    GDE.object_destroy iface (toObjectPtr object)

--------------------------------------------------------------------------------
-- Running

-- | The application's window.
data WindowConfig = WindowConfig
  { windowTitle :: String,
    -- | initial width and height, in pixels
    windowSize :: (Int, Int),
    -- | the look of the controls
    windowTheme :: Theme
  }

-- | A 640x480 window titled after the program, in the dark 'ModernDark'
-- theme.
defaultWindow :: WindowConfig
defaultWindow = WindowConfig {windowTitle = "", windowSize = (640, 480), windowTheme = ModernDark}

-- | Open a window and run the UI in it until it is closed (or 'quit' is
-- called).
--
-- The UI runs every frame, and keeps its state with 'state':
--
-- @
-- main :: IO ()
-- main = runUI defaultWindow $ do
--   (count, setCount) <- state (0 :: Int)
--   label (\"Count: \" ++ show count)
--   clicked <- button \"Increment\"
--   when clicked (liftIO (setCount (count + 1)))
-- @
--
-- The top-level widgets are stacked vertically. Call this from the main
-- thread, and only once: Godot cannot be started again in the same process.
runUI :: WindowConfig -> UI () -> IO ()
runUI = runUIWith []

-- | 'runUI' with extra Godot command line arguments (e.g. @--headless@).
runUIWith :: [String] -> WindowConfig -> UI () -> IO ()
runUIWith extraArgs win frame = do
  progName <- getProgName
  let title = if null (windowTitle win) then progName else windowTitle win
  -- No audio: a UI has no use for Godot's audio thread.
  withGodot (project title (windowSize win) (windowTheme win)) (["--audio-driver", "Dummy"] ++ extraArgs) $ \inst -> do
    (tree, root) <- setupRoot title
    -- 'children' replaces this scope with a fresh one for each frame.
    outside <- Scope root False <$> newIORef [] <*> newIORef [] <*> newIORef 0 <*> newIORef 0
    after <- newIORef []
    let env = Env {envScope = outside, envAttrs = defaultAttrs, envTree = tree, envAfter = after}
        loop = do
          unUI (children False root frame) env
          sequence_ . reverse =<< atomicModifyIORef' after (\queued -> ([], queued))
          stop <- GodotInstance.iteration inst
          unless stop loop
    loop `finally` dispose root

-- | Close the window and return from 'runUI' after this frame.
quit :: UI ()
quit = UI (\env -> SceneTree.quit (envTree env) 0)

-- | The scene tree and the widget holding the top-level UI.
setupRoot :: String -> IO (SceneTree, Widget)
setupRoot title = do
  Just mainLoop <- Engine.getMainLoop =<< Engine.singleton
  Just tree <- cast @SceneTree mainLoop
  Just window <- SceneTree.getRoot tree
  -- Through the display server rather than the Window node, which appends
  -- "(DEBUG)" to its title when Godot is a debug build.
  display <- DisplayServer.singleton
  titleString <- toGString title
  DisplayServer.windowSetTitle display titleString 0
  root <- contentRoot (upcast window)
  pure (tree, root)

-- | The Godot project: only settings and the theme, as the UI itself is all
-- created at runtime.
project :: String -> (Int, Int) -> Theme -> Project
project title (width, height) theme =
  defaultProject
    { projectSettings =
        [ "config/name=" ++ quoted title,
          -- One user data directory (shader cache etc.) for all godot-imui
          -- programs, rather than one per window title.
          "config/use_custom_user_dir=true",
          "config/custom_user_dir_name=\"godot-imui\"",
          -- Only redraw when something changed.
          "run/low_processor_mode=true",
          -- ... and poll at about 100 Hz in between (the default is 144 Hz).
          "run/low_processor_mode_sleep_usec=10000",
          "boot_splash/show_image=false",
          "",
          "[debug]",
          "file_logging/enable_file_logging=false",
          "file_logging/enable_file_logging.pc=false",
          "",
          "[rendering]",
          -- Godot's OpenGL renderer, which is plenty for 2D UI. It renders on
          -- the GPU that drives the desktop. Godot's default Vulkan renderer
          -- prefers a discrete GPU, and on hybrid graphics laptops (desktop
          -- on the integrated GPU) presenting its frames across GPUs can
          -- freeze the whole X server, especially while the window is moved
          -- or resized. It also starts faster and uses less memory.
          "renderer/rendering_method=\"gl_compatibility\"",
          "renderer/rendering_method.mobile=\"gl_compatibility\""
        ]
          ++ themeSettings
          ++ [ "",
               "[display]",
               -- Dialogs and menus are windows of their own.
               "window/subwindows/embed_subwindows=false",
               "window/size/viewport_width=" ++ show width,
               "window/size/viewport_height=" ++ show height
             ],
      projectFiles = [("theme.tres", resource) | Just resource <- [themeResource theme]]
    }
  where
    -- The look of the controls, unless it is Godot's default theme.
    themeSettings = case (themeResource theme, themeClearColor theme) of
      (Just _, Just background) ->
        [ -- The background, seen while the window is resized.
          "environment/defaults/default_clear_color=" ++ background,
          "",
          "[gui]",
          "theme/custom=\"res://theme.tres\""
        ]
      _ -> []
    quoted s = "\"" ++ concatMap escape s ++ "\""
    escape '"' = "\\\""
    escape '\\' = "\\\\"
    escape '\n' = "\\n"
    escape c = [c]
