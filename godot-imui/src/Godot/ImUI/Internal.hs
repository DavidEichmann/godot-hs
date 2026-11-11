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
-- kind (and 'envKey') matches, created if it is new, and freed if the block
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
    UI (..)
  , Env (..)
  , Scope (..)
  , Attrs (..)
  , localAttrs
  , getAttrs
  , afterFrame

    -- * Widgets
  , Widget (..)
  , Sizing (..)
  , natural
  , widget
  , node
  , container
  , children
  , contentRoot
  , prop
  , forget
  , controlled
  , state
  , fired
  , events
  , release

    -- * Running
  , WindowConfig (..)
  , defaultWindow
  , runUI
  , runUIWith
  , quit
  ) where

import Control.Exception (finally, throwIO, try)
import Control.Monad (unless, when)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Dynamic (Dynamic, Typeable, fromDynamic, toDyn)
import Data.IORef
import Data.Int (Int64)
import qualified Data.Map.Strict as Map
import Foreign.Ptr (freeHaskellFunPtr, nullPtr)
import Foreign.Storable (poke)
import GHC.Clock (getMonotonicTimeNSec)
import Godot.Api
import Godot.Api.Internal (getInterface)
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
import qualified Godot.GDExtension as GDE
import System.Directory
  ( createDirectory
  , getCurrentDirectory
  , getTemporaryDirectory
  , removeDirectoryRecursive
  , setCurrentDirectory
  )
import System.Environment (getProgName)
import System.FilePath ((</>))
import System.IO.Error (isAlreadyExistsError)
import System.IO.Unsafe (unsafePerformIO)

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
  { envScope :: !Scope
  -- ^ the container being filled
  , envAttrs :: !Attrs
  -- ^ modifiers for the widgets created directly in this block
  , envKey :: !String
  -- ^ distinguishes widgets of the same kind (see 'Godot.ImUI.keyed')
  , envTree :: !SceneTree
  , envAfter :: !(IORef [IO ()])
  -- ^ what 'afterFrame' queued, last first
  }

-- | The children of a container, while they are being reconciled.
data Scope = Scope
  { scopeWidget :: !Widget
  -- ^ the container
  , scopeHorizontal :: !Bool
  -- ^ does the container lay its children out left to right?
  , scopeOld :: !(IORef [Widget])
  -- ^ last frame's children not reused yet, in tree order
  , scopeNew :: !(IORef [Widget])
  -- ^ this frame's children so far, last first
  , scopeCount :: !(IORef Int64)
  }

-- | What the modifiers in "Godot.ImUI" set. 'attrEnabled' is inherited by
-- nested widgets; the rest only applies to the widgets created directly in
-- the modified block.
data Attrs = Attrs
  { attrEnabled :: !Bool
  , attrExpand :: !Bool
  , attrMinSize :: !(Int, Int)
  , attrTooltip :: !String
  , attrPlaceholder :: !String
  }

defaultAttrs :: Attrs
defaultAttrs = Attrs True False (0, 0) "" ""

localAttrs :: (Attrs -> Attrs) -> UI a -> UI a
localAttrs f (UI body) = UI (\env -> body env {envAttrs = f (envAttrs env)})

getAttrs :: UI Attrs
getAttrs = UI (pure . envAttrs)

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
  { wKind :: !String
  , wKey :: !String
  , wNode :: !Node
  , wChildren :: !(IORef [Widget])
  , wProps :: !(IORef (Map.Map String Dynamic))
  -- ^ the last value of each 'prop', and each 'state'
  }

newWidget :: String -> String -> Node -> IO Widget
newWidget kind key n = Widget kind key n <$> newIORef [] <*> newIORef Map.empty

-- | How a kind of widget is sized by default. 'Godot.ImUI.expand' and
-- 'Godot.ImUI.minSize' override it.
data Sizing = Sizing
  { growsHorizontally :: !Bool
  -- ^ take up spare horizontal space (e.g. a text field in a row)
  , growsVertically :: !Bool
  , defaultMinSize :: !(Int, Int)
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
widget :: forall c. Inherits c Control => String -> Sizing -> IO c -> UI (Widget, c)
widget kind sizing create = do
  (w, c) <- node kind (upcast @Control <$> create)
  applyAttrs w c sizing
  pure (w, unsafeFromObjectPtr (toObjectPtr c))

-- | Like 'widget', for nodes that are not controls (popups, windows, ...):
-- they are not laid out by their container, so the sizing modifiers do not
-- apply to them.
node :: forall c. Inherits c Node => String -> IO c -> UI (Widget, c)
node kind create = UI $ \env -> do
  let sc = envScope env
      key = envKey env
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

-- | Size and tooltip, from the widget's defaults and the modifiers in scope.
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
container
  :: forall c a
   . Inherits c Control
  => String
  -> Bool
  -- ^ lays its children out left to right?
  -> Sizing
  -> IO c
  -> UI a
  -> UI a
container kind horizontal sizing create body = do
  (w, _ :: c) <- widget kind sizing create
  children horizontal w body

-- | Run a block, making the widgets it creates the children of the given
-- widget (replacing the ones it created last frame but not this time).
children :: Bool -> Widget -> UI a -> UI a
children horizontal w (UI body) = UI $ \env -> do
  let parent = wNode w
  sc <- Scope w horizontal <$> (newIORef =<< readIORef (wChildren w)) <*> newIORef [] <*> newIORef 0
  a <-
    body
      env
        { envScope = sc
        , envAttrs = defaultAttrs {attrEnabled = attrEnabled (envAttrs env)}
        , envKey = ""
        }
  unused <- readIORef (scopeOld sc)
  mapM_ (\u -> Node.removeChild parent (wNode u) >> Node.queueFree (wNode u)) unused
  writeIORef (wChildren w) . reverse =<< readIORef (scopeNew sc)
  pure a

-- | Fill a window with a themed background, a margin and a vertical box,
-- and return the box as a widget whose 'children' are the window's content.
contentRoot :: Node -> IO Widget
contentRoot window = do
  background <- PanelContainer.construct
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
state :: Typeable s => Widget -> String -> IO s -> UI s
state w name create = liftIO $ do
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
events :: Typeable e => Widget -> String -> ([Variant] -> IO e) -> UI [e]
events w signal decode = do
  queue <- state w ("signal:" ++ signal) $ do
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
release :: Inherits a RefCounted => a -> IO ()
release object = do
  unused <- RefCounted.unreference object
  when unused $ do
    iface <- getInterface
    GDE.object_destroy iface (toObjectPtr object)

--------------------------------------------------------------------------------
-- Running

-- | The application's window.
data WindowConfig = WindowConfig
  { windowTitle :: String
  , windowSize :: (Int, Int)
  -- ^ initial width and height, in pixels
  }

-- | A 640x480 window titled after the program.
defaultWindow :: WindowConfig
defaultWindow = WindowConfig {windowTitle = "", windowSize = (640, 480)}

-- | Open a window and run the UI in it until it is closed (or 'quit' is
-- called).
--
-- The function is given the application's state, describes the UI for it,
-- and returns the state for the next frame:
--
-- @
-- main :: IO ()
-- main = runUI defaultWindow (0 :: Int) $ \\count -> do
--   label (\"Count: \" ++ show count)
--   clicked <- button \"Increment\"
--   pure (if clicked then count + 1 else count)
-- @
--
-- The top-level widgets are stacked vertically. Call this from the main
-- thread, and only once: Godot cannot be started again in the same process.
runUI :: WindowConfig -> s -> (s -> UI s) -> IO ()
runUI = runUIWith []

-- | 'runUI' with extra Godot command line arguments (e.g. @--headless@).
runUIWith :: [String] -> WindowConfig -> s -> (s -> UI s) -> IO ()
runUIWith extraArgs win initial frame = do
  again <- atomicModifyIORef' alreadyRun (\r -> (True, r))
  when again $ fail "Godot.ImUI.runUI: Godot can only be started once per process"
  progName <- getProgName
  let title = if null (windowTitle win) then progName else windowTitle win
  -- Godot calls this while creating the instance. Nothing is registered: we
  -- only need the GDExtension interface.
  onLevel <- GDE.wrapGDExtensionInitializeCallback $ \_ _ -> pure ()
  initFn <- GDE.wrapGDExtensionInitializationFunction $ \gpa library rInit -> do
    _ <- initialize gpa library
    poke
      rInit
      GDE.GDExtensionInitialization
        { GDE.gdExtensionInitialization_minimum_initialization_level = GDE.GDEXTENSION_INITIALIZATION_CORE
        , GDE.gdExtensionInitialization_userdata = nullPtr
        , GDE.gdExtensionInitialization_initialize = onLevel
        , GDE.gdExtensionInitialization_deinitialize = onLevel
        }
    pure 1
  project <- createProject title (windowSize win)
  -- Godot changes the working directory to the project's. Change it back
  -- once Godot is running, so that relative paths in the application keep
  -- meaning what they did.
  cwd <- getCurrentDirectory
  -- No audio: a UI has no use for Godot's audio thread.
  let args = [progName, "--path", project, "--audio-driver", "Dummy"] ++ extraArgs
  ( GDE.withGodotArgs args $ \argc argv -> do
      instPtr <- GDE.createGodotInstance argc argv initFn
      when (instPtr == GDE.GDExtensionObjectPtr nullPtr) $ fail "Godot.ImUI: could not create the Godot instance"
      let inst = unsafeFromObjectPtr @GodotInstance instPtr
      ( do
          started <- GodotInstance.start inst
          unless started $ fail "Godot.ImUI: could not start Godot"
          setCurrentDirectory cwd
          (tree, root) <- setupRoot title
          -- 'children' replaces this scope with a fresh one for each frame.
          outside <- Scope root False <$> newIORef [] <*> newIORef [] <*> newIORef 0
          after <- newIORef []
          let env = Env {envScope = outside, envAttrs = defaultAttrs, envKey = "", envTree = tree, envAfter = after}
              loop s = do
                s' <- unUI (children False root (frame s)) env
                sequence_ . reverse =<< atomicModifyIORef' after (\queued -> ([], queued))
                stop <- GodotInstance.iteration inst
                unless stop (loop s')
          loop initial
        )
        `finally` GDE.destroyGodotInstance instPtr
    )
    `finally` do
      setCurrentDirectory cwd
      removeDirectoryRecursive project
      freeHaskellFunPtr initFn
      freeHaskellFunPtr onLevel

-- | Godot cannot be restarted: godot-low caches pointers into the engine.
alreadyRun :: IORef Bool
alreadyRun = unsafePerformIO (newIORef False)
{-# NOINLINE alreadyRun #-}

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

-- | Godot only runs a project, so make an empty one in a fresh temporary
-- directory. The UI itself is all created at runtime.
createProject :: String -> (Int, Int) -> IO FilePath
createProject title (width, height) = do
  tmp <- getTemporaryDirectory
  let fresh = do
        n <- getMonotonicTimeNSec
        let dir = tmp </> ("godot-imui-" ++ show n)
        r <- try (createDirectory dir)
        case r of
          Right () -> pure dir
          Left e
            | isAlreadyExistsError e -> fresh
            | otherwise -> throwIO e
  dir <- fresh
  writeFile (dir </> "project.godot") $
    unlines
      [ "config_version=5"
      , ""
      , "[application]"
      , "config/name=" ++ quoted title
      , -- One user data directory (shader cache etc.) for all godot-imui
        -- programs, rather than one per window title.
        "config/use_custom_user_dir=true"
      , "config/custom_user_dir_name=\"godot-imui\""
      , "run/main_scene=\"res://main.tscn\""
      , -- Only redraw when something changed.
        "run/low_processor_mode=true"
      , -- ... and poll at about 100 Hz in between (the default is 144 Hz).
        "run/low_processor_mode_sleep_usec=10000"
      , "boot_splash/show_image=false"
      , ""
      , "[debug]"
      , "file_logging/enable_file_logging=false"
      , "file_logging/enable_file_logging.pc=false"
      , ""
      , "[display]"
      , -- Dialogs and menus are windows of their own.
        "window/subwindows/embed_subwindows=false"
      , "window/size/viewport_width=" ++ show width
      , "window/size/viewport_height=" ++ show height
      ]
  writeFile (dir </> "main.tscn") $
    unlines ["[gd_scene format=3]", "", "[node name=\"Main\" type=\"Node\"]"]
  -- Godot reports an error if an exported project has no script class cache.
  createDirectory (dir </> ".godot")
  writeFile (dir </> ".godot" </> "global_script_class_cache.cfg") "list=[]\n"
  pure dir
  where
    quoted s = "\"" ++ concatMap escape s ++ "\""
    escape '"' = "\\\""
    escape '\\' = "\\\\"
    escape '\n' = "\\n"
    escape c = [c]
