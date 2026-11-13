{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

-- | Runs a UI headless for a few frames. Between frames it plays the user
-- by poking the Godot controls directly (emitting a button's @pressed@
-- signal, changing a text field's text, ...) and checks what the widgets
-- return in the next frame.
module Main (main) where

import Control.Concurrent (MVar, newEmptyMVar, putMVar, readMVar, takeMVar, threadDelay)
import Control.Exception (IOException, onException, throwIO, try)
import Control.Monad (forM, forM_, forever, unless, when)
import Data.List (isInfixOf)
import Data.Maybe (isJust)
import Data.IORef
import Data.Int (Int64)
import qualified Data.Tree as T
import Data.Typeable (Typeable)
import Godot.Api
import qualified Godot.Api.Class.BaseButton as BaseButton
import qualified Godot.Api.Class.Button as Button
import qualified Godot.Api.Class.Control as Control
import qualified Godot.Api.Class.Engine as Engine
import qualified Godot.Api.Class.ColorPickerButton as ColorPickerButton
import qualified Godot.Api.Class.Image as Image
import qualified Godot.Api.Class.ItemList as ItemList
import qualified Godot.Api.Class.Label as Label
import qualified Godot.Api.Class.MenuBar as MenuBar
import qualified Godot.Api.Class.MenuButton as MenuButton
import qualified Godot.Api.Class.PopupMenu as PopupMenu
import qualified Godot.Api.Class.Texture2D as Texture2D
import qualified Godot.Api.Class.Theme as Theme
import qualified Godot.Api.Class.ThemeDB as ThemeDB
import qualified Godot.Api.Class.TextureRect as TextureRect
import qualified Godot.Api.Class.Tree as Tree
import qualified Godot.Api.Class.TreeItem as TreeItem
import qualified Godot.Api.Class.Window as Window
import qualified Godot.Api.Class.LineEdit as LineEdit
import qualified Godot.Api.Class.Node as Node
import qualified Godot.Api.Class.Object as Object
import qualified Godot.Api.Class.OptionButton as OptionButton
import qualified Godot.Api.Class.ProgressBar as ProgressBar
import qualified Godot.Api.Class.Range as Range
import qualified Godot.Api.Class.SceneTree as SceneTree
import qualified Godot.Api.Class.TabBar as TabBar
import qualified Godot.Api.Class.TextEdit as TextEdit
import Godot.ImUI hiding (Forest, Tree (..))
import Godot.ImUI.Internal (release, runUIWith)
import System.Directory (getCurrentDirectory, getTemporaryDirectory, removeFile)
import System.FilePath ((</>))
import System.Exit (exitFailure)
import System.Timeout (timeout)

data State = State
  { frame :: Int
  , count :: Int
  , name :: String
  , notes :: String
  , checked :: Bool
  , volume :: Double
  , copies :: Int
  , size :: Int
  , tab :: Int
  , items :: [String]
  -- ^ keys of a list of text fields
  , extra :: Extra
  }

-- | The state of the widgets in 'extras'.
data Extra = Extra
  { wrap :: Bool
  , forest :: T.Forest String
  , treeSel :: Maybe [Int]
  , listSel :: Int
  , radio :: Int
  , switch :: Bool
  , colour :: Color
  , dialogOpen :: Bool
  , fileOpen :: Bool
  }

main :: IO ()
main = do
  failures <- newIORef (0 :: Int)
  finished <- newIORef False
  gates <- Gates <$> newEmptyMVar <*> newEmptyMVar <*> newEmptyMVar <*> newEmptyMVar <*> newEmptyMVar
  let check :: String -> Bool -> UI ()
      check what ok = liftIO $ do
        putStrLn ((if ok then "ok   " else "FAIL ") ++ what)
        unless ok (modifyIORef' failures (+ 1))
      fruit = [T.Node "Fruit" [T.Node "Apple" [], T.Node "Pear" []], T.Node "Veg" [T.Node "Leek" []], T.Node "Bread" []]
      initial =
        State 0 0 "start" "" False 2 1 0 0 ["a", "b", "c"] $
          Extra False fruit Nothing 0 0 False (Color 1 0 0 1) False False
  png <- (</> "godot-imui-test.png") <$> getTemporaryDirectory
  -- Remembered between frames: the Godot objects behind the list items.
  itemIds <- newIORef []
  cwdBefore <- getCurrentDirectory
  cwdInside <- newIORef ""

  runUIWith ["--headless"] defaultWindow {windowTitle = "godot-imui \"test\""} $ withState initial $ \s -> do
    let f = frame s
    -- State in the UI monad: counts the frames.
    (frames, setFrames) <- state (0 :: Int)
    liftIO (setFrames (frames + 1))
    when (f == 3) (check ("state: kept from frame to frame: " ++ show frames) (frames == 3))
    when (f == 2) (liftIO (writeIORef cwdInside =<< getCurrentDirectory))
    -- An image file for 'image' to load.
    when (f == 0) $ liftIO $ do
      Just pixels <- Image.createEmpty 8 4 False Image_FORMAT_RGBA8
      Image.fill pixels (Color 1 0 0 1)
      _ <- Image.savePng pixels =<< toGString png
      release pixels
    -- The generated theme loaded (Godot only logs an error if it does not).
    when (f == 0) $ do
      buttonStyle <- liftIO $ do
        theme <- ThemeDB.getProjectTheme =<< ThemeDB.singleton
        case theme of
          Nothing -> pure False
          Just t -> do
            normal <- toStringName "normal"
            buttonType <- toStringName "Button"
            has <- Theme.hasStylebox t normal buttonType
            release t
            pure has
      check "theme: the Modern theme is the project's theme" buttonStyle
    heading "Test" -- 0
    clicked <- button ("Count: " ++ show (count s)) -- 1
    name' <- textInputWith textInputOpts {placeholder = "Name"} (name s) -- 2
    checked' <- checkbox "Check" (checked s) -- 3
    volume' <- slider (0, 10) (volume s) -- 4
    copies' <- spinBox (1, 5) (copies s) -- 5
    size' <- dropdown ["Small", "Medium", "Large"] (size s) -- 6
    tab' <- tabBar ["One", "Two"] (tab s) -- 7
    notes' <- textArea (notes s) -- 8
    progressBarWith progressBarOpts {indeterminate = f == 2} (volume s / 10) -- 9
    separator -- 10
    -- 11: a row whose content changes from frame to frame
    hbox $ do
      label "Row"
      when (f == 3) (label "only in frame 3")
      separator
      disabledClick <- buttonWith buttonOpts {enabled = False} "Disabled"
      check "a disabled button is never clicked" (not disabledClick)
      spacer
    -- 12: nested containers and options
    scrollWith scrollOpts {expand = True} $ panel $ grid 2 $ do
      label "A"
      _ <- buttonWith buttonOpts {tooltip = "tip", minSize = (80, 0)} "B"
      center (paragraph "C")
      padding 4 (label "D")
    -- 13: the rest of the widgets
    extra' <- vbox (extras check png gates f (extra s))
    -- 14..: keyed items, reordered in frame 6
    forM_ (items s) $ \item -> textInputWith textInputOpts {key = item} item

    box <- liftIO rootBox
    let base = 14
    case f of
      -- The controls exist now: play the user.
      1 -> liftIO $ do
        b <- child @Button box 1
        pressed <- toVariant =<< toStringName "pressed"
        _ <- Object.emitSignal b [pressed]
        e <- child @LineEdit box 2
        LineEdit.setText e =<< toGString "typed"
        c <- child @CheckBox box 3
        BaseButton.setPressed c True
        sl <- child @HSlider box 4
        Range.setValue sl 7
        sp <- child @SpinBox box 5
        Range.setValue sp 99 -- clamped to the range
        o <- child @OptionButton box 6
        OptionButton.select o 2
        t <- child @TabBar box 7
        TabBar.setCurrentTab t 1
        te <- child @TextEdit box 8
        TextEdit.setText te =<< toGString "line 1\nline 2"
        writeIORef itemIds =<< forM [0 .. 2] (\i -> child @LineEdit box (base + i) >>= Object.getInstanceId)
      2 -> do
        spinning <- liftIO (child @ProgressBar box 9 >>= ProgressBar.isIndeterminate)
        check "progressBar: indeterminate" spinning
        check "button: clicked after its signal" clicked
        check "textInput: returns the typed text" (name' == "typed")
        check "checkbox: returns the new state" checked'
        check ("slider: returns the new value: " ++ show volume') (abs (volume' - 7) < 0.01)
        check ("spinBox: returns the clamped value: " ++ show copies') (copies' == 5)
        check "dropdown: returns the new selection" (size' == 2)
        check "tabBar: returns the new tab" (tab' == 1)
        check "textArea: returns the typed text" (notes' == "line 1\nline 2")
        shown <- liftIO (child @Button box 1 >>= Button.getText >>= fromGString)
        check ("button: text follows the state: " ++ shown) (shown == "Count: 0")
      3 -> do
        spinning <- liftIO (child @ProgressBar box 9 >>= ProgressBar.isIndeterminate)
        check "progressBar: no longer indeterminate" (not spinning)
        check "button: clicked only once" (not clicked)
        shown <- liftIO (child @Button box 1 >>= Button.getText >>= fromGString)
        check ("button: text follows the state: " ++ shown) (shown == "Count: 1")
        n <- liftIO (child @HBoxContainer box 11 >>= \row -> Node.getChildCount row False)
        check ("conditional widget is created: " ++ show n) (n == 5)
        second <- liftIO (child @HBoxContainer box 11 >>= \row -> child @Label row 1 >>= Label.getText >>= fromGString)
        check ("conditional widget is in its place: " ++ second) (second == "only in frame 3")
      4 -> do
        n <- liftIO (child @HBoxContainer box 11 >>= \row -> Node.getChildCount row False)
        check ("conditional widget is removed: " ++ show n) (n == 4)
        disabled <- liftIO (child @HBoxContainer box 11 >>= \row -> child @Button row 2 >>= BaseButton.isDisabled)
        check "enabled False disables a button" disabled
        tip <- liftIO $ do
          sc <- child @ScrollContainer box 12
          content <- child @VBoxContainer sc 0
          p <- child @PanelContainer content 0
          m <- child @MarginContainer p 0
          v <- child @VBoxContainer m 0
          g <- child @GridContainer v 0
          b <- child @Button g 1
          Control.getTooltipText b >>= fromGString
        check ("nested containers and tooltip: " ++ tip) (tip == "tip")
      -- In frame 5 the state below sets values programmatically.
      6 -> do
        shown <- liftIO (child @LineEdit box 2 >>= LineEdit.getText >>= fromGString)
        check ("textInput: shows the state's text: " ++ shown) (shown == "reset" && name' == "reset")
        sel <- liftIO (child @OptionButton box 6 >>= OptionButton.getSelected)
        check ("dropdown: shows the state's selection: " ++ show sel) (sel == 0 && size' == 0)
        v <- liftIO (child @HSlider box 4 >>= Range.getValue)
        check ("slider: shows the state's value: " ++ show v) (abs (v - 1) < 0.01)
      -- In frame 6 the state below reverses the items.
      7 -> do
        before <- liftIO (readIORef itemIds)
        after <- liftIO (forM [0 .. 2] (\i -> child @LineEdit box (base + i) >>= Object.getInstanceId))
        check "keyed: controls follow their items when reordered" (after == reverse before)
        texts <- liftIO (forM [0 .. 2] (\i -> child @LineEdit box (base + i) >>= LineEdit.getText >>= fromGString))
        check ("keyed: order in the tree: " ++ show texts) (texts == ["c", "b", "a"])
      -- In frame 7 the state below drops the middle item. Its control is
      -- removed at the end of frame 8.
      9 -> do
        n <- liftIO (Node.getChildCount box False)
        check ("removed item's control is gone: " ++ show n) (n == base + 2)
        liftIO (writeIORef finished True)
        quit
      _ -> pure ()

    let s' =
          s
            { frame = f + 1
            , count = if clicked then count s + 1 else count s
            , name = name'
            , notes = notes'
            , checked = checked'
            , volume = volume'
            , copies = copies'
            , size = size'
            , tab = tab'
            , extra = extra'
            }
    pure $ case f of
      5 -> s' {name = "reset", size = 0, volume = 1}
      6 -> s' {items = reverse (items s)}
      7 -> s' {items = ["c", "a"]}
      _ -> s'

  removeFile png
  cwdAfter <- getCurrentDirectory
  check' failures ("the working directory is restored: " ++ cwdAfter) (cwdAfter == cwdBefore)
  inside <- readIORef cwdInside
  check' failures "the working directory is unchanged while the UI runs" (inside == cwdBefore)

  -- Godot cannot be started twice: that must be an error, not a crash.
  second <- try (runUIWith ["--headless"] defaultWindow (pure ()))
  check' failures "runUI refuses to run a second time" $ case second of
    Left (_ :: IOException) -> True
    Right () -> False

  done <- readIORef finished
  unless done (putStrLn "FAIL the UI stopped before the last frame")
  n <- readIORef failures
  when (n > 0 || not done) exitFailure

-- | Keep the test's state in a 'state', passing it through each frame.
withState :: (Typeable s) => s -> (s -> UI s) -> UI ()
withState initial ui = do
  (s, set) <- state initial
  s' <- ui s
  liftIO (set s')

-- | Menus, trees, images, dialogs, ...: the content of top-level widget 13.
extras :: (String -> Bool -> UI ()) -> FilePath -> Gates -> Int -> Extra -> UI Extra
extras check png gates f x = do
  (open, wrap', recent) <- menuBar $ do
    -- 0
    chosen <- menu "File" $ do
      open <- menuItem "Open"
      menuSeparator
      wrap' <- menuCheck "Wrap" (wrap x)
      recent <- menu "Recent" (menuItem "a.txt")
      _ <- menuItemWith menuItemOpts {enabled = False} "Disabled"
      pure (open, wrap', recent)
    about <- menu "Help" (menuItem "About")
    check "menu: an item that was not chosen is not reported" (not about)
    pure chosen
  option <- menu "Options" (menuItem "One") -- 1
  treeSel' <- tree (forest x) (treeSel x) -- 2
  listSel' <- listBox ["x", "y", "z"] (listSel x) -- 3
  image png -- 4
  radio' <- hbox (radioButtons ["A", "B", "C"] (radio x)) -- 5
  switch' <- toggle "Switch" (switch x) -- 6
  linked <- link "Link" -- 7
  richText "[b]Bold[/b]" -- 8
  colour' <- colorPicker (colour x) -- 9
  collapsible "More" (label "inside") -- 10
  _ <- hsplit (label "left") (label "right") -- 11
  _ <- flow (button "f1" >> button "f2") -- 12
  -- 13: one of the dialogs, when open
  dialogResult <- if dialogOpen x then Just <$> dialog "Dialog" (button "OK") else pure Nothing
  fileResult <- if fileOpen x then openFileDialog "Open" ["*.txt"] else pure Choosing
  -- After the dialogs: a container with a state, not created in frame 3.
  when (f `elem` [1, 2, 4]) $ vbox $ do
    (n, setN) <- state (0 :: Int)
    (s, setS) <- state "a"
    liftIO (setN (n + 1) >> setS (s ++ "a"))
    when (f == 2) (check ("state: kept in its container: " ++ show (n, s)) (n == 1 && s == "aa"))
    when (f == 4) (check ("state: starts again when its container is created again: " ++ show (n, s)) (n == 0 && s == "a"))

  e <- liftIO (rootBox >>= \box -> child @VBoxContainer box 13)
  file <- liftIO (child @MenuBar e 0 >>= \bar -> child @PopupMenu bar 0)
  let text get = liftIO (get >>= fromGString)
  case f of
    1 -> do
      bar <- liftIO (child @MenuBar e 0)
      menus <- liftIO (MenuBar.getMenuCount bar)
      title <- text (MenuBar.getMenuTitle bar 0)
      check ("menuBar: has its menus: " ++ show (menus, title)) (menus == 2 && title == "File")
      n <- liftIO (PopupMenu.getItemCount file)
      first <- text (PopupMenu.getItemText file 0)
      isSeparator <- liftIO (PopupMenu.isItemSeparator file 1)
      isChecked <- liftIO (PopupMenu.isItemChecked file 2)
      submenu <- text (PopupMenu.getItemText file 3)
      isDisabled <- liftIO (PopupMenu.isItemDisabled file 4)
      check ("menu: has its items: " ++ show (n, first, submenu)) (n == 5 && first == "Open" && submenu == "Recent")
      check "menu: separator, check mark and disabled item" (isSeparator && not isChecked && isDisabled)
      t <- liftIO (child @Tree e 2)
      Just root <- liftIO (Tree.getRoot t)
      Just fruit <- liftIO (TreeItem.getChild root 0)
      Just pear <- liftIO (TreeItem.getChild fruit 1)
      Just veg <- liftIO (TreeItem.getChild root 1)
      top <- liftIO (TreeItem.getChildCount root)
      pearText <- text (TreeItem.getText pear 0)
      check ("tree: has its items: " ++ show (top, pearText)) (top == 3 && pearText == "Pear")
      shown <- liftIO (child @TextureRect e 4 >>= TextureRect.getTexture)
      pixels <- case shown of
        Nothing -> pure (0, 0)
        Just texture -> liftIO $ do
          dimensions <- (,) <$> Texture2D.getWidth texture <*> Texture2D.getHeight texture
          release texture
          pure dimensions
      check ("image: shows the file: " ++ show pixels) (pixels == (8, 4))
      -- Play the user.
      liftIO $ do
        emit file "index_pressed" . pure =<< toVariant (0 :: Int64)
        emit file "index_pressed" . pure =<< toVariant (2 :: Int64)
        recentMenu <- child @PopupMenu file 0
        emit recentMenu "index_pressed" . pure =<< toVariant (0 :: Int64)
        Just options <- child @MenuButton e 1 >>= MenuButton.getPopup
        emit options "index_pressed" . pure =<< toVariant (0 :: Int64)
        TreeItem.setCollapsed veg True
        TreeItem.select pear 0 True
        child @ItemList e 3 >>= \l -> ItemList.select l 2 True
        child @HBoxContainer e 5 >>= \row -> child @CheckBox row 2 >>= \b -> BaseButton.setPressed b True
        child @CheckButton e 6 >>= \b -> BaseButton.setPressed b True
        child @LinkButton e 7 >>= \b -> emit b "pressed" []
        child @ColorPickerButton e 9 >>= \b -> ColorPickerButton.setPickColor b (Color 0 1 0 1)
    2 -> do
      check "menuItem: chosen" open
      check "menuCheck: toggled" wrap'
      check "menu: an item of a submenu is chosen" recent
      check "menu: outside a menu bar it is a button with a menu" option
      check ("tree: returns the selected path: " ++ show treeSel') (treeSel' == Just [0, 1])
      check ("listBox: returns the new selection: " ++ show listSel') (listSel' == 2)
      check ("radioButtons: returns the new selection: " ++ show radio') (radio' == 2)
      check "toggle: returns the new state" switch'
      check "link: clicked" linked
      check ("colorPicker: returns the new colour: " ++ show colour') (colour' == Color 0 1 0 1)
    3 -> do
      check "menuItem: chosen only once" (not open)
      isChecked <- liftIO (PopupMenu.isItemChecked file 2)
      check "menuCheck: shows the state" isChecked
      row <- liftIO (child @HBoxContainer e 5)
      pressed <- liftIO (forM [0, 2] (\i -> child @CheckBox row i >>= BaseButton.isPressed))
      check ("radioButtons: only the selected one is on: " ++ show pressed) (pressed == [False, True])
    -- In frame 3 the state below changes the forest.
    4 -> do
      t <- liftIO (child @Tree e 2)
      Just root <- liftIO (Tree.getRoot t)
      Just fruit <- liftIO (TreeItem.getChild root 0)
      Just plum <- liftIO (TreeItem.getChild fruit 1)
      Just veg <- liftIO (TreeItem.getChild root 1)
      top <- liftIO (TreeItem.getChildCount root)
      plumText <- text (TreeItem.getText plum 0)
      check ("tree: shows the changed items: " ++ show (top, plumText)) (top == 4 && plumText == "Plum")
      collapsed <- liftIO (TreeItem.isCollapsed veg)
      check "tree: unchanged items keep their state" collapsed
      selected <- liftIO (TreeItem.isSelected plum 0)
      check ("tree: keeps the selection: " ++ show treeSel') (selected && treeSel' == Just [0, 1])
    -- In frame 4 the state below opens the dialog.
    5 -> do
      win <- liftIO (child @Window e 13)
      title <- text (Window.getTitle win)
      modal <- liftIO (Window.isExclusive win)
      check ("dialog: is created: " ++ title) (title == "Dialog" && modal)
      ok <- liftIO $ do
        background <- child @PanelContainer win 0
        margin <- child @MarginContainer background 0
        content <- child @VBoxContainer margin 0
        child @Button content 0 >>= Button.getText >>= fromGString
      check "dialog: holds its widgets" (ok == "OK")
      check "dialog: returns its block's result while open" (dialogResult == Just (Just False))
      liftIO (emit win "close_requested" [])
    6 -> do
      -- Windows are shown at the end of the frame that creates them.
      visible <- liftIO (child @Window e 13 >>= Window.isVisible)
      check "dialog: is shown" visible
      check "dialog: returns Nothing when closed" (dialogResult == Just Nothing)
    -- In frame 6 the state below closes the dialog and opens the file dialog.
    7 -> do
      d <- liftIO (child @FileDialog e 13)
      check "openFileDialog: is created" (fileResult == Choosing)
      liftIO (emit d "file_selected" . pure =<< toVariant =<< toGString "/tmp/chosen.txt")
    8 -> do
      visible <- liftIO (child @FileDialog e 13 >>= Window.isVisible)
      check "openFileDialog: is shown" visible
      check ("openFileDialog: returns the chosen file: " ++ show fileResult) (fileResult == Chose "/tmp/chosen.txt")
      n <- liftIO (Node.getChildCount e False)
      check ("dialog: is gone when no longer created: " ++ show n) (n == 14)
    _ -> pure ()

  -- Last, so that the other widgets' positions do not change.
  asyncTest check gates f

  let x' = x {wrap = wrap', treeSel = treeSel', listSel = listSel', radio = radio', switch = switch', colour = colour'}
  pure $ case f of
    3 -> x' {forest = [T.Node "Fruit" [T.Node "Apple" [], T.Node "Plum" []], T.Node "Veg" [T.Node "Leek" []], T.Node "Bread" [], T.Node "Milk" []]}
    4 -> x' {dialogOpen = True}
    6 -> x' {dialogOpen = False, fileOpen = True}
    8 -> x' {fileOpen = False}
    _ -> x'

-- | What the work in 'asyncTest' waits for, and tells.
data Gates = Gates
  { gate1, gate2, gate3 :: MVar Int,
    -- | that the work was cancelled
    cancelled2, cancelled4 :: MVar ()
  }

-- | 'asyncState', in a container created in frames 0 to 6. (Keyed, so that
-- it is not mistaken for the container before it, which comes and goes.)
asyncTest :: (String -> Bool -> UI ()) -> Gates -> Int -> UI ()
asyncTest check g f = do
  when (f <= 6) $ vboxWith vboxOpts {key = "async"} $ do
    (st, start, stop) <- asyncState (Just (takeMVar (gate1 g)))
    let is what ok = check ("asyncState: " ++ what ++ ": " ++ show st) ok
        -- Time for work that can finish to finish, before the next frame.
        settle = threadDelay 100000
    case f of
      0 -> is "runs the initial work" (case st of Running Nothing -> True; _ -> False)
      1 -> do
        is "still running" (case st of Running Nothing -> True; _ -> False)
        liftIO (putMVar (gate1 g) 1 >> settle)
      2 -> do
        is "done" (case st of Done 1 -> True; _ -> False)
        liftIO (start (throwIO (userError "boom")) >> settle)
      3 -> do
        is "failed, keeping the value before" (case st of Failed e (Just 1) -> "boom" `isInfixOf` show e; _ -> False)
        liftIO (start (takeMVar (gate2 g) `onException` putMVar (cancelled2 g) ()))
      4 -> do
        is "running again, with the last value" (case st of Running (Just 1) -> True; _ -> False)
        -- Interrupts the work running.
        liftIO (start (pure 3) >> settle)
      5 -> do
        is "the new work's result" (case st of Done 3 -> True; _ -> False)
        interrupted <- liftIO (timeout 1000000 (readMVar (cancelled2 g)))
        check "asyncState: starting work cancels the work running" (isJust interrupted)
        liftIO (start (takeMVar (gate3 g)) >> stop)
      6 -> do
        is "cancelling keeps the last result" (case st of Done 3 -> True; _ -> False)
        liftIO (start (forever (threadDelay 1000000) `onException` putMVar (cancelled4 g) ()))
      _ -> pure ()
  -- The container is gone from frame 7: its work is cancelled.
  when (f == 8) $ do
    cancelled <- liftIO (timeout 1000000 (readMVar (cancelled4 g)))
    check "asyncState: the work is cancelled when its container is gone" (isJust cancelled)

-- | Emit a signal of an object, with the given arguments.
emit :: Inherits o Object => o -> String -> [Variant] -> IO ()
emit object signal args = do
  signalName <- toVariant =<< toStringName signal
  _ <- Object.emitSignal object (signalName : args)
  pure ()

-- | The box holding the top-level widgets.
rootBox :: IO Node
rootBox = do
  Just mainLoop <- Engine.getMainLoop =<< Engine.singleton
  Just sceneTree <- cast @SceneTree mainLoop
  Just root <- SceneTree.getRoot sceneTree
  n <- Node.getChildCount root False
  Just background <- Node.getChild root (n - 1) False
  Just margin <- Node.getChild background 0 False
  Just box <- Node.getChild margin 0 False
  pure box

-- | The i-th child of a node, which must be of the given class.
child :: forall c p. (GodotClass c, Inherits p Node) => p -> Int64 -> IO c
child parent i = do
  found <- Node.getChild parent i False
  case found of
    Nothing -> fail ("no child " ++ show i)
    Just n -> cast @c n >>= maybe (fail ("child " ++ show i ++ " is not a " ++ godotClassName @c)) pure

check' :: IORef Int -> String -> Bool -> IO ()
check' failures what ok = do
  putStrLn ((if ok then "ok   " else "FAIL ") ++ what)
  unless ok (modifyIORef' failures (+ 1))
