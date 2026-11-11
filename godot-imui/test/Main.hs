{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Runs a UI headless for a few frames. Between frames it plays the user
-- by poking the Godot controls directly (emitting a button's @pressed@
-- signal, changing a text field's text, ...) and checks what the widgets
-- return in the next frame.
module Main (main) where

import Control.Exception (IOException, try)
import Control.Monad (forM, forM_, unless, when)
import Data.IORef
import Data.Int (Int64)
import Godot.Api
import qualified Godot.Api.Class.BaseButton as BaseButton
import qualified Godot.Api.Class.Button as Button
import qualified Godot.Api.Class.Control as Control
import qualified Godot.Api.Class.Engine as Engine
import qualified Godot.Api.Class.Label as Label
import qualified Godot.Api.Class.LineEdit as LineEdit
import qualified Godot.Api.Class.Node as Node
import qualified Godot.Api.Class.Object as Object
import qualified Godot.Api.Class.OptionButton as OptionButton
import qualified Godot.Api.Class.Range as Range
import qualified Godot.Api.Class.SceneTree as SceneTree
import qualified Godot.Api.Class.TabBar as TabBar
import qualified Godot.Api.Class.TextEdit as TextEdit
import Godot.ImUI
import Godot.ImUI.Internal (runUIWith)
import System.Directory (getCurrentDirectory)
import System.Exit (exitFailure)

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
  }

main :: IO ()
main = do
  failures <- newIORef (0 :: Int)
  finished <- newIORef False
  let check :: String -> Bool -> UI ()
      check what ok = liftIO $ do
        putStrLn ((if ok then "ok   " else "FAIL ") ++ what)
        unless ok (modifyIORef' failures (+ 1))
      initial = State 0 0 "start" "" False 2 1 0 0 ["a", "b", "c"]
  -- Remembered between frames: the Godot objects behind the list items.
  itemIds <- newIORef []
  cwdBefore <- getCurrentDirectory
  cwdInside <- newIORef ""

  runUIWith ["--headless"] defaultWindow {windowTitle = "godot-imui \"test\""} initial $ \s -> do
    let f = frame s
    when (f == 2) (liftIO (writeIORef cwdInside =<< getCurrentDirectory))
    heading "Test" -- 0
    clicked <- button ("Count: " ++ show (count s)) -- 1
    name' <- placeholder "Name" (textInput (name s)) -- 2
    checked' <- checkbox "Check" (checked s) -- 3
    volume' <- slider (0, 10) (volume s) -- 4
    copies' <- spinBox (1, 5) (copies s) -- 5
    size' <- dropdown ["Small", "Medium", "Large"] (size s) -- 6
    tab' <- tabBar ["One", "Two"] (tab s) -- 7
    notes' <- textArea (notes s) -- 8
    progressBar (volume s / 10) -- 9
    separator -- 10
    -- 11: a row whose content changes from frame to frame
    hbox $ do
      label "Row"
      when (f == 3) (label "only in frame 3")
      separator
      disabledClick <- enabled False (button "Disabled")
      check "a disabled button is never clicked" (not disabledClick)
      spacer
    -- 12: nested containers and modifiers
    expand $ scroll $ panel $ grid 2 $ do
      label "A"
      _ <- tooltip "tip" (minSize 80 0 (button "B"))
      center (paragraph "C")
      padding 4 (label "D")
    -- 13..: keyed items, reordered in frame 6
    forM_ (items s) $ \item -> keyed item (textInput item)

    box <- liftIO rootBox
    let base = 13
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
            }
    pure $ case f of
      5 -> s' {name = "reset", size = 0, volume = 1}
      6 -> s' {items = reverse (items s)}
      7 -> s' {items = ["c", "a"]}
      _ -> s'

  cwdAfter <- getCurrentDirectory
  check' failures ("the working directory is restored: " ++ cwdAfter) (cwdAfter == cwdBefore)
  inside <- readIORef cwdInside
  check' failures "the working directory is unchanged while the UI runs" (inside == cwdBefore)

  -- Godot cannot be started twice: that must be an error, not a crash.
  second <- try (runUIWith ["--headless"] defaultWindow () pure)
  check' failures "runUI refuses to run a second time" $ case second of
    Left (_ :: IOException) -> True
    Right () -> False

  done <- readIORef finished
  unless done (putStrLn "FAIL the UI stopped before the last frame")
  n <- readIORef failures
  when (n > 0 || not done) exitFailure

-- | The box holding the top-level widgets.
rootBox :: IO Node
rootBox = do
  Just mainLoop <- Engine.getMainLoop =<< Engine.singleton
  Just tree <- cast @SceneTree mainLoop
  Just window <- SceneTree.getRoot tree
  n <- Node.getChildCount window False
  Just background <- Node.getChild window (n - 1) False
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
