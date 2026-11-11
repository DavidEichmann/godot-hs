-- | A simple immediate mode UI, rendered by Godot.
--
-- The UI is a function from your application's state to a 'UI' action that
-- returns the next state. It runs every frame: each widget shows itself and
-- returns what the user did with it.
--
-- @
-- import Godot.ImUI
--
-- main :: IO ()
-- main = 'runUI' 'defaultWindow' (0 :: Int) $ \\count -> do
--   'label' (\"Count: \" ++ show count)
--   clicked <- 'button' \"Increment\"
--   pure (if clicked then count + 1 else count)
-- @
--
-- * Widgets that edit a value take the current value and return the new
--   one: @name' <- 'textInput' name@.
-- * Layout is done with containers that take a block of widgets: 'vbox',
--   'hbox', 'grid', ...  The top level is a 'vbox'.
-- * Ordinary control flow decides what is shown: use 'Control.Monad.when',
--   @case@, 'mapM_', ... Widgets that are no longer created disappear.
-- * 'UI' is a 'MonadIO': use 'liftIO' for anything else.
module Godot.ImUI
  ( -- * Running
    runUI
  , WindowConfig (..)
  , defaultWindow
  , UI
  , quit
  , liftIO

    -- * Text
  , label
  , heading
  , paragraph

    -- * Input
  , button
  , checkbox
  , textInput
  , passwordInput
  , textArea
  , slider
  , spinBox
  , dropdown
  , tabBar

    -- * Other widgets
  , progressBar
  , separator
  , spacer

    -- * Layout
  , vbox
  , hbox
  , grid
  , panel
  , scroll
  , center
  , padding

    -- * Modifiers
    -- $modifiers
  , enabled
  , expand
  , minSize
  , tooltip
  , placeholder
  , keyed
  ) where

import Control.Monad.IO.Class (liftIO)
import Foreign.Ptr (nullPtr)
import Godot.Api
import qualified Godot.Api.Class.BaseButton as BaseButton
import qualified Godot.Api.Class.Button as Button
import qualified Godot.Api.Class.CenterContainer as CenterContainer
import qualified Godot.Api.Class.CheckBox as CheckBox
import qualified Godot.Api.Class.Control as Control
import qualified Godot.Api.Class.GridContainer as GridContainer
import qualified Godot.Api.Class.HBoxContainer as HBoxContainer
import qualified Godot.Api.Class.HSeparator as HSeparator
import qualified Godot.Api.Class.HSlider as HSlider
import qualified Godot.Api.Class.Label as Label
import qualified Godot.Api.Class.LineEdit as LineEdit
import qualified Godot.Api.Class.MarginContainer as MarginContainer
import qualified Godot.Api.Class.OptionButton as OptionButton
import qualified Godot.Api.Class.PanelContainer as PanelContainer
import qualified Godot.Api.Class.ProgressBar as ProgressBar
import qualified Godot.Api.Class.Range as Range
import qualified Godot.Api.Class.ScrollContainer as ScrollContainer
import qualified Godot.Api.Class.Slider as Slider
import qualified Godot.Api.Class.SpinBox as SpinBox
import qualified Godot.Api.Class.TabBar as TabBar
import qualified Godot.Api.Class.TextEdit as TextEdit
import qualified Godot.Api.Class.VBoxContainer as VBoxContainer
import qualified Godot.Api.Class.VSeparator as VSeparator
import Godot.GDExtension (GDExtensionObjectPtr (..))
import Godot.ImUI.Internal

--------------------------------------------------------------------------------
-- Helpers

-- | Sync a text property.
text :: Widget -> String -> (GString -> IO ()) -> UI ()
text w s set = prop w "text" s (\v -> toGString v >>= set)

-- | Sync whether the widget reacts to input, from 'enabled'.
enable :: Widget -> (Bool -> IO ()) -> UI ()
enable w set = do
  attrs <- getAttrs
  prop w "enabled" (attrEnabled attrs) set

-- | Sync the hint shown in an empty text field, from 'placeholder'.
hint :: Widget -> (GString -> IO ()) -> UI ()
hint w set = do
  attrs <- getAttrs
  prop w "placeholder" (attrPlaceholder attrs) (\v -> toGString v >>= set)

wide :: Sizing
wide = natural {growsHorizontally = True}

filling :: Sizing
filling = natural {growsHorizontally = True, growsVertically = True}

--------------------------------------------------------------------------------
-- Text

-- | A line of text.
label :: String -> UI ()
label s = do
  (w, l) <- widget "Label" natural $ do
    l <- Label.construct
    -- Line up with the text of buttons, text fields, ... in the same row.
    Label.setVerticalAlignment l VERTICAL_ALIGNMENT_CENTER
    pure l
  text w s (Label.setText l)

-- | A line of large text.
heading :: String -> UI ()
heading s = do
  (w, l) <- widget "Heading" natural $ do
    l <- Label.construct
    fontSize <- toStringName "font_size"
    Control.addThemeFontSizeOverride l fontSize 24
    pure l
  text w s (Label.setText l)

-- | Text that wraps over several lines to fit the available width.
paragraph :: String -> UI ()
paragraph s = do
  (w, l) <- widget "Paragraph" wide $ do
    l <- Label.construct
    Label.setAutowrapMode l TextServer_AUTOWRAP_WORD_SMART
    pure l
  text w s (Label.setText l)

--------------------------------------------------------------------------------
-- Input

-- | A button. Returns 'True' in the frame after it was clicked.
--
-- @
-- clicked <- button \"Save\"
-- when clicked (liftIO save)
-- @
button :: String -> UI Bool
button s = do
  (w, b) <- widget "Button" natural Button.construct
  text w s (Button.setText b)
  enable w (BaseButton.setDisabled b . not)
  fired w "pressed"

-- | A checkbox with a label. Takes whether it is checked and returns
-- whether it is now.
checkbox :: String -> Bool -> UI Bool
checkbox s checked = do
  (w, b) <- widget "CheckBox" natural CheckBox.construct
  text w s (Button.setText b)
  enable w (BaseButton.setDisabled b . not)
  controlled w checked (BaseButton.isPressed b) (BaseButton.setPressedNoSignal b)

-- | A single-line text field. Takes its text and returns the text after
-- the user's edits.
--
-- @
-- name' <- textInput name
-- @
textInput :: String -> UI String
textInput = lineEdit "LineEdit" False

-- | A 'textInput' that hides what is typed.
passwordInput :: String -> UI String
passwordInput = lineEdit "Password" True

lineEdit :: String -> Bool -> String -> UI String
lineEdit kind secret s = do
  (w, e) <- widget kind wide $ do
    e <- LineEdit.construct
    LineEdit.setSecret e secret
    pure e
  hint w (LineEdit.setPlaceholder e)
  enable w (LineEdit.setEditable e)
  controlled w s (LineEdit.getText e >>= fromGString) (\v -> toGString v >>= LineEdit.setText e)

-- | A multi-line text editor. Use 'expand' to let it fill the space left.
textArea :: String -> UI String
textArea s = do
  (w, e) <- widget "TextEdit" wide {defaultMinSize = (0, 120)} $ do
    e <- TextEdit.construct
    TextEdit.setLineWrappingMode e TextEdit_LINE_WRAPPING_BOUNDARY
    pure e
  hint w (TextEdit.setPlaceholder e)
  enable w (TextEdit.setEditable e)
  controlled w s (TextEdit.getText e >>= fromGString) (\v -> toGString v >>= TextEdit.setText e)

-- | A horizontal slider over a range of numbers.
--
-- @
-- volume' <- slider (0, 1) volume
-- @
slider :: (Double, Double) -> Double -> UI Double
slider (lo, hi) v = do
  (w, s) <- widget "HSlider" wide HSlider.construct
  prop w "range" (lo, hi) $ \_ -> do
    Range.setMin s lo
    Range.setMax s hi
    -- Effectively continuous, but the arrow keys still move the slider.
    Range.setStep s ((hi - lo) / 1000)
  enable w (Slider.setEditable s)
  controlled w v (Range.getValue s) (Range.setValue s)

-- | A number field with up and down arrows, for a whole number in a range.
spinBox :: (Int, Int) -> Int -> UI Int
spinBox (lo, hi) v = do
  (w, s) <- widget "SpinBox" natural SpinBox.construct
  prop w "range" (lo, hi) $ \_ -> do
    Range.setMin s (fromIntegral lo)
    Range.setMax s (fromIntegral hi)
  enable w (SpinBox.setEditable s)
  controlled w v (round <$> Range.getValue s) (Range.setValue s . fromIntegral)

-- | A dropdown list. Takes the options and the index of the selected one
-- (anything else for none) and returns the index selected now.
--
-- @
-- size' <- dropdown [\"Small\", \"Medium\", \"Large\"] size
-- @
dropdown :: [String] -> Int -> UI Int
dropdown options selected = do
  (w, o) <- widget "OptionButton" natural OptionButton.construct
  prop w "options" options $ \_ -> do
    OptionButton.clear o
    mapM_ (\s -> toGString s >>= \g -> OptionButton.addItem o g (-1)) options
    forget w "value"
  enable w (BaseButton.setDisabled o . not)
  let valid = if selected >= 0 && selected < length options then selected else -1
  controlled w valid (fromIntegral <$> OptionButton.getSelected o) (OptionButton.select o . fromIntegral)

-- | A row of tabs. Takes the tab titles and the index of the current tab
-- and returns the index of the tab that is current now. Show the tab's
-- content yourself:
--
-- @
-- tab' <- tabBar [\"General\", \"Advanced\"] tab
-- case tab' of
--   0 -> generalSettings
--   _ -> advancedSettings
-- @
tabBar :: [String] -> Int -> UI Int
tabBar titles current = do
  (w, t) <- widget "TabBar" natural TabBar.construct
  prop w "tabs" titles $ \_ -> do
    TabBar.clearTabs t
    mapM_ (\s -> toGString s >>= \g -> TabBar.addTab t g noIcon) titles
    forget w "value"
  if null titles
    then pure current
    else do
      let valid = max 0 (min (length titles - 1) current)
      controlled w valid (fromIntegral <$> TabBar.getCurrentTab t) (TabBar.setCurrentTab t . fromIntegral)
  where
    noIcon = unsafeFromObjectPtr (GDExtensionObjectPtr nullPtr) :: Texture2D

--------------------------------------------------------------------------------
-- Other widgets

-- | A progress bar, for a fraction from 0 to 1.
progressBar :: Double -> UI ()
progressBar fraction = do
  (w, p) <- widget "ProgressBar" wide $ do
    p <- ProgressBar.construct
    Range.setMax p 1
    Range.setStep p 0
    pure p
  prop w "value" fraction (Range.setValue p)

-- | A line between widgets: horizontal in a 'vbox', vertical in an 'hbox'.
separator :: UI ()
separator = do
  horizontal <- UI (pure . scopeHorizontal . envScope)
  if horizontal
    then () <$ widget "VSeparator" natural VSeparator.construct
    else () <$ widget "HSeparator" natural HSeparator.construct

-- | Empty space that takes up whatever room is left, pushing the widgets
-- after it to the end of the 'hbox' or 'vbox'.
spacer :: UI ()
spacer = () <$ widget "Spacer" filling Control.construct

--------------------------------------------------------------------------------
-- Layout

-- | Stack widgets vertically.
vbox :: UI a -> UI a
vbox = container "VBoxContainer" False natural VBoxContainer.construct

-- | Lay widgets out in a row.
--
-- @
-- hbox $ do
--   label \"Name:\"
--   name' <- textInput name
--   ...
-- @
hbox :: UI a -> UI a
hbox = container "HBoxContainer" True natural HBoxContainer.construct

-- | Lay widgets out in a grid with the given number of columns, filling it
-- row by row.
grid :: Int -> UI a -> UI a
grid columns body = do
  (w, g) <- widget "GridContainer" natural GridContainer.construct
  prop w "columns" columns (GridContainer.setColumns g . fromIntegral)
  children False w body

-- | A framed group of widgets, stacked vertically.
panel :: UI a -> UI a
panel = container "PanelContainer" False natural PanelContainer.construct . padding 8

-- | A vertically stacked group of widgets that scrolls if it does not fit.
-- Takes up the space left in its container.
scroll :: UI a -> UI a
scroll =
  container "ScrollContainer" False filling ScrollContainer.construct
    . container "VBoxContainer" False wide VBoxContainer.construct

-- | Centre a (vertically stacked) group of widgets in the space left in
-- the container.
center :: UI a -> UI a
center = container "CenterContainer" False filling CenterContainer.construct . vbox

-- | Surround a (vertically stacked) group of widgets with empty space of
-- the given width, in pixels.
padding :: Int -> UI a -> UI a
padding pixels body = do
  (w, m) <- widget "MarginContainer" natural MarginContainer.construct
  prop w "padding" pixels $ \_ ->
    mapM_
      (\side -> toStringName ("margin_" ++ side) >>= \name -> Control.addThemeConstantOverride m name (fromIntegral pixels))
      ["left", "top", "right", "bottom"]
  children False w (vbox body)

--------------------------------------------------------------------------------
-- Modifiers

-- $modifiers
-- These change the widgets created directly in the block they are applied
-- to (not the ones nested in its containers, except for 'enabled'):
--
-- @
-- 'enabled' (not (null name)) $ do
--   ok <- 'tooltip' \"Save and close\" (button \"OK\")
--   ...
-- @

-- | Whether the widgets (and the ones nested in them) react to input.
enabled :: Bool -> UI a -> UI a
enabled e = localAttrs (\a -> a {attrEnabled = attrEnabled a && e})

-- | Let the widgets grow to take up the space left in their container.
expand :: UI a -> UI a
expand = localAttrs (\a -> a {attrExpand = True})

-- | Make the widgets at least this wide and high, in pixels. (0 leaves a
-- dimension up to the widget.)
minSize :: Int -> Int -> UI a -> UI a
minSize width height = localAttrs (\a -> a {attrMinSize = (width, height)})

-- | Text shown when the mouse rests on the widgets.
tooltip :: String -> UI a -> UI a
tooltip s = localAttrs (\a -> a {attrTooltip = s})

-- | Hint shown in text fields while they are empty.
placeholder :: String -> UI a -> UI a
placeholder s = localAttrs (\a -> a {attrPlaceholder = s})

-- | Give the widgets created in the block an identity.
--
-- Widgets keep their state (keyboard focus, cursor and scroll position)
-- from frame to frame by their position among the widgets of the same kind
-- in their container. When showing a list whose items can be removed or
-- reordered, key each item's widgets by something that identifies the item
-- so the state follows it:
--
-- @
-- forM items $ \\item ->
--   keyed (show (itemId item)) $ hbox $ ...
-- @
keyed :: String -> UI a -> UI a
keyed key (UI body) = UI (\env -> body env {envKey = key})
