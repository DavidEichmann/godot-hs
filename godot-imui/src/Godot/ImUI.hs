{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

-- | A simple immediate mode UI, rendered by Godot.
--
-- The UI is a 'UI' action that runs every frame: each widget shows itself
-- and returns what the user did with it. It keeps its state with 'state'.
--
-- @
-- import Control.Monad (when)
-- import Godot.ImUI
--
-- main :: IO ()
-- main = 'runUI' 'defaultWindow' $ do
--   (count, setCount) <- 'state' (0 :: Int)
--   'label' (\"Count: \" ++ show count)
--   clicked <- 'button' \"Increment\"
--   when clicked ('liftIO' (setCount (count + 1)))
-- @
--
-- * Widgets that edit a value take the current value and return the new
--   one: @name' <- 'textInput' name@.
-- * Each widget has a variant ending in @With@ that takes its options:
--   @'buttonWith' 'buttonOpts' {enabled = False} \"OK\"@ (see "#options").
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
  , Theme (..)
  , UI
  , quit
  , liftIO
  , state
  , asyncState
  , AsyncState (..)
  , latestValue

    -- * Options #options#
    -- $options

    -- * Text
  , label
  , labelWith
  , heading
  , headingWith
  , HeadingOpts
  , headingOpts
  , paragraph
  , paragraphWith
  , ParagraphOpts
  , paragraphOpts
  , LabelOpts (..)
  , labelOpts
  , richText
  , richTextWith
  , RichTextOpts (..)
  , richTextOpts
  , image
  , imageWith
  , ImageOpts (..)
  , imageOpts

    -- * Input
  , button
  , buttonWith
  , link
  , linkWith
  , LinkOpts
  , linkOpts
  , ButtonOpts (..)
  , buttonOpts
  , checkbox
  , checkboxWith
  , toggle
  , toggleWith
  , ToggleOpts
  , toggleOpts
  , CheckboxOpts (..)
  , checkboxOpts
  , radioButtons
  , radioButtonsWith
  , RadioButtonsOpts (..)
  , radioButtonsOpts
  , textInput
  , textInputWith
  , passwordInput
  , passwordInputWith
  , PasswordInputOpts
  , passwordInputOpts
  , TextInputOpts (..)
  , textInputOpts
  , textArea
  , textAreaWith
  , TextAreaOpts (..)
  , textAreaOpts
  , slider
  , sliderWith
  , SliderOpts (..)
  , sliderOpts
  , spinBox
  , spinBoxWith
  , SpinBoxOpts (..)
  , spinBoxOpts
  , dropdown
  , dropdownWith
  , DropdownOpts (..)
  , dropdownOpts
  , listBox
  , listBoxWith
  , ListBoxOpts (..)
  , listBoxOpts
  , tabBar
  , tabBarWith
  , TabBarOpts (..)
  , tabBarOpts
  , colorPicker
  , colorPickerWith
  , ColorPickerOpts (..)
  , colorPickerOpts
  , Color (..)

    -- * Trees
  , tree
  , treeWith
  , TreeOpts (..)
  , treeOpts
  , T.Tree (..)
  , T.Forest

    -- * Menus
    -- $menus
  , menuBar
  , menuBarWith
  , MenuBarOpts (..)
  , menuBarOpts
  , menu
  , menuWith
  , MenuOpts (..)
  , menuOpts
  , menuItem
  , menuItemWith
  , menuCheck
  , menuCheckWith
  , MenuCheckOpts
  , menuCheckOpts
  , MenuItemOpts (..)
  , menuItemOpts
  , menuSeparator

    -- * Dialogs
    -- $dialogs
  , dialog
  , dialogWith
  , window
  , windowWith
  , WindowOpts
  , windowOpts
  , messageBox
  , messageBoxWith
  , MessageBoxOpts
  , messageBoxOpts
  , confirm
  , confirmWith
  , ConfirmOpts
  , confirmOpts
  , openFileDialog
  , openFileDialogWith
  , OpenFileDialogOpts
  , openFileDialogOpts
  , saveFileDialog
  , saveFileDialogWith
  , SaveFileDialogOpts
  , saveFileDialogOpts
  , directoryDialog
  , directoryDialogWith
  , DirectoryDialogOpts
  , directoryDialogOpts
  , DialogOpts (..)
  , dialogOpts
  , Choice (..)

    -- * Other widgets
  , progressBar
  , progressBarWith
  , ProgressBarOpts (..)
  , progressBarOpts
  , separator
  , separatorWith
  , SeparatorOpts (..)
  , separatorOpts
  , spacer
  , spacerWith
  , SpacerOpts (..)
  , spacerOpts

    -- * Layout
  , vbox
  , vboxWith
  , VBoxOpts
  , vboxOpts
  , hbox
  , hboxWith
  , HBoxOpts
  , hboxOpts
  , BoxOpts (..)
  , boxOpts
  , grid
  , gridWith
  , GridOpts (..)
  , gridOpts
  , flow
  , flowWith
  , FlowOpts (..)
  , flowOpts
  , panel
  , panelWith
  , PanelOpts (..)
  , panelOpts
  , collapsible
  , collapsibleWith
  , CollapsibleOpts (..)
  , collapsibleOpts
  , scroll
  , scrollWith
  , ScrollOpts (..)
  , scrollOpts
  , hsplit
  , hsplitWith
  , HSplitOpts
  , hsplitOpts
  , vsplit
  , vsplitWith
  , VSplitOpts
  , vsplitOpts
  , SplitOpts (..)
  , splitOpts
  , center
  , centerWith
  , CenterOpts (..)
  , centerOpts
  , padding
  , paddingWith
  , PaddingOpts (..)
  , paddingOpts
  ) where

import Control.Monad (forM, forM_, unless, when, zipWithM)
import Control.Monad.IO.Class (liftIO)
import Data.IORef
import Data.Int (Int64)
import Data.List (intercalate)
import Data.Maybe (isJust)
import qualified Data.Tree as T
import Godot.Api hiding (Theme)
import qualified Godot.Api.Builtin.PackedInt32Array as PackedInt32Array
import qualified Godot.Api.Class.BaseButton as BaseButton
import qualified Godot.Api.Class.Button as Button
import qualified Godot.Api.Class.ButtonGroup as ButtonGroup
import qualified Godot.Api.Class.CenterContainer as CenterContainer
import qualified Godot.Api.Class.CheckBox as CheckBox
import qualified Godot.Api.Class.CheckButton as CheckButton
import qualified Godot.Api.Class.ColorPickerButton as ColorPickerButton
import qualified Godot.Api.Class.Control as Control
import qualified Godot.Api.Class.FileDialog as FileDialog
import qualified Godot.Api.Class.FoldableContainer as FoldableContainer
import qualified Godot.Api.Class.GridContainer as GridContainer
import qualified Godot.Api.Class.HBoxContainer as HBoxContainer
import qualified Godot.Api.Class.HFlowContainer as HFlowContainer
import qualified Godot.Api.Class.HSeparator as HSeparator
import qualified Godot.Api.Class.HSlider as HSlider
import qualified Godot.Api.Class.HSplitContainer as HSplitContainer
import qualified Godot.Api.Class.Image as Image
import qualified Godot.Api.Class.ImageTexture as ImageTexture
import qualified Godot.Api.Class.ItemList as ItemList
import qualified Godot.Api.Class.Label as Label
import qualified Godot.Api.Class.LineEdit as LineEdit
import qualified Godot.Api.Class.LinkButton as LinkButton
import qualified Godot.Api.Class.MarginContainer as MarginContainer
import qualified Godot.Api.Class.MenuBar as MenuBar
import qualified Godot.Api.Class.MenuButton as MenuButton
import qualified Godot.Api.Class.OptionButton as OptionButton
import qualified Godot.Api.Class.PanelContainer as PanelContainer
import qualified Godot.Api.Class.PopupMenu as PopupMenu
import qualified Godot.Api.Class.ProgressBar as ProgressBar
import qualified Godot.Api.Class.Range as Range
import qualified Godot.Api.Class.RichTextLabel as RichTextLabel
import qualified Godot.Api.Class.ScrollContainer as ScrollContainer
import qualified Godot.Api.Class.Slider as Slider
import qualified Godot.Api.Class.SpinBox as SpinBox
import qualified Godot.Api.Class.TabBar as TabBar
import qualified Godot.Api.Class.TextEdit as TextEdit
import qualified Godot.Api.Class.TextureRect as TextureRect
import qualified Godot.Api.Class.Tree as Tree
import qualified Godot.Api.Class.TreeItem as TreeItem
import qualified Godot.Api.Class.VBoxContainer as VBoxContainer
import qualified Godot.Api.Class.VSeparator as VSeparator
import qualified Godot.Api.Class.VSplitContainer as VSplitContainer
import qualified Godot.Api.Class.Window as Window
import Godot.Api.Internal (getInterface, nullObjectPtr)
import Godot.GDExtension (GDExtensionObjectPtr, object_destroy)
import Godot.ImUI.Internal
import System.Directory (doesFileExist, getCurrentDirectory, makeAbsolute)
import System.IO (hPutStrLn, stderr)

--------------------------------------------------------------------------------
-- Options

-- $options
-- Each widget function has a variant ending in @With@ that takes the
-- widget's options as its first argument: a record, given as the default
-- options with the ones you want changed.
--
-- @
-- ok <- 'buttonWith' 'buttonOpts' {enabled = not (null name), tooltip = \"Save and close\"} \"OK\"
-- @
--
-- Most widgets have these options:
--
-- [@enabled :: Bool@] whether it reacts to input (default 'True')
-- [@tooltip :: String@] text shown when the mouse rests on it
-- [@expand :: Bool@] let it grow to take up the space left in its container
-- [@minSize :: (Int, Int)@] make it at least this wide and high, in pixels
--   (0 leaves a dimension up to the widget)
-- [@key :: String@] its identity, see below
--
-- Many option records have the same field names, so a module that sets
-- them needs @DuplicateRecordFields@. GHC then knows which record is
-- updated from the @With@ function it is passed to, but warns that it
-- will not in future (@-Wambiguous-fields@).
--
-- Widgets keep their state (keyboard focus, cursor and scroll position)
-- from frame to frame by their position among the widgets of the same kind
-- in their container. When showing a list whose items can be removed or
-- reordered, key each item's widget by something that identifies the item
-- so the state follows it (the widgets inside a keyed container go with
-- it):
--
-- @
-- forM items $ \\item ->
--   'hboxWith' 'hboxOpts' {key = show (itemId item)} $ ...
-- @

-- | Create a widget with these options: its tooltip, whether it expands,
-- its minimum size and its key.
layout :: String -> Bool -> (Int, Int) -> String -> UI a -> UI a
layout tip grows size k = withAttrs Attrs {attrTooltip = tip, attrExpand = grows, attrMinSize = size, attrKey = k}

-- | Create a node that is not laid out (a window) with this key.
keyedNode :: String -> UI a -> UI a
keyedNode k = withAttrs defaultAttrs {attrKey = k}

--------------------------------------------------------------------------------
-- Helpers

-- | Sync a text property.
syncText :: Widget -> String -> (GString -> IO ()) -> UI ()
syncText w s set = prop w "text" s (\v -> toGString v >>= set)

-- | Sync whether the widget reacts to input.
enable :: Widget -> Bool -> (Bool -> IO ()) -> UI ()
enable w e = prop w "enabled" e

-- | Sync the hint shown in an empty text field.
hint :: Widget -> String -> (GString -> IO ()) -> UI ()
hint w s set = prop w "placeholder" s (\v -> toGString v >>= set)

-- | No icon, for Godot methods that take an optional texture.
noTexture :: Texture2D
noTexture = unsafeFromObjectPtr nullObjectPtr

wide :: Sizing
wide = natural {growsHorizontally = True}

-- | Text that wraps takes the width it is given, but at least this much,
-- so that a window sized to fit its content (a 'dialog') has a width for
-- the text to wrap to.
wrapping :: Sizing
wrapping = wide {defaultMinSize = (wrapWidth, 0)}

wrapWidth :: Int
wrapWidth = 240

-- | Until it is laid out, a control has no width, and wrapping text would
-- ask for the height of a line per word. Give it its minimum width for now.
startWide :: Inherits c Control => c -> IO ()
startWide c = Control.setSize c (Vector2 (fromIntegral wrapWidth) 0) False

filling :: Sizing
filling = natural {growsHorizontally = True, growsVertically = True}

--------------------------------------------------------------------------------
-- Text

-- | The options of 'labelWith', 'headingWith' and 'paragraphWith'.
data LabelOpts = LabelOpts
  { tooltip :: String
  , expand :: Bool
  , minSize :: (Int, Int)
  , key :: String
  }

labelOpts :: LabelOpts
labelOpts = LabelOpts {tooltip = "", expand = False, minSize = (0, 0), key = ""}

-- | A line of text.
label :: String -> UI ()
label = labelWith labelOpts

labelWith :: LabelOpts -> String -> UI ()
labelWith LabelOpts {..} s = layout tooltip expand minSize key $ do
  (w, l) <- widget "Label" natural $ do
    l <- Label.construct
    -- Line up with the text of buttons, text fields, ... in the same row.
    Label.setVerticalAlignment l VERTICAL_ALIGNMENT_CENTER
    pure l
  syncText w s (Label.setText l)

-- | A line of large text.
heading :: String -> UI ()
heading = headingWith headingOpts

-- | The options of 'headingWith', the same as 'labelOpts'.
type HeadingOpts = LabelOpts

headingOpts :: HeadingOpts
headingOpts = labelOpts

headingWith :: HeadingOpts -> String -> UI ()
headingWith LabelOpts {..} s = layout tooltip expand minSize key $ do
  (w, l) <- widget "Heading" natural $ do
    l <- Label.construct
    fontSize <- toStringName "font_size"
    Control.addThemeFontSizeOverride l fontSize 24
    pure l
  syncText w s (Label.setText l)

-- | Text that wraps over several lines to fit the available width.
paragraph :: String -> UI ()
paragraph = paragraphWith paragraphOpts

-- | The options of 'paragraphWith', the same as 'labelOpts'.
type ParagraphOpts = LabelOpts

paragraphOpts :: ParagraphOpts
paragraphOpts = labelOpts

paragraphWith :: ParagraphOpts -> String -> UI ()
paragraphWith LabelOpts {..} s = layout tooltip expand minSize key $ do
  (w, l) <- widget "Paragraph" wrapping $ do
    l <- Label.construct
    Label.setAutowrapMode l TextServer_AUTOWRAP_WORD_SMART
    startWide l
    pure l
  syncText w s (Label.setText l)

-- | The options of 'richTextWith'.
data RichTextOpts = RichTextOpts
  { tooltip :: String
  , expand :: Bool
  , minSize :: (Int, Int)
  , key :: String
  }

richTextOpts :: RichTextOpts
richTextOpts = RichTextOpts {tooltip = "", expand = False, minSize = (0, 0), key = ""}

-- | Formatted text, wrapping to fit the available width. The formatting
-- is written in Godot's BBCode:
--
-- @
-- richText \"[b]Bold[/b], [i]italic[/i], [color=orange]orange[/color] and [url]a link[/url].\"
-- @
richText :: String -> UI ()
richText = richTextWith richTextOpts

richTextWith :: RichTextOpts -> String -> UI ()
richTextWith RichTextOpts {..} s = layout tooltip expand minSize key $ do
  (w, l) <- widget "RichTextLabel" wrapping $ do
    l <- RichTextLabel.construct
    RichTextLabel.setUseBbcode l True
    RichTextLabel.setFitContent l True
    startWide l
    pure l
  syncText w s (RichTextLabel.setText l)

-- | The options of 'imageWith'.
data ImageOpts = ImageOpts
  { tooltip :: String
  , expand :: Bool
  -- ^ also scales the image to fit
  , minSize :: (Int, Int)
  -- ^ also scales the image to fit
  , key :: String
  }

imageOpts :: ImageOpts
imageOpts = ImageOpts {tooltip = "", expand = False, minSize = (0, 0), key = ""}

-- | An image from a file (PNG, JPEG, SVG, WebP, ...), at its own size.
-- With the @minSize@ or @expand@ option it is scaled to fit, keeping its
-- proportions:
--
-- @
-- imageWith imageOpts {minSize = (64, 64)} \"logo.png\"
-- @
--
-- The file is read when the path changes, not every frame.
image :: FilePath -> UI ()
image = imageWith imageOpts

imageWith :: ImageOpts -> FilePath -> UI ()
imageWith ImageOpts {..} path = layout tooltip expand minSize key $ do
  (w, t) <- widget "TextureRect" natural $ do
    t <- TextureRect.construct
    TextureRect.setStretchMode t TextureRect_STRETCH_KEEP_ASPECT_CENTERED
    pure t
  let scaled = expand || minSize /= (0, 0)
  prop w "scaled" scaled $ \fit ->
    TextureRect.setExpandMode t (if fit then TextureRect_EXPAND_IGNORE_SIZE else TextureRect_EXPAND_KEEP_SIZE)
  prop w "path" path $ \file -> do
    loaded <- loadTexture file
    case loaded of
      Just texture -> do
        TextureRect.setTexture t (upcast texture)
        release texture -- the control keeps its own reference
      Nothing -> do
        TextureRect.setTexture t noTexture
        hPutStrLn stderr ("Godot.ImUI.image: cannot load " ++ file)

loadTexture :: FilePath -> IO (Maybe ImageTexture)
loadTexture path = do
  absolute <- makeAbsolute path
  exists <- doesFileExist absolute
  if not exists
    then pure Nothing
    else do
      loaded <- Image.loadFromFile =<< toGString absolute
      case loaded of
        Nothing -> pure Nothing
        Just pixels -> do
          texture <- ImageTexture.createFromImage pixels
          release pixels
          pure texture

--------------------------------------------------------------------------------
-- Input

-- | The options of 'buttonWith' and 'linkWith'.
data ButtonOpts = ButtonOpts
  { enabled :: Bool
  , tooltip :: String
  , expand :: Bool
  , minSize :: (Int, Int)
  , key :: String
  }

buttonOpts :: ButtonOpts
buttonOpts = ButtonOpts {enabled = True, tooltip = "", expand = False, minSize = (0, 0), key = ""}

-- | A button. Returns 'True' in the frame after it was clicked.
--
-- @
-- clicked <- button \"Save\"
-- when clicked (liftIO save)
-- @
button :: String -> UI Bool
button = buttonWith buttonOpts

buttonWith :: ButtonOpts -> String -> UI Bool
buttonWith ButtonOpts {..} s = layout tooltip expand minSize key $ do
  (w, b) <- widget "Button" natural Button.construct
  syncText w s (Button.setText b)
  enable w enabled (BaseButton.setDisabled b . not)
  fired w "pressed"

-- | Text that is clicked like a button. Returns 'True' in the frame after
-- it was clicked.
link :: String -> UI Bool
link = linkWith linkOpts

-- | The options of 'linkWith', the same as 'buttonOpts'.
type LinkOpts = ButtonOpts

linkOpts :: LinkOpts
linkOpts = buttonOpts

linkWith :: LinkOpts -> String -> UI Bool
linkWith ButtonOpts {..} s = layout tooltip expand minSize key $ do
  (w, b) <- widget "LinkButton" natural LinkButton.construct
  syncText w s (LinkButton.setText b)
  enable w enabled (BaseButton.setDisabled b . not)
  fired w "pressed"

-- | The options of 'checkboxWith' and 'toggleWith'.
data CheckboxOpts = CheckboxOpts
  { enabled :: Bool
  , tooltip :: String
  , expand :: Bool
  , minSize :: (Int, Int)
  , key :: String
  }

checkboxOpts :: CheckboxOpts
checkboxOpts = CheckboxOpts {enabled = True, tooltip = "", expand = False, minSize = (0, 0), key = ""}

-- | A checkbox with a label. Takes whether it is checked and returns
-- whether it is now.
checkbox :: String -> Bool -> UI Bool
checkbox = checkboxWith checkboxOpts

checkboxWith :: CheckboxOpts -> String -> Bool -> UI Bool
checkboxWith = checkable "CheckBox" CheckBox.construct

-- | An on/off switch with a label. Takes whether it is on and returns
-- whether it is now.
toggle :: String -> Bool -> UI Bool
toggle = toggleWith toggleOpts

-- | The options of 'toggleWith', the same as 'checkboxOpts'.
type ToggleOpts = CheckboxOpts

toggleOpts :: ToggleOpts
toggleOpts = checkboxOpts

toggleWith :: ToggleOpts -> String -> Bool -> UI Bool
toggleWith = checkable "CheckButton" CheckButton.construct

checkable :: (Inherits c Control, Inherits c Button, Inherits c BaseButton) => String -> IO c -> CheckboxOpts -> String -> Bool -> UI Bool
checkable kind create CheckboxOpts {..} s checked = layout tooltip expand minSize key $ do
  (w, b) <- widget kind natural create
  syncText w s (Button.setText b)
  enable w enabled (BaseButton.setDisabled b . not)
  controlled w checked (BaseButton.isPressed b) (BaseButton.setPressedNoSignal b)

-- | The options of 'radioButtonsWith'.
data RadioButtonsOpts = RadioButtonsOpts
  { enabled :: Bool
  , tooltip :: String
  , expand :: Bool
  , minSize :: (Int, Int)
  , key :: String
  }

radioButtonsOpts :: RadioButtonsOpts
radioButtonsOpts = RadioButtonsOpts {enabled = True, tooltip = "", expand = False, minSize = (0, 0), key = ""}

-- | One radio button per option, of which one is selected. Takes the
-- options and the index of the selected one (anything else for none) and
-- returns the index selected now. The buttons are laid out by the
-- container they are in.
--
-- @
-- size' <- hbox (radioButtons [\"Small\", \"Medium\", \"Large\"] size)
-- @
radioButtons :: [String] -> Int -> UI Int
radioButtons = radioButtonsWith radioButtonsOpts

radioButtonsWith :: RadioButtonsOpts -> [String] -> Int -> UI Int
radioButtonsWith RadioButtonsOpts {..} options selected = layout tooltip expand minSize key $ do
  states <- forM (zip [0 ..] options) $ \(i, s) -> do
    (w, b) <- widget "RadioButton" natural $ do
      b <- CheckBox.construct
      -- In a group (of its own) a checkbox looks like a radio button, and
      -- cannot be unchecked by clicking it.
      group <- ButtonGroup.construct
      BaseButton.setButtonGroup b group
      release group -- the button keeps its own reference
      pure b
    syncText w s (Button.setText b)
    enable w enabled (BaseButton.setDisabled b . not)
    on <- controlled w (i == selected) (BaseButton.isPressed b) (BaseButton.setPressedNoSignal b)
    pure (i, on)
  pure $ case [i | (i, True) <- states, i /= selected] of
    clicked : _ -> clicked
    [] -> selected

-- | The options of 'textInputWith' and 'passwordInputWith'.
data TextInputOpts = TextInputOpts
  { placeholder :: String
  -- ^ hint shown while it is empty
  , enabled :: Bool
  , tooltip :: String
  , expand :: Bool
  , minSize :: (Int, Int)
  , key :: String
  }

textInputOpts :: TextInputOpts
textInputOpts = TextInputOpts {placeholder = "", enabled = True, tooltip = "", expand = False, minSize = (0, 0), key = ""}

-- | A single-line text field. Takes its text and returns the text after
-- the user's edits.
--
-- @
-- name' <- textInputWith textInputOpts {placeholder = \"Your name\"} name
-- @
textInput :: String -> UI String
textInput = textInputWith textInputOpts

textInputWith :: TextInputOpts -> String -> UI String
textInputWith = lineEdit "LineEdit" False

-- | A 'textInput' that hides what is typed.
passwordInput :: String -> UI String
passwordInput = passwordInputWith passwordInputOpts

-- | The options of 'passwordInputWith', the same as 'textInputOpts'.
type PasswordInputOpts = TextInputOpts

passwordInputOpts :: PasswordInputOpts
passwordInputOpts = textInputOpts

passwordInputWith :: PasswordInputOpts -> String -> UI String
passwordInputWith = lineEdit "Password" True

lineEdit :: String -> Bool -> TextInputOpts -> String -> UI String
lineEdit kind secret TextInputOpts {..} s = layout tooltip expand minSize key $ do
  (w, e) <- widget kind wide $ do
    e <- LineEdit.construct
    LineEdit.setSecret e secret
    pure e
  hint w placeholder (LineEdit.setPlaceholder e)
  enable w enabled (LineEdit.setEditable e)
  controlled w s (LineEdit.getText e >>= fromGString) (\v -> toGString v >>= LineEdit.setText e)

-- | The options of 'textAreaWith'.
data TextAreaOpts = TextAreaOpts
  { placeholder :: String
  -- ^ hint shown while it is empty
  , enabled :: Bool
  , tooltip :: String
  , expand :: Bool
  , minSize :: (Int, Int)
  , key :: String
  }

textAreaOpts :: TextAreaOpts
textAreaOpts = TextAreaOpts {placeholder = "", enabled = True, tooltip = "", expand = False, minSize = (0, 0), key = ""}

-- | A multi-line text editor. Use the @expand@ option to let it fill the
-- space left.
textArea :: String -> UI String
textArea = textAreaWith textAreaOpts

textAreaWith :: TextAreaOpts -> String -> UI String
textAreaWith TextAreaOpts {..} s = layout tooltip expand minSize key $ do
  (w, e) <- widget "TextEdit" wide {defaultMinSize = (0, 120)} $ do
    e <- TextEdit.construct
    TextEdit.setLineWrappingMode e TextEdit_LINE_WRAPPING_BOUNDARY
    pure e
  hint w placeholder (TextEdit.setPlaceholder e)
  enable w enabled (TextEdit.setEditable e)
  controlled w s (TextEdit.getText e >>= fromGString) (\v -> toGString v >>= TextEdit.setText e)

-- | The options of 'sliderWith'.
data SliderOpts = SliderOpts
  { enabled :: Bool
  , tooltip :: String
  , expand :: Bool
  , minSize :: (Int, Int)
  , key :: String
  }

sliderOpts :: SliderOpts
sliderOpts = SliderOpts {enabled = True, tooltip = "", expand = False, minSize = (0, 0), key = ""}

-- | A horizontal slider over a range of numbers.
--
-- @
-- volume' <- slider (0, 1) volume
-- @
slider :: (Double, Double) -> Double -> UI Double
slider = sliderWith sliderOpts

sliderWith :: SliderOpts -> (Double, Double) -> Double -> UI Double
sliderWith SliderOpts {..} (lo, hi) v = layout tooltip expand minSize key $ do
  (w, s) <- widget "HSlider" wide HSlider.construct
  prop w "range" (lo, hi) $ \_ -> do
    Range.setMin s lo
    Range.setMax s hi
    -- Effectively continuous, but the arrow keys still move the slider.
    Range.setStep s ((hi - lo) / 1000)
  enable w enabled (Slider.setEditable s)
  controlled w v (Range.getValue s) (Range.setValue s)

-- | The options of 'spinBoxWith'.
data SpinBoxOpts = SpinBoxOpts
  { enabled :: Bool
  , tooltip :: String
  , expand :: Bool
  , minSize :: (Int, Int)
  , key :: String
  }

spinBoxOpts :: SpinBoxOpts
spinBoxOpts = SpinBoxOpts {enabled = True, tooltip = "", expand = False, minSize = (0, 0), key = ""}

-- | A number field with up and down arrows, for a whole number in a range.
spinBox :: (Int, Int) -> Int -> UI Int
spinBox = spinBoxWith spinBoxOpts

spinBoxWith :: SpinBoxOpts -> (Int, Int) -> Int -> UI Int
spinBoxWith SpinBoxOpts {..} (lo, hi) v = layout tooltip expand minSize key $ do
  (w, s) <- widget "SpinBox" natural SpinBox.construct
  prop w "range" (lo, hi) $ \_ -> do
    Range.setMin s (fromIntegral lo)
    Range.setMax s (fromIntegral hi)
  enable w enabled (SpinBox.setEditable s)
  controlled w v (round <$> Range.getValue s) (Range.setValue s . fromIntegral)

-- | The options of 'dropdownWith'.
data DropdownOpts = DropdownOpts
  { enabled :: Bool
  , tooltip :: String
  , expand :: Bool
  , minSize :: (Int, Int)
  , key :: String
  }

dropdownOpts :: DropdownOpts
dropdownOpts = DropdownOpts {enabled = True, tooltip = "", expand = False, minSize = (0, 0), key = ""}

-- | A dropdown list. Takes the options and the index of the selected one
-- (anything else for none) and returns the index selected now.
--
-- @
-- size' <- dropdown [\"Small\", \"Medium\", \"Large\"] size
-- @
dropdown :: [String] -> Int -> UI Int
dropdown = dropdownWith dropdownOpts

dropdownWith :: DropdownOpts -> [String] -> Int -> UI Int
dropdownWith DropdownOpts {..} options selected = layout tooltip expand minSize key $ do
  (w, o) <- widget "OptionButton" natural OptionButton.construct
  prop w "options" options $ \_ -> do
    OptionButton.clear o
    mapM_ (\s -> toGString s >>= \g -> OptionButton.addItem o g (-1)) options
    forget w "value"
  enable w enabled (BaseButton.setDisabled o . not)
  let valid = if selected >= 0 && selected < length options then selected else -1
  controlled w valid (fromIntegral <$> OptionButton.getSelected o) (OptionButton.select o . fromIntegral)

-- | The options of 'listBoxWith'.
data ListBoxOpts = ListBoxOpts
  { tooltip :: String
  , expand :: Bool
  , minSize :: (Int, Int)
  , key :: String
  }

listBoxOpts :: ListBoxOpts
listBoxOpts = ListBoxOpts {tooltip = "", expand = False, minSize = (0, 0), key = ""}

-- | A list of options, of which one is selected. Takes the options and
-- the index of the selected one (anything else for none) and returns the
-- index selected now. Use the @expand@ option to let it fill the space
-- left.
listBox :: [String] -> Int -> UI Int
listBox = listBoxWith listBoxOpts

listBoxWith :: ListBoxOpts -> [String] -> Int -> UI Int
listBoxWith ListBoxOpts {..} options selected = layout tooltip expand minSize key $ do
  (w, l) <- widget "ItemList" wide {defaultMinSize = (0, 120)} ItemList.construct
  prop w "options" options $ \_ -> do
    ItemList.clear l
    mapM_ (\s -> toGString s >>= \g -> ItemList.addItem l g noTexture True) options
    forget w "value"
  let valid = if selected >= 0 && selected < length options then selected else -1
      get = do
        chosen <- ItemList.getSelectedItems l
        n <- PackedInt32Array.size chosen
        if n == 0 then pure (-1) else fromIntegral <$> PackedInt32Array.get chosen 0
      set i
        | i < 0 = ItemList.deselectAll l
        | otherwise = ItemList.select l (fromIntegral i) True
  controlled w valid get set

-- | The options of 'colorPickerWith'.
data ColorPickerOpts = ColorPickerOpts
  { enabled :: Bool
  , tooltip :: String
  , expand :: Bool
  , minSize :: (Int, Int)
  , key :: String
  }

colorPickerOpts :: ColorPickerOpts
colorPickerOpts = ColorPickerOpts {enabled = True, tooltip = "", expand = False, minSize = (0, 0), key = ""}

-- | A button showing a colour, which opens a colour picker. Takes the
-- colour and returns the colour picked now.
--
-- @
-- colour' <- colorPicker colour     -- e.g. Color 1 0.5 0 1: opaque orange
-- @
colorPicker :: Color -> UI Color
colorPicker = colorPickerWith colorPickerOpts

colorPickerWith :: ColorPickerOpts -> Color -> UI Color
colorPickerWith ColorPickerOpts {..} colour = layout tooltip expand minSize key $ do
  (w, b) <- widget "ColorPickerButton" natural {defaultMinSize = (48, 0)} ColorPickerButton.construct
  enable w enabled (BaseButton.setDisabled b . not)
  controlled w colour (ColorPickerButton.getPickColor b) (ColorPickerButton.setPickColor b)

-- | The options of 'tabBarWith'.
data TabBarOpts = TabBarOpts
  { tooltip :: String
  , expand :: Bool
  , minSize :: (Int, Int)
  , key :: String
  }

tabBarOpts :: TabBarOpts
tabBarOpts = TabBarOpts {tooltip = "", expand = False, minSize = (0, 0), key = ""}

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
tabBar = tabBarWith tabBarOpts

tabBarWith :: TabBarOpts -> [String] -> Int -> UI Int
tabBarWith TabBarOpts {..} titles current = layout tooltip expand minSize key $ do
  (w, t) <- widget "TabBar" natural TabBar.construct
  prop w "tabs" titles $ \_ -> do
    TabBar.clearTabs t
    mapM_ (\s -> toGString s >>= \g -> TabBar.addTab t g noTexture) titles
    forget w "value"
  if null titles
    then pure current
    else do
      let valid = max 0 (min (length titles - 1) current)
      controlled w valid (fromIntegral <$> TabBar.getCurrentTab t) (TabBar.setCurrentTab t . fromIntegral)

--------------------------------------------------------------------------------
-- Trees

-- | The options of 'treeWith'.
data TreeOpts = TreeOpts
  { tooltip :: String
  , expand :: Bool
  , minSize :: (Int, Int)
  , key :: String
  }

treeOpts :: TreeOpts
treeOpts = TreeOpts {tooltip = "", expand = False, minSize = (0, 0), key = ""}

-- | A tree of items that can be collapsed, of which one is selected. Takes
-- the items (a 'T.Forest' from "Data.Tree") and the selected item, as the
-- path of indices leading to it (@Just [1, 0]@ is the first child of the
-- second top-level item), and returns the path of the item selected now.
--
-- @
-- selected' <- tree [Node \"Fruit\" [Node \"Apple\" [], Node \"Pear\" []], Node \"Bread\" []] selected
-- @
--
-- Use the @expand@ option to let it fill the space left.
tree :: T.Forest String -> Maybe [Int] -> UI (Maybe [Int])
tree = treeWith treeOpts

treeWith :: TreeOpts -> T.Forest String -> Maybe [Int] -> UI (Maybe [Int])
treeWith TreeOpts {..} forest selected = layout tooltip expand minSize key $ do
  (w, t) <- widget "Tree" wide {defaultMinSize = (0, 120)} $ do
    t <- Tree.construct
    Tree.setHideRoot t True
    pure t
  -- Godot's tree items, mirrored so that only what changed is updated
  -- (which keeps the items the user collapsed collapsed).
  items <- widgetState w "items" $ do
    Just root <- Tree.createItem t (unsafeFromObjectPtr nullObjectPtr) (-1)
    newIORef (TreeNode root "" [])
  prop w "forest" forest $ \new -> do
    TreeNode root _ old <- readIORef items
    synced <- syncTree t root old new
    writeIORef items (TreeNode root "" synced)
    forget w "value"
  TreeNode _ _ top <- liftIO (readIORef items)
  let valid = selected >>= \path -> path <$ treeNodeAt top path
      get = do
        item <- Tree.getSelected t
        pure (item >>= treePathTo top . toObjectPtr)
      set Nothing = Tree.deselectAll t
      set (Just path) = forM_ (treeNodeAt top path) (\(TreeNode item _ _) -> TreeItem.select item 0 True)
  controlled w valid get set

-- | A Godot tree item, its text and its children.
data TreeNode = TreeNode TreeItem String [TreeNode]

-- | Make the children of a tree item show the given forest.
syncTree :: Tree -> TreeItem -> [TreeNode] -> T.Forest String -> IO [TreeNode]
syncTree t parent old new = do
  kept <- zipWithM update old new
  added <- forM (drop (length old) new) $ \(T.Node s sub) -> do
    Just item <- Tree.createItem t parent (-1)
    TreeItem.setText item 0 =<< toGString s
    TreeNode item s <$> syncTree t item [] sub
  -- Freeing an item frees its children too.
  iface <- getInterface
  forM_ (drop (length new) old) $ \(TreeNode item _ _) -> object_destroy iface (toObjectPtr item)
  pure (kept ++ added)
  where
    update (TreeNode item shown sub) (T.Node s sub') = do
      when (shown /= s) (TreeItem.setText item 0 =<< toGString s)
      TreeNode item s <$> syncTree t item sub sub'

treeNodeAt :: [TreeNode] -> [Int] -> Maybe TreeNode
treeNodeAt nodes path = case path of
  [] -> Nothing
  i : rest
    | i < 0 || i >= length nodes -> Nothing
    | null rest -> Just (nodes !! i)
    | otherwise -> let TreeNode _ _ sub = nodes !! i in treeNodeAt sub rest

treePathTo :: [TreeNode] -> GDExtensionObjectPtr -> Maybe [Int]
treePathTo nodes target = case [path | (i, n) <- zip [0 ..] nodes, Just path <- [within i n]] of
  path : _ -> Just path
  [] -> Nothing
  where
    within i (TreeNode item _ sub)
      | toObjectPtr item == target = Just [i]
      | otherwise = (i :) <$> treePathTo sub target

--------------------------------------------------------------------------------
-- Menus

-- $menus
-- A menu is a block of 'menuItem's (and nested 'menu's, which are its
-- submenus). An item returns 'True' in the frame after it was chosen:
--
-- @
-- menuBar $ do
--   menu \"File\" $ do
--     open <- menuItem \"Open...\"
--     menuSeparator
--     close <- menuItem \"Quit\"
--     when close quit
--     ...
--   menu \"View\" $ do
--     wrap' <- menuCheck \"Wrap lines\" wrap
--     ...
-- @
--
-- In a 'menuBar', 'menu' adds a menu to the bar. Anywhere else it is a
-- button that opens the menu.

-- | The options of 'menuBarWith'.
data MenuBarOpts = MenuBarOpts
  { tooltip :: String
  , expand :: Bool
  , minSize :: (Int, Int)
  , key :: String
  }

menuBarOpts :: MenuBarOpts
menuBarOpts = MenuBarOpts {tooltip = "", expand = False, minSize = (0, 0), key = ""}

-- | A bar of 'menu's, usually the first thing in the window.
menuBar :: UI a -> UI a
menuBar = menuBarWith menuBarOpts

menuBarWith :: MenuBarOpts -> UI a -> UI a
menuBarWith MenuBarOpts {..} = layout tooltip expand minSize key . container "MenuBar" True natural MenuBar.construct

-- | The options of 'menuWith'. They only apply to a menu that is a button
-- (outside a 'menuBar' and a 'menu'), except for @key@.
data MenuOpts = MenuOpts
  { enabled :: Bool
  , tooltip :: String
  , expand :: Bool
  , minSize :: (Int, Int)
  , key :: String
  }

menuOpts :: MenuOpts
menuOpts = MenuOpts {enabled = True, tooltip = "", expand = False, minSize = (0, 0), key = ""}

-- | A menu with the given title, holding the items created by the block:
-- a menu of the enclosing 'menuBar', a submenu of the enclosing 'menu', or
-- otherwise a button that opens the menu.
menu :: String -> UI a -> UI a
menu = menuWith menuOpts

menuWith :: MenuOpts -> String -> UI a -> UI a
menuWith MenuOpts {..} title body = layout tooltip expand minSize key $ do
  parent <- UI (pure . scopeWidget . envScope)
  case wKind parent of
    "MenuBar" -> do
      (w, popup) <- node "PopupMenu" PopupMenu.construct
      prop w "title" title (\s -> toGString s >>= Window.setTitle popup)
      menuItems w popup body
    "PopupMenu" -> do
      (w, popup) <- node "PopupMenu" PopupMenu.construct
      _ <- menuEntry (EntrySubmenu title popup)
      menuItems w popup body
    _ -> do
      (w, b) <- widget "MenuButton" natural MenuButton.construct
      syncText w title (Button.setText b)
      enable w enabled (BaseButton.setDisabled b . not)
      -- The button has a popup of its own, which holds the items.
      popup <- widgetState w "popup" $ do
        Just popup <- MenuButton.getPopup b
        Widget "PopupMenu" "" (upcast popup) <$> newIORef [] <*> newIORef mempty
      menuItems popup (unsafeFromObjectPtr (toObjectPtr (wNode popup))) body

-- | The options of 'menuItemWith' and 'menuCheckWith'.
newtype MenuItemOpts = MenuItemOpts
  { enabled :: Bool
  }

menuItemOpts :: MenuItemOpts
menuItemOpts = MenuItemOpts {enabled = True}

-- | A menu item. Returns 'True' in the frame after it was chosen.
menuItem :: String -> UI Bool
menuItem = menuItemWith menuItemOpts

menuItemWith :: MenuItemOpts -> String -> UI Bool
menuItemWith MenuItemOpts {..} s = menuEntry (EntryItem s Nothing enabled)

-- | A menu item with a check mark. Takes whether it is checked and returns
-- whether it is now.
menuCheck :: String -> Bool -> UI Bool
menuCheck = menuCheckWith menuCheckOpts

-- | The options of 'menuCheckWith', the same as 'menuItemOpts'.
type MenuCheckOpts = MenuItemOpts

menuCheckOpts :: MenuCheckOpts
menuCheckOpts = menuItemOpts

menuCheckWith :: MenuCheckOpts -> String -> Bool -> UI Bool
menuCheckWith MenuItemOpts {..} s checked = do
  chosen <- menuEntry (EntryItem s (Just checked) enabled)
  pure (checked /= chosen)

-- | A line between menu items.
menuSeparator :: UI ()
menuSeparator = () <$ menuEntry EntrySeparator

-- | The items of a Godot popup menu are not nodes, so they are collected
-- while the menu's block runs and synced with the popup after it.
data MenuState = MenuState
  { menuShown :: IORef [MenuEntry]
  -- ^ the items the popup has
  , menuEntries :: IORef [MenuEntry]
  -- ^ the items created so far this frame, last first
  , menuChosen :: IORef [Int]
  -- ^ the indices of the items chosen since the last frame
  }

data MenuEntry
  = -- | text, whether it is checked (if it can be), enabled
    EntryItem String (Maybe Bool) Bool
  | EntrySeparator
  | EntrySubmenu String PopupMenu

menuItems :: Widget -> PopupMenu -> UI a -> UI a
menuItems w popup body = do
  st <- widgetState w "menu" (MenuState <$> newIORef [] <*> newIORef [] <*> newIORef [])
  chosen <- events w "index_pressed" $ \args -> case args of
    index : _ -> fromIntegral <$> (fromVariant index :: IO Int64)
    [] -> pure (-1)
  liftIO $ do
    writeIORef (menuChosen st) chosen
    writeIORef (menuEntries st) []
  a <- children False w body
  liftIO $ do
    shown <- readIORef (menuShown st)
    entries <- reverse <$> readIORef (menuEntries st)
    syncMenu popup shown entries
    writeIORef (menuShown st) entries
  pure a

-- | Add an entry to the enclosing menu. Returns whether it was chosen.
menuEntry :: MenuEntry -> UI Bool
menuEntry entry = do
  parent <- UI (pure . scopeWidget . envScope)
  st <- widgetState parent "menu" (fail "Godot.ImUI: menu items can only be created in a menu" :: IO MenuState)
  liftIO $ do
    entries <- readIORef (menuEntries st)
    writeIORef (menuEntries st) (entry : entries)
    elem (length entries) <$> readIORef (menuChosen st)

syncMenu :: PopupMenu -> [MenuEntry] -> [MenuEntry] -> IO ()
syncMenu popup shown entries
  -- The same items: update them in place (the menu may be open).
  | length shown == length entries && and (zipWith sameKind shown entries) =
      forM_ (zip3 [0 ..] shown entries) $ \(i, old, new) -> case (old, new) of
        (EntryItem s checked enable', EntryItem s' checked' enable'') -> do
          when (s /= s') (PopupMenu.setItemText popup i =<< toGString s')
          when (checked /= checked') (PopupMenu.setItemChecked popup i (checked' == Just True))
          when (enable' /= enable'') (PopupMenu.setItemDisabled popup i (not enable''))
        (EntrySubmenu s _, EntrySubmenu s' _) ->
          when (s /= s') (PopupMenu.setItemText popup i =<< toGString s')
        _ -> pure ()
  | otherwise = do
      PopupMenu.clear popup False
      forM_ (zip [0 ..] entries) $ \(i, entry) -> case entry of
        EntryItem s checked enable' -> do
          g <- toGString s
          case checked of
            Nothing -> PopupMenu.addItem popup g (-1) KEY_NONE
            Just on -> PopupMenu.addCheckItem popup g (-1) KEY_NONE >> PopupMenu.setItemChecked popup i on
          unless enable' (PopupMenu.setItemDisabled popup i True)
        EntrySeparator -> toGString "" >>= \g -> PopupMenu.addSeparator popup g (-1)
        EntrySubmenu s submenu -> toGString s >>= \g -> PopupMenu.addSubmenuNodeItem popup g submenu (-1)
  where
    sameKind (EntryItem _ a _) (EntryItem _ b _) = isJust a == isJust b
    sameKind EntrySeparator EntrySeparator = True
    sameKind (EntrySubmenu _ a) (EntrySubmenu _ b) = a == b
    sameKind _ _ = False

--------------------------------------------------------------------------------
-- Dialogs

-- $dialogs
-- A dialog is open for as long as it is created: keep whether it is open
-- in your state, and stop creating it when it is done.
--
-- @
-- about' <-
--   if not about
--     then pure False
--     else do
--       closed <- messageBox \"About\" \"Made with godot-imui.\"
--       pure (not closed)
-- @

-- | The options of the dialogs ('dialogWith', 'windowWith',
-- 'messageBoxWith', ...).
newtype DialogOpts = DialogOpts
  { key :: String
  }

dialogOpts :: DialogOpts
dialogOpts = DialogOpts {key = ""}

-- | A dialog window with the given title, holding the (vertically
-- stacked) widgets created by the block. The rest of the UI cannot be used
-- while it is open.
--
-- Returns the block's result, or 'Nothing' in the frame after the user
-- closed the window.
dialog :: String -> UI a -> UI (Maybe a)
dialog = dialogWith dialogOpts

dialogWith :: DialogOpts -> String -> UI a -> UI (Maybe a)
dialogWith = subwindow "Dialog" True

-- | Like 'dialog', but the rest of the UI can be used while it is open.
window :: String -> UI a -> UI (Maybe a)
window = windowWith windowOpts

-- | The options of 'windowWith', the same as 'dialogOpts'.
type WindowOpts = DialogOpts

windowOpts :: WindowOpts
windowOpts = dialogOpts

windowWith :: WindowOpts -> String -> UI a -> UI (Maybe a)
windowWith = subwindow "Window" False

subwindow :: String -> Bool -> DialogOpts -> String -> UI a -> UI (Maybe a)
subwindow kind modal DialogOpts {..} title body = keyedNode key $ do
  (w, win) <- node kind $ do
    win <- Window.construct
    Window.setVisible win False
    Window.setWrapControls win True -- as large as its content needs
    Window.setTransient win True
    Window.setExclusive win modal
    pure win
  prop w "title" title (\s -> toGString s >>= Window.setTitle win)
  content <- widgetState w "content" (contentRoot (upcast win))
  a <- children False content body
  -- Shown once it has its content, so that it is centred at its real size,
  -- and once a dialog that is no longer created is gone: only one can
  -- block the UI at a time.
  later <- afterFrame
  prop w "shown" True (\_ -> later (Window.popupCentered win (Vector2i 0 0)))
  closed <- fired w "close_requested"
  pure (if closed then Nothing else Just a)

-- | A 'dialog' with a message and an OK button. Returns 'True' in the
-- frame after it was dismissed.
messageBox :: String -> String -> UI Bool
messageBox = messageBoxWith messageBoxOpts

-- | The options of 'messageBoxWith', the same as 'dialogOpts'.
type MessageBoxOpts = DialogOpts

messageBoxOpts :: MessageBoxOpts
messageBoxOpts = dialogOpts

messageBoxWith :: MessageBoxOpts -> String -> String -> UI Bool
messageBoxWith opts title message = do
  ok <- dialogWith opts title $ do
    label message
    hbox (spacer >> button "OK")
  pure (ok /= Just False)

-- | A 'dialog' with a question and OK and Cancel buttons. Returns
-- 'Nothing' until it is answered, then whether it was OK.
confirm :: String -> String -> UI (Maybe Bool)
confirm = confirmWith confirmOpts

-- | The options of 'confirmWith', the same as 'dialogOpts'.
type ConfirmOpts = DialogOpts

confirmOpts :: ConfirmOpts
confirmOpts = dialogOpts

confirmWith :: ConfirmOpts -> String -> String -> UI (Maybe Bool)
confirmWith opts title question = do
  answer <- dialogWith opts title $ do
    label question
    hbox $ do
      spacer
      ok <- button "OK"
      cancel <- button "Cancel"
      pure (if ok then Just True else if cancel then Just False else Nothing)
  pure (maybe (Just False) id answer)

-- | The state of a dialog that asks for something.
data Choice a
  = -- | the dialog is still open
    Choosing
  | Cancelled
  | Chose a
  deriving (Eq, Show)

-- | A dialog for choosing an existing file. Takes the title and the file
-- name patterns to show, e.g. @[\"*.png\", \"*.jpg\"]@ (none for all files).
--
-- @
-- choice <- openFileDialog \"Open image\" [\"*.png\"]
-- case choice of
--   Choosing -> ...       -- still open
--   Cancelled -> ...      -- stop creating the dialog
--   Chose path -> ...     -- stop creating the dialog, and use the path
-- @
openFileDialog :: String -> [String] -> UI (Choice FilePath)
openFileDialog = openFileDialogWith openFileDialogOpts

-- | The options of 'openFileDialogWith', the same as 'dialogOpts'.
type OpenFileDialogOpts = DialogOpts

openFileDialogOpts :: OpenFileDialogOpts
openFileDialogOpts = dialogOpts

openFileDialogWith :: OpenFileDialogOpts -> String -> [String] -> UI (Choice FilePath)
openFileDialogWith = fileDialog "OpenFileDialog" FileDialog_FILE_MODE_OPEN_FILE "file_selected"

-- | A dialog for choosing where to save a file (asking before replacing
-- an existing one). Takes the title and the file name patterns to show.
saveFileDialog :: String -> [String] -> UI (Choice FilePath)
saveFileDialog = saveFileDialogWith saveFileDialogOpts

-- | The options of 'saveFileDialogWith', the same as 'dialogOpts'.
type SaveFileDialogOpts = DialogOpts

saveFileDialogOpts :: SaveFileDialogOpts
saveFileDialogOpts = dialogOpts

saveFileDialogWith :: SaveFileDialogOpts -> String -> [String] -> UI (Choice FilePath)
saveFileDialogWith = fileDialog "SaveFileDialog" FileDialog_FILE_MODE_SAVE_FILE "file_selected"

-- | A dialog for choosing a directory.
directoryDialog :: String -> UI (Choice FilePath)
directoryDialog = directoryDialogWith directoryDialogOpts

-- | The options of 'directoryDialogWith', the same as 'dialogOpts'.
type DirectoryDialogOpts = DialogOpts

directoryDialogOpts :: DirectoryDialogOpts
directoryDialogOpts = dialogOpts

directoryDialogWith :: DirectoryDialogOpts -> String -> UI (Choice FilePath)
directoryDialogWith opts title = fileDialog "DirectoryDialog" FileDialog_FILE_MODE_OPEN_DIR "dir_selected" opts title []

fileDialog :: String -> FileDialog_FileMode -> String -> DialogOpts -> String -> [String] -> UI (Choice FilePath)
fileDialog kind mode signal DialogOpts {..} title patterns = keyedNode key $ do
  (w, d) <- node kind $ do
    d <- FileDialog.construct
    Window.setVisible d False
    FileDialog.setAccess d FileDialog_ACCESS_FILESYSTEM
    FileDialog.setFileMode d mode
    FileDialog.setModeOverridesTitle d False
    FileDialog.setCurrentDir d =<< toGString =<< getCurrentDirectory
    pure d
  prop w "title" title (\s -> toGString s >>= Window.setTitle d)
  prop w "patterns" patterns $ \ps -> do
    FileDialog.clearFilters d
    unless (null ps) $ do
      filters <- toGString (intercalate ", " ps)
      none <- toGString ""
      FileDialog.addFilter d filters none none
  chosen <- events w signal $ \args -> case args of
    path : _ -> (fromVariant path :: IO GString) >>= fromGString
    [] -> pure ""
  cancelled <- fired w "canceled"
  later <- afterFrame
  prop w "shown" True (\_ -> later (Window.popupCentered d (Vector2i 720 480)))
  -- Godot hides the dialog when it is answered. Should it still be created
  -- after that, show it again.
  let answered choice = liftIO (forget w "shown") >> pure choice
  case chosen of
    path : _ -> answered (Chose path)
    []
      | cancelled -> answered Cancelled
      | otherwise -> pure Choosing

--------------------------------------------------------------------------------
-- Other widgets

-- | The options of 'progressBarWith'.
data ProgressBarOpts = ProgressBarOpts
  { tooltip :: String
  , expand :: Bool
  , minSize :: (Int, Int)
  , key :: String
  , indeterminate :: Bool
  -- ^ show that something is going on, with an animation, instead of how
  -- far it got (the fraction is then ignored)
  }

progressBarOpts :: ProgressBarOpts
progressBarOpts = ProgressBarOpts {tooltip = "", expand = False, minSize = (0, 0), key = "", indeterminate = False}

-- | A progress bar, for a fraction from 0 to 1. (With the @indeterminate@
-- option, an animation for work whose progress is not known.)
progressBar :: Double -> UI ()
progressBar = progressBarWith progressBarOpts

progressBarWith :: ProgressBarOpts -> Double -> UI ()
progressBarWith ProgressBarOpts {..} fraction = layout tooltip expand minSize key $ do
  (w, p) <- widget "ProgressBar" wide $ do
    p <- ProgressBar.construct
    Range.setMax p 1
    Range.setStep p 0
    pure p
  prop w "value" fraction (Range.setValue p)
  prop w "indeterminate" indeterminate (ProgressBar.setIndeterminate p)

-- | The options of 'separatorWith'.
data SeparatorOpts = SeparatorOpts
  { tooltip :: String
  , expand :: Bool
  , minSize :: (Int, Int)
  , key :: String
  }

separatorOpts :: SeparatorOpts
separatorOpts = SeparatorOpts {tooltip = "", expand = False, minSize = (0, 0), key = ""}

-- | A line between widgets: horizontal in a 'vbox', vertical in an 'hbox'.
separator :: UI ()
separator = separatorWith separatorOpts

separatorWith :: SeparatorOpts -> UI ()
separatorWith SeparatorOpts {..} = layout tooltip expand minSize key $ do
  horizontal <- UI (pure . scopeHorizontal . envScope)
  if horizontal
    then () <$ widget "VSeparator" natural VSeparator.construct
    else () <$ widget "HSeparator" natural HSeparator.construct

-- | The options of 'spacerWith'.
data SpacerOpts = SpacerOpts
  { tooltip :: String
  , expand :: Bool
  , minSize :: (Int, Int)
  , key :: String
  }

spacerOpts :: SpacerOpts
spacerOpts = SpacerOpts {tooltip = "", expand = False, minSize = (0, 0), key = ""}

-- | Empty space that takes up whatever room is left, pushing the widgets
-- after it to the end of the 'hbox' or 'vbox'.
spacer :: UI ()
spacer = spacerWith spacerOpts

spacerWith :: SpacerOpts -> UI ()
spacerWith SpacerOpts {..} = layout tooltip expand minSize key (() <$ widget "Spacer" filling Control.construct)

--------------------------------------------------------------------------------
-- Layout

-- | The options of 'vboxWith' and 'hboxWith'.
data BoxOpts = BoxOpts
  { tooltip :: String
  , expand :: Bool
  , minSize :: (Int, Int)
  , key :: String
  }

boxOpts :: BoxOpts
boxOpts = BoxOpts {tooltip = "", expand = False, minSize = (0, 0), key = ""}

-- | Stack widgets vertically.
vbox :: UI a -> UI a
vbox = vboxWith vboxOpts

-- | The options of 'vboxWith', the same as 'boxOpts'.
type VBoxOpts = BoxOpts

vboxOpts :: VBoxOpts
vboxOpts = boxOpts

vboxWith :: VBoxOpts -> UI a -> UI a
vboxWith BoxOpts {..} = layout tooltip expand minSize key . column

-- | A vertical box, with no options.
column :: UI a -> UI a
column = container "VBoxContainer" False natural VBoxContainer.construct

-- | Lay widgets out in a row.
--
-- @
-- hbox $ do
--   label \"Name:\"
--   name' <- textInput name
--   ...
-- @
hbox :: UI a -> UI a
hbox = hboxWith hboxOpts

-- | The options of 'hboxWith', the same as 'boxOpts'.
type HBoxOpts = BoxOpts

hboxOpts :: HBoxOpts
hboxOpts = boxOpts

hboxWith :: HBoxOpts -> UI a -> UI a
hboxWith BoxOpts {..} = layout tooltip expand minSize key . container "HBoxContainer" True natural HBoxContainer.construct

-- | The options of 'gridWith'.
data GridOpts = GridOpts
  { tooltip :: String
  , expand :: Bool
  , minSize :: (Int, Int)
  , key :: String
  }

gridOpts :: GridOpts
gridOpts = GridOpts {tooltip = "", expand = False, minSize = (0, 0), key = ""}

-- | Lay widgets out in a grid with the given number of columns, filling it
-- row by row.
grid :: Int -> UI a -> UI a
grid = gridWith gridOpts

gridWith :: GridOpts -> Int -> UI a -> UI a
gridWith GridOpts {..} columns body = layout tooltip expand minSize key $ do
  (w, g) <- widget "GridContainer" natural GridContainer.construct
  prop w "columns" columns (GridContainer.setColumns g . fromIntegral)
  children False w body

-- | The options of 'flowWith'.
data FlowOpts = FlowOpts
  { tooltip :: String
  , expand :: Bool
  , minSize :: (Int, Int)
  , key :: String
  }

flowOpts :: FlowOpts
flowOpts = FlowOpts {tooltip = "", expand = False, minSize = (0, 0), key = ""}

-- | Lay widgets out in a row that wraps onto further rows when it is too
-- long, like words in a paragraph.
flow :: UI a -> UI a
flow = flowWith flowOpts

flowWith :: FlowOpts -> UI a -> UI a
flowWith FlowOpts {..} = layout tooltip expand minSize key . container "HFlowContainer" True natural HFlowContainer.construct

-- | The options of 'collapsibleWith'.
data CollapsibleOpts = CollapsibleOpts
  { tooltip :: String
  , expand :: Bool
  , minSize :: (Int, Int)
  , key :: String
  }

collapsibleOpts :: CollapsibleOpts
collapsibleOpts = CollapsibleOpts {tooltip = "", expand = False, minSize = (0, 0), key = ""}

-- | A (vertically stacked) group of widgets under a title that the user
-- can click to fold the group away.
collapsible :: String -> UI a -> UI a
collapsible = collapsibleWith collapsibleOpts

collapsibleWith :: CollapsibleOpts -> String -> UI a -> UI a
collapsibleWith CollapsibleOpts {..} title body = layout tooltip expand minSize key $ do
  (w, f) <- widget "FoldableContainer" natural FoldableContainer.construct
  syncText w title (FoldableContainer.setTitle f)
  children False w (column body)

-- | The options of 'hsplitWith' and 'vsplitWith'.
data SplitOpts = SplitOpts
  { tooltip :: String
  , expand :: Bool
  , minSize :: (Int, Int)
  , key :: String
  }

splitOpts :: SplitOpts
splitOpts = SplitOpts {tooltip = "", expand = False, minSize = (0, 0), key = ""}

-- | Two (vertically stacked) groups of widgets side by side, with a
-- divider between them that the user can drag. Takes up the space left in
-- its container.
--
-- @
-- (selected', ()) <- hsplit (treeWith treeOpts {expand = True} items selected) (details selected)
-- @
hsplit :: UI a -> UI b -> UI (a, b)
hsplit = hsplitWith hsplitOpts

-- | The options of 'hsplitWith', the same as 'splitOpts'.
type HSplitOpts = SplitOpts

hsplitOpts :: HSplitOpts
hsplitOpts = splitOpts

hsplitWith :: HSplitOpts -> UI a -> UI b -> UI (a, b)
hsplitWith = split "HSplitContainer" True HSplitContainer.construct

-- | Like 'hsplit', with one group above the other.
vsplit :: UI a -> UI b -> UI (a, b)
vsplit = vsplitWith vsplitOpts

-- | The options of 'vsplitWith', the same as 'splitOpts'.
type VSplitOpts = SplitOpts

vsplitOpts :: VSplitOpts
vsplitOpts = splitOpts

vsplitWith :: VSplitOpts -> UI a -> UI b -> UI (a, b)
vsplitWith = split "VSplitContainer" False VSplitContainer.construct

split :: Inherits c Control => String -> Bool -> IO c -> SplitOpts -> UI a -> UI b -> UI (a, b)
split kind horizontal create SplitOpts {..} first second =
  layout tooltip expand minSize key $
    container kind horizontal filling create ((,) <$> side first <*> side second)
  where
    side :: UI x -> UI x
    side = container "VBoxContainer" False filling VBoxContainer.construct

-- | The options of 'panelWith'.
data PanelOpts = PanelOpts
  { tooltip :: String
  , expand :: Bool
  , minSize :: (Int, Int)
  , key :: String
  }

panelOpts :: PanelOpts
panelOpts = PanelOpts {tooltip = "", expand = False, minSize = (0, 0), key = ""}

-- | A framed group of widgets, stacked vertically.
panel :: UI a -> UI a
panel = panelWith panelOpts

panelWith :: PanelOpts -> UI a -> UI a
panelWith PanelOpts {..} = layout tooltip expand minSize key . container "PanelContainer" False natural PanelContainer.construct . padded 8

-- | The options of 'scrollWith'.
data ScrollOpts = ScrollOpts
  { tooltip :: String
  , expand :: Bool
  , minSize :: (Int, Int)
  , key :: String
  }

scrollOpts :: ScrollOpts
scrollOpts = ScrollOpts {tooltip = "", expand = False, minSize = (0, 0), key = ""}

-- | A vertically stacked group of widgets that scrolls if it does not fit.
-- Takes up the space left in its container.
scroll :: UI a -> UI a
scroll = scrollWith scrollOpts

scrollWith :: ScrollOpts -> UI a -> UI a
scrollWith ScrollOpts {..} =
  layout tooltip expand minSize key
    . container "ScrollContainer" False filling ScrollContainer.construct
    . container "VBoxContainer" False wide VBoxContainer.construct

-- | The options of 'centerWith'.
data CenterOpts = CenterOpts
  { tooltip :: String
  , expand :: Bool
  , minSize :: (Int, Int)
  , key :: String
  }

centerOpts :: CenterOpts
centerOpts = CenterOpts {tooltip = "", expand = False, minSize = (0, 0), key = ""}

-- | Centre a (vertically stacked) group of widgets in the space left in
-- the container.
center :: UI a -> UI a
center = centerWith centerOpts

centerWith :: CenterOpts -> UI a -> UI a
centerWith CenterOpts {..} = layout tooltip expand minSize key . container "CenterContainer" False filling CenterContainer.construct . column

-- | The options of 'paddingWith'.
data PaddingOpts = PaddingOpts
  { tooltip :: String
  , expand :: Bool
  , minSize :: (Int, Int)
  , key :: String
  }

paddingOpts :: PaddingOpts
paddingOpts = PaddingOpts {tooltip = "", expand = False, minSize = (0, 0), key = ""}

-- | Surround a (vertically stacked) group of widgets with empty space of
-- the given width, in pixels.
padding :: Int -> UI a -> UI a
padding = paddingWith paddingOpts

paddingWith :: PaddingOpts -> Int -> UI a -> UI a
paddingWith PaddingOpts {..} pixels = layout tooltip expand minSize key . padded pixels

-- | Surround a vertical box with empty space.
padded :: Int -> UI a -> UI a
padded pixels body = do
  (w, m) <- widget "MarginContainer" natural MarginContainer.construct
  prop w "padding" pixels $ \_ ->
    mapM_
      (\side -> toStringName ("margin_" ++ side) >>= \name -> Control.addThemeConstantOverride m name (fromIntegral pixels))
      ["left", "top", "right", "bottom"]
  children False w (column body)
