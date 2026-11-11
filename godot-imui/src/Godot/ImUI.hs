{-# LANGUAGE FlexibleContexts #-}

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
  , Theme (..)
  , UI
  , quit
  , liftIO

    -- * Text
  , label
  , heading
  , paragraph

  , richText
  , image

    -- * Input
  , button
  , link
  , checkbox
  , toggle
  , radioButtons
  , textInput
  , passwordInput
  , textArea
  , slider
  , spinBox
  , dropdown
  , listBox
  , tabBar
  , colorPicker
  , Color (..)

    -- * Trees
  , tree
  , T.Tree (..)
  , T.Forest

    -- * Menus
    -- $menus
  , menuBar
  , menu
  , menuItem
  , menuCheck
  , menuSeparator

    -- * Dialogs
    -- $dialogs
  , dialog
  , window
  , messageBox
  , confirm
  , openFileDialog
  , saveFileDialog
  , directoryDialog
  , Choice (..)

    -- * Other widgets
  , progressBar
  , separator
  , spacer

    -- * Layout
  , vbox
  , hbox
  , grid
  , flow
  , panel
  , collapsible
  , scroll
  , hsplit
  , vsplit
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
  (w, l) <- widget "Paragraph" wrapping $ do
    l <- Label.construct
    Label.setAutowrapMode l TextServer_AUTOWRAP_WORD_SMART
    startWide l
    pure l
  text w s (Label.setText l)

-- | Formatted text, wrapping to fit the available width. The formatting
-- is written in Godot's BBCode:
--
-- @
-- richText \"[b]Bold[/b], [i]italic[/i], [color=orange]orange[/color] and [url]a link[/url].\"
-- @
richText :: String -> UI ()
richText s = do
  (w, l) <- widget "RichTextLabel" wrapping $ do
    l <- RichTextLabel.construct
    RichTextLabel.setUseBbcode l True
    RichTextLabel.setFitContent l True
    startWide l
    pure l
  text w s (RichTextLabel.setText l)

-- | An image from a file (PNG, JPEG, SVG, WebP, ...), at its own size.
-- With 'minSize' or 'expand' it is scaled to fit, keeping its proportions:
--
-- @
-- minSize 64 64 (image \"logo.png\")
-- @
--
-- The file is read when the path changes, not every frame.
image :: FilePath -> UI ()
image path = do
  attrs <- getAttrs
  (w, t) <- widget "TextureRect" natural $ do
    t <- TextureRect.construct
    TextureRect.setStretchMode t TextureRect_STRETCH_KEEP_ASPECT_CENTERED
    pure t
  let scaled = attrExpand attrs || attrMinSize attrs /= (0, 0)
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

-- | Text that is clicked like a button. Returns 'True' in the frame after
-- it was clicked.
link :: String -> UI Bool
link s = do
  (w, b) <- widget "LinkButton" natural LinkButton.construct
  text w s (LinkButton.setText b)
  enable w (BaseButton.setDisabled b . not)
  fired w "pressed"

-- | A checkbox with a label. Takes whether it is checked and returns
-- whether it is now.
checkbox :: String -> Bool -> UI Bool
checkbox = checkable "CheckBox" CheckBox.construct

-- | An on/off switch with a label. Takes whether it is on and returns
-- whether it is now.
toggle :: String -> Bool -> UI Bool
toggle = checkable "CheckButton" CheckButton.construct

checkable :: (Inherits c Control, Inherits c Button, Inherits c BaseButton) => String -> IO c -> String -> Bool -> UI Bool
checkable kind create s checked = do
  (w, b) <- widget kind natural create
  text w s (Button.setText b)
  enable w (BaseButton.setDisabled b . not)
  controlled w checked (BaseButton.isPressed b) (BaseButton.setPressedNoSignal b)

-- | One radio button per option, of which one is selected. Takes the
-- options and the index of the selected one (anything else for none) and
-- returns the index selected now. The buttons are laid out by the
-- container they are in.
--
-- @
-- size' <- hbox (radioButtons [\"Small\", \"Medium\", \"Large\"] size)
-- @
radioButtons :: [String] -> Int -> UI Int
radioButtons options selected = do
  states <- forM (zip [0 ..] options) $ \(i, s) -> do
    (w, b) <- widget "RadioButton" natural $ do
      b <- CheckBox.construct
      -- In a group (of its own) a checkbox looks like a radio button, and
      -- cannot be unchecked by clicking it.
      BaseButton.setButtonGroup b =<< ButtonGroup.construct
      pure b
    text w s (Button.setText b)
    enable w (BaseButton.setDisabled b . not)
    on <- controlled w (i == selected) (BaseButton.isPressed b) (BaseButton.setPressedNoSignal b)
    pure (i, on)
  pure $ case [i | (i, True) <- states, i /= selected] of
    clicked : _ -> clicked
    [] -> selected

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

-- | A list of options, of which one is selected. Takes the options and
-- the index of the selected one (anything else for none) and returns the
-- index selected now. Use 'expand' to let it fill the space left.
listBox :: [String] -> Int -> UI Int
listBox options selected = do
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

-- | A button showing a colour, which opens a colour picker. Takes the
-- colour and returns the colour picked now.
--
-- @
-- colour' <- colorPicker colour     -- e.g. Color 1 0.5 0 1: opaque orange
-- @
colorPicker :: Color -> UI Color
colorPicker colour = do
  (w, b) <- widget "ColorPickerButton" natural {defaultMinSize = (48, 0)} ColorPickerButton.construct
  enable w (BaseButton.setDisabled b . not)
  controlled w colour (ColorPickerButton.getPickColor b) (ColorPickerButton.setPickColor b)

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
    mapM_ (\s -> toGString s >>= \g -> TabBar.addTab t g noTexture) titles
    forget w "value"
  if null titles
    then pure current
    else do
      let valid = max 0 (min (length titles - 1) current)
      controlled w valid (fromIntegral <$> TabBar.getCurrentTab t) (TabBar.setCurrentTab t . fromIntegral)

--------------------------------------------------------------------------------
-- Trees

-- | A tree of items that can be collapsed, of which one is selected. Takes
-- the items (a 'T.Forest' from "Data.Tree") and the selected item, as the
-- path of indices leading to it (@Just [1, 0]@ is the first child of the
-- second top-level item), and returns the path of the item selected now.
--
-- @
-- selected' <- tree [Node \"Fruit\" [Node \"Apple\" [], Node \"Pear\" []], Node \"Bread\" []] selected
-- @
--
-- Use 'expand' to let it fill the space left.
tree :: T.Forest String -> Maybe [Int] -> UI (Maybe [Int])
tree forest selected = do
  (w, t) <- widget "Tree" wide {defaultMinSize = (0, 120)} $ do
    t <- Tree.construct
    Tree.setHideRoot t True
    pure t
  -- Godot's tree items, mirrored so that only what changed is updated
  -- (which keeps the items the user collapsed collapsed).
  items <- state w "items" $ do
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

-- | A bar of 'menu's, usually the first thing in the window.
menuBar :: UI a -> UI a
menuBar = container "MenuBar" True natural MenuBar.construct

-- | A menu with the given title, holding the items created by the block:
-- a menu of the enclosing 'menuBar', a submenu of the enclosing 'menu', or
-- otherwise a button that opens the menu.
menu :: String -> UI a -> UI a
menu title body = do
  parent <- UI (pure . scopeWidget . envScope)
  case wKind parent of
    "MenuBar" -> do
      (w, popup) <- node "PopupMenu" PopupMenu.construct
      prop w "title" title (\s -> toGString s >>= Window.setTitle popup)
      menuItems w popup body
    "PopupMenu" -> do
      (w, popup) <- node "PopupMenu" PopupMenu.construct
      _ <- menuEntry (\_ -> EntrySubmenu title popup)
      menuItems w popup body
    _ -> do
      (w, b) <- widget "MenuButton" natural MenuButton.construct
      text w title (Button.setText b)
      enable w (BaseButton.setDisabled b . not)
      -- The button has a popup of its own, which holds the items.
      popup <- state w "popup" $ do
        Just popup <- MenuButton.getPopup b
        Widget "PopupMenu" "" (upcast popup) <$> newIORef [] <*> newIORef mempty
      menuItems popup (unsafeFromObjectPtr (toObjectPtr (wNode popup))) body

-- | A menu item. Returns 'True' in the frame after it was chosen.
menuItem :: String -> UI Bool
menuItem s = menuEntry (EntryItem s Nothing)

-- | A menu item with a check mark. Takes whether it is checked and returns
-- whether it is now.
menuCheck :: String -> Bool -> UI Bool
menuCheck s checked = do
  chosen <- menuEntry (EntryItem s (Just checked))
  pure (checked /= chosen)

-- | A line between menu items.
menuSeparator :: UI ()
menuSeparator = () <$ menuEntry (const EntrySeparator)

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
  st <- state w "menu" (MenuState <$> newIORef [] <*> newIORef [] <*> newIORef [])
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
menuEntry :: (Bool -> MenuEntry) -> UI Bool
menuEntry entry = do
  parent <- UI (pure . scopeWidget . envScope)
  st <- state parent "menu" (fail "Godot.ImUI: menu items can only be created in a menu" :: IO MenuState)
  attrs <- getAttrs
  liftIO $ do
    entries <- readIORef (menuEntries st)
    writeIORef (menuEntries st) (entry (attrEnabled attrs) : entries)
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

-- | A dialog window with the given title, holding the (vertically
-- stacked) widgets created by the block. The rest of the UI cannot be used
-- while it is open.
--
-- Returns the block's result, or 'Nothing' in the frame after the user
-- closed the window.
dialog :: String -> UI a -> UI (Maybe a)
dialog = subwindow "Dialog" True

-- | Like 'dialog', but the rest of the UI can be used while it is open.
window :: String -> UI a -> UI (Maybe a)
window = subwindow "Window" False

subwindow :: String -> Bool -> String -> UI a -> UI (Maybe a)
subwindow kind modal title body = do
  (w, win) <- node kind $ do
    win <- Window.construct
    Window.setVisible win False
    Window.setWrapControls win True -- as large as its content needs
    Window.setTransient win True
    Window.setExclusive win modal
    pure win
  prop w "title" title (\s -> toGString s >>= Window.setTitle win)
  content <- state w "content" (contentRoot (upcast win))
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
messageBox title message = do
  ok <- dialog title $ do
    label message
    hbox (spacer >> button "OK")
  pure (ok /= Just False)

-- | A 'dialog' with a question and OK and Cancel buttons. Returns
-- 'Nothing' until it is answered, then whether it was OK.
confirm :: String -> String -> UI (Maybe Bool)
confirm title question = do
  answer <- dialog title $ do
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
openFileDialog = fileDialog "OpenFileDialog" FileDialog_FILE_MODE_OPEN_FILE "file_selected"

-- | A dialog for choosing where to save a file (asking before replacing
-- an existing one). Takes the title and the file name patterns to show.
saveFileDialog :: String -> [String] -> UI (Choice FilePath)
saveFileDialog = fileDialog "SaveFileDialog" FileDialog_FILE_MODE_SAVE_FILE "file_selected"

-- | A dialog for choosing a directory.
directoryDialog :: String -> UI (Choice FilePath)
directoryDialog title = fileDialog "DirectoryDialog" FileDialog_FILE_MODE_OPEN_DIR "dir_selected" title []

fileDialog :: String -> FileDialog_FileMode -> String -> String -> [String] -> UI (Choice FilePath)
fileDialog kind mode signal title patterns = do
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

-- | Lay widgets out in a row that wraps onto further rows when it is too
-- long, like words in a paragraph.
flow :: UI a -> UI a
flow = container "HFlowContainer" True natural HFlowContainer.construct

-- | A (vertically stacked) group of widgets under a title that the user
-- can click to fold the group away.
collapsible :: String -> UI a -> UI a
collapsible title body = do
  (w, f) <- widget "FoldableContainer" natural FoldableContainer.construct
  text w title (FoldableContainer.setTitle f)
  children False w (vbox body)

-- | Two (vertically stacked) groups of widgets side by side, with a
-- divider between them that the user can drag. Takes up the space left in
-- its container.
--
-- @
-- (selected', ()) <- hsplit (expand (tree items selected)) (details selected)
-- @
hsplit :: UI a -> UI b -> UI (a, b)
hsplit = split "HSplitContainer" True HSplitContainer.construct

-- | Like 'hsplit', with one group above the other.
vsplit :: UI a -> UI b -> UI (a, b)
vsplit = split "VSplitContainer" False VSplitContainer.construct

split :: Inherits c Control => String -> Bool -> IO c -> UI a -> UI b -> UI (a, b)
split kind horizontal create first second =
  container kind horizontal filling create ((,) <$> side first <*> side second)
  where
    side :: UI x -> UI x
    side = container "VBoxContainer" False filling VBoxContainer.construct

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
