-- Styles are built by updating the fields of other flat styles.
{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

-- | The look of the UI: a port of the \"Modern\" style of Godot's editor
-- (editor/themes/theme_modern.cpp in Godot 4.7) to a theme for the 2D UI
-- controls.
--
-- Godot's editor themes are part of the editor, which is not in libgodot;
-- applications get Godot's (older looking) default theme. So the theme is
-- rebuilt here, with the colours, margins and icons the editor computes for
-- its default settings, and written out as a Godot theme resource (@.tres@)
-- that the generated project uses as its custom theme. Whatever it does not
-- set falls back to Godot's default theme.
module Godot.ImUI.Theme
  ( Theme (..)
  , themeResource
  , themeClearColor
  , backgroundType
  ) where

import Data.List (intercalate)
import qualified Data.Map.Strict as Map
import Numeric (showFFloat)

-- | The look of the controls.
data Theme
  = -- | The default style of Godot's editor (its \"Modern\" style).
    ModernDark
  | -- | The same, with the editor's light colour preset.
    ModernLight
  | -- | Godot's default theme for applications.
    GodotDefault
  deriving (Eq, Show)

-- | The theme type variation (of @PanelContainer@) for the background of a
-- window's content.
backgroundType :: String
backgroundType = "ImUIBackground"

-- | The contents of the theme resource, if the theme is not Godot's own.
themeResource :: Theme -> Maybe String
themeResource t = render . modern <$> palette t

-- | The colour to clear windows with (seen while a window is resized), as
-- a Godot @Color@ literal.
themeClearColor :: Theme -> Maybe String
themeClearColor t = showColor . pBase <$> palette t

--------------------------------------------------------------------------------
-- Colours

data RGBA = RGBA !Double !Double !Double !Double
  deriving (Eq, Ord)

rgb :: Double -> Double -> Double -> RGBA
rgb r g b = RGBA r g b 1

white, black, transparent :: RGBA
white = rgb 1 1 1
black = rgb 0 0 0
transparent = RGBA 1 1 1 0

lerp :: RGBA -> RGBA -> Double -> RGBA
lerp (RGBA a b c d) (RGBA e f g h) t = RGBA (l a e) (l b f) (l c g) (l d h)
  where
    l x y = x + (y - x) * t

times :: RGBA -> RGBA -> RGBA
times (RGBA a b c d) (RGBA e f g h) = RGBA (a * e) (b * f) (c * g) (d * h)

-- | Multiply the alpha.
fade :: Double -> RGBA -> RGBA
fade a c = c `times` RGBA 1 1 1 a

withAlpha :: Double -> RGBA -> RGBA
withAlpha a (RGBA r g b _) = RGBA r g b a

scale :: Double -> RGBA -> RGBA
scale k (RGBA r g b a) = RGBA (k * r) (k * g) (k * b) (k * a)

clampColor :: RGBA -> RGBA
clampColor (RGBA r g b a) = RGBA (c r) (c g) (c b) (c a)
  where
    c = max 0 . min 1

darkened :: Double -> RGBA -> RGBA
darkened k (RGBA r g b a) = RGBA (r * (1 - k)) (g * (1 - k)) (b * (1 - k)) a

luminance :: RGBA -> Double
luminance (RGBA r g b _) = 0.2126 * r + 0.7152 * g + 0.0722 * b

toHSV :: RGBA -> (Double, Double, Double)
toHSV (RGBA r g b _) = (h, s, mx)
  where
    mx = maximum [r, g, b]
    d = mx - minimum [r, g, b]
    s = if mx == 0 then 0 else d / mx
    h0
      | d == 0 = 0
      | r == mx = (g - b) / d
      | g == mx = 2 + (b - r) / d
      | otherwise = 4 + (r - g) / d
    h = (if h0 < 0 then h0 + 6 else h0) / 6

fromHSV :: Double -> Double -> Double -> Double -> RGBA
fromHSV h s v a = case i `mod` 6 of
  0 -> RGBA v t p a
  1 -> RGBA q v p a
  2 -> RGBA p v t a
  3 -> RGBA p q v a
  4 -> RGBA t p v a
  _ -> RGBA v p q a
  where
    h6 = h * 6
    i = floor h6 :: Int
    f = h6 - fromIntegral i
    p = v * (1 - s)
    q = v * (1 - s * f)
    t = v * (1 - s * (1 - f))

--------------------------------------------------------------------------------
-- The editor's settings

data Palette = Palette
  { pDark :: Bool
  , pBase :: RGBA
  , pAccent :: RGBA
  , pContrast :: Double
  }

-- | The editor's \"Default\" and \"Light\" colour presets.
palette :: Theme -> Maybe Palette
palette t = case t of
  ModernDark -> Just Palette {pDark = True, pBase = rgb 0.161 0.161 0.161, pAccent = rgb 0.337 0.62 1, pContrast = 0.3}
  ModernLight -> Just Palette {pDark = False, pBase = rgb 0.9 0.9 0.9, pAccent = rgb 0.18 0.5 1, pContrast = -0.06}
  GodotDefault -> Nothing

-- The \"Default\" spacing preset, at an editor scale of 1.
cornerRadius :: Int
cornerRadius = 4

baseMargin, increasedMargin, separationMargin, popupMargin :: Double
baseMargin = 4
increasedMargin = 4
separationMargin = 4
popupMargin = baseMargin * 2.4

-- | The base colour, darkened (or lightened, for negative offsets) and
-- desaturated.
baseColor :: Palette -> Double -> Double -> RGBA
baseColor p dimness saturation = fromHSV h (s * saturation) v' a
  where
    RGBA _ _ _ a = pBase p
    (h, s, v) = toHSV (pBase p)
    contrast = if dimness < 0 then max (-0.1) (min 0.5 (pContrast p)) else pContrast p
    v' = max 0 (min 1 (v + (0 - v) * contrast * dimness))

--------------------------------------------------------------------------------
-- Theme items

type Sides = (Double, Double, Double, Double)

data Box
  = Flat
      { bg :: RGBA
      , drawCenter :: Bool
      , contentMargin :: Sides
      , radius :: (Int, Int, Int, Int)
      -- ^ top left, top right, bottom right, bottom left
      , cornerDetail :: Int
      , borderWidth :: (Int, Int, Int, Int)
      -- ^ left, top, right, bottom
      , borderColor :: RGBA
      , shadowColor :: RGBA
      , shadowSize :: Int
      , shadowOffset :: (Double, Double)
      , expandMargin :: Sides
      }
  | Empty Sides
  | Line {lineColor :: RGBA, thickness :: Int, grow :: Double, vertical :: Bool}

data Icon
  = Svg String [(RGBA, RGBA)]
  -- ^ an editor icon, and colours to replace in it
  | NoIcon

data Item
  = Style String Box
  | Colour String RGBA
  | Const String Int
  | IconItem String Icon
  | BaseType String

all4 :: a -> (a, a, a, a)
all4 x = (x, x, x, x)

-- | @EditorThemeManager::make_flat_stylebox@.
flat :: RGBA -> Sides -> Int -> Box
flat c margins r =
  Flat
    { bg = c
    , drawCenter = True
    , contentMargin = margins
    , radius = all4 r
    , cornerDetail = ceiling (0.8 * fromIntegral r :: Double)
    , borderWidth = all4 0
    , borderColor = rgb 0.8 0.8 0.8
    , shadowColor = RGBA 0 0 0 0.6
    , shadowSize = 0
    , shadowOffset = (0, 0)
    , expandMargin = all4 0
    }

-- | Godot's \"not set\" margins.
unset :: Sides
unset = all4 (-1)

-- | The theme, built as @ThemeModern::populate_shared_styles@ and
-- @populate_standard_styles@ build the editor's.
modern :: Palette -> [(String, [Item])]
modern p =
  [ -- Panels.
    ("Panel", [Style "panel" (flat dark1 (6, 4, 6, 4) cornerRadius)])
  , -- The editor's PanelContainer draws nothing. godot-imui's 'panel' is a
    -- framed group: outline it (filling it would hide the text fields'
    -- background). The padding is godot-imui's.
    ( "PanelContainer"
    , [Style "panel" baseStyle {drawCenter = False, contentMargin = all4 0, borderWidth = all4 1, borderColor = surfaceHigh}]
    )
  , -- The background of a window: the editor's foreground panels.
    ( backgroundType
    , [BaseType "PanelContainer", Style "panel" baseStyle {contentMargin = all4 0, radius = all4 0}]
    )
  , ("ScrollContainer", [Style "panel" (Empty unset)])
  , ("TooltipLabel", [Colour "font_color" fontHover, Colour "font_shadow_color" transparent])
  , ("TooltipPanel", [Style "panel" baseStyle {bg = surfacePopup, contentMargin = all4 0, radius = all4 0}])
  , ("PopupPanel", [Style "panel" popupPanel])
  , -- Buttons.
    ( "Button"
    , buttonStyles buttonStyle buttonHover buttonPressed buttonPressed buttonDisabled
        ++ [Style "focus" focusStyle]
        ++ fontColors
        ++ [ Colour "icon_normal_color" iconNormal
           , Colour "icon_hover_color" iconHover
           , Colour "icon_focus_color" iconFocus
           , Colour "icon_hover_pressed_color" iconPressed
           , Colour "icon_pressed_color" iconPressed
           , Colour "icon_disabled_color" iconDisabled
           , Const "h_separation" 4
           , Const "outline_size" 0
           , Const "align_to_largest_stylebox" 1
           ]
    )
  , -- Menu bars and menu buttons are flat, like the editor's main menu.
    ( "MenuBar"
    , buttonStyles flatButton flatHover flatPressed flatHoverPressed flatButton
        ++ fontColors
        ++ [Const "h_separation" 4, Const "outline_size" 0]
    )
  , ("MenuButton", buttonStyles emptyWide flatHover flatPressed flatHoverPressed emptyWide)
  , ( "OptionButton"
    , [ IconItem "arrow" (icon "GuiOptionArrow")
      , Const "arrow_margin" (round (baseMargin * 2))
      , Const "modulate_arrow" 1
      , Const "h_separation" 4
      ]
    )
  , ( "CheckButton"
    , buttonStyles panelContainerStyle panelContainerStyle panelContainerStyle panelContainerStyle panelContainerStyle
        ++ [ IconItem "checked" (accentIcon "GuiToggleOn")
           , IconItem "checked_disabled" (icon "GuiToggleOnDisabled")
           , IconItem "unchecked" (icon "GuiToggleOff")
           , IconItem "unchecked_disabled" (icon "GuiToggleOffDisabled")
           , Const "h_separation" 8
           , Const "check_v_offset" 0
           , Const "outline_size" 0
           ]
    )
  , ( "CheckBox"
    , buttonStyles checkboxStyle {drawCenter = False} checkboxStyle checkboxStyle checkboxStyle checkboxStyle
        ++ checkIcons
        ++ [Const "h_separation" 8, Const "check_v_offset" 0, Const "outline_size" 0]
    )
  , ("LinkButton", [Style "focus" (Empty unset), Const "outline_size" 0] ++ fontColors)
  , -- Tree & ItemList.
    ( "Tree"
    , [ Style "panel" (Empty (baseMargin * 1.5, baseMargin * 2.5, baseMargin * 1.5, baseMargin * 2.5))
      , Style "focus" focusStyle
      , Style "button_pressed" treeButtonPressed
      , Style "custom_button" flatButton
      , Style "custom_button_pressed" treeButtonPressed
      , Style "button_hover" treeHover
      , Style "hovered" treeHover
      , Style "hovered_dimmed" treeHover
      , Style "custom_button_hover" treeHover
      , Style "selected" flatPressed {contentMargin = all4 0}
      , Style "selected_focus" focusStyle
      , Style "hovered_selected" flatHoverPressed {contentMargin = all4 0}
      , Style "hovered_selected_focus" focusStyle
      , Style "cursor" cursor
      , Style "cursor_unfocused" cursor
      , Style "title_button_normal" treeTitle
      , Style "title_button_hover" treeTitle
      , Style "title_button_pressed" treeTitle
      , IconItem "checked" (accentIcon "GuiChecked")
      , IconItem "checked_disabled" (icon "GuiCheckedDisabled")
      , IconItem "indeterminate" (accentIcon "GuiIndeterminate")
      , IconItem "indeterminate_disabled" (icon "GuiIndeterminateDisabled")
      , IconItem "unchecked" (icon "GuiUnchecked")
      , IconItem "unchecked_disabled" (icon "GuiUncheckedDisabled")
      , IconItem "arrow" (icon "GuiTreeArrowDown")
      , IconItem "arrow_collapsed" (icon "GuiTreeArrowRight")
      , IconItem "arrow_collapsed_mirrored" (icon "GuiTreeArrowLeft")
      , IconItem "updown" (icon "GuiTreeUpdown")
      , IconItem "select_arrow" (icon "GuiDropdown")
      , Colour "custom_button_font_highlight" fontHover
      , Colour "font_color" fontColor
      , Colour "font_hovered_color" fontHover
      , Colour "font_hovered_dimmed_color" fontHover
      , Colour "font_hovered_selected_color" monoFont
      , Colour "font_selected_color" monoFont
      , Colour "font_disabled_color" fontDisabled
      , Colour "font_outline_color" fontOutline
      , Colour "title_button_color" fontColor
      , Colour "relationship_line_color" relationshipLine
      , Colour "parent_hl_line_color" (fade 0.2 mono)
      , Colour "children_hl_line_color" relationshipLine
      , Colour "drop_on_item_color" (pAccent p)
      , Colour "drop_position_color" iconNormal
      , Colour "guide_color" transparent
      , Colour "scroll_hint_color" scrollHint
      , Const "v_separation" (floor ((baseMargin * 0.175) ** 3))
      , Const "h_separation" (round (increasedMargin + 2))
      , Const "guide_width" 0
      , Const "item_margin" (round (max (3 * increasedMargin) 12))
      , Const "inner_item_margin_top" (round (baseMargin * 0.75))
      , Const "inner_item_margin_bottom" (round (baseMargin * 0.75))
      , Const "inner_item_margin_left" (round baseMargin)
      , Const "inner_item_margin_right" (round baseMargin)
      , Const "check_h_separation" (round (baseMargin * 1.5))
      , Const "icon_h_separation" (round (baseMargin * 1.5))
      , Const "button_margin" (round baseMargin)
      , Const "scroll_border" 40
      , Const "scroll_speed" 12
      , Const "outline_size" 0
      , Const "scrollbar_margin_left" 0
      , Const "scrollbar_margin_top" 0
      , Const "scrollbar_margin_right" 0
      , Const "scrollbar_margin_bottom" 0
      , Const "scrollbar_h_separation" 1
      , Const "scrollbar_v_separation" 1
      , -- Relationship lines for the selected item only.
        Const "draw_guides" 0
      , Const "draw_relationship_lines" 1
      , Const "relationship_line_width" 0
      , Const "parent_hl_line_width" 1
      , Const "children_hl_line_width" 1
      , Const "parent_hl_line_margin" 3
      ]
    )
  , ( "ItemList"
    , [ Style "panel" baseStyle {contentMargin = all4 (baseMargin * 2)}
      , Style "focus" focusStyle
      , Style "cursor" cursor
      , Style "cursor_unfocused" cursor
      , Style "selected_focus" focusStyle
      , Style "selected" flatPressed
      , Style "hovered" flatHover
      , Style "hovered_selected" flatHoverPressed
      , Style "hovered_selected_focus" focusStyle
      , Colour "font_color" fontColor
      , Colour "font_hovered_color" fontHover
      , Colour "font_hovered_selected_color" fontHoverPressed
      , Colour "font_selected_color" fontPressed
      , Colour "font_outline_color" fontOutline
      , Colour "guide_color" transparent
      , Colour "scroll_hint_color" scrollHint
      , Const "v_separation" (round (baseMargin * 1.5))
      , Const "h_separation" (round (increasedMargin + 2))
      , Const "icon_margin" (round (increasedMargin + 2))
      , Const "line_separation" (round separationMargin)
      , Const "outline_size" 0
      ]
    )
  , -- TabBar & TabContainer.
    ( "TabContainer"
    , tabStyles
        ++ tabColors
        ++ [ Style "tabbar_background" baseStyle {bg = surfaceLowest, radius = (cornerRadius, cornerRadius, 0, 0), contentMargin = (0, 0, baseMargin * 0.25, 0)}
           , Style "panel" baseStyle {contentMargin = all4 (increasedMargin * 1.5), radius = (0, 0, cornerRadius, cornerRadius)}
           , Const "side_margin" 0
           , Const "outline_size" 0
           ]
    )
  , ( "TabBar"
    , tabStyles
        ++ tabColors
        ++ [ Style "button_pressed" panelContainerStyle
           , Style "button_highlight" panelContainerStyle
           , Const "h_separation" 4
           , Const "outline_size" 0
           ]
    )
  , -- Separators.
    ("HSeparator", [Style "separator" separatorLine])
  , ("VSeparator", [Style "separator" separatorLine {vertical = True}])
  , ("Separator", [Const "separation" (round (baseMargin * 2))])
  , -- LineEdit & TextEdit.
    ( "LineEdit"
    , [ Style "normal" textStyle
      , Style "focus" focusStyle
      , Style "read_only" textReadOnly
      , Colour "font_color" fontColor
      , Colour "font_selected_color" fontPressed
      , Colour "font_uneditable_color" fontReadOnly
      , Colour "font_placeholder_color" fontDisabled
      , Colour "font_outline_color" fontOutline
      , Colour "caret_color" fontColor
      , Colour "selection_color" selection
      , Colour "clear_button_color" fontColor
      , Colour "clear_button_color_pressed" (pAccent p)
      , Const "minimum_character_width" 4
      , Const "outline_size" 0
      , Const "caret_width" 1
      ]
    )
  , ( "TextEdit"
    , [ Style "normal" textStyle
      , Style "focus" focusStyle
      , Style "read_only" textReadOnly
      , Colour "font_color" fontColor
      , Colour "font_readonly_color" fontReadOnly
      , Colour "font_placeholder_color" fontDisabled
      , Colour "font_outline_color" fontOutline
      , Colour "caret_color" fontColor
      , Colour "selection_color" selection
      , Const "line_spacing" 4
      , Const "outline_size" 0
      , Const "caret_width" 1
      ]
    )
  , -- Containers.
    ("BoxContainer", [Const "separation" separation])
  , ("HBoxContainer", [Const "separation" separation])
  , ("VBoxContainer", [Const "separation" separation])
  , -- (round (widget_margin.y - 2), widget_margin.y being increasedMargin + 1)
    ("GridContainer", [Const "h_separation" separation, Const "v_separation" (round (increasedMargin + 1 - 2))])
  , ("FlowContainer", flowSeparation)
  , ("HFlowContainer", flowSeparation)
  , ("VFlowContainer", flowSeparation)
  , ("SplitContainer", splitConstants)
  , ("HSplitContainer", splitConstants)
  , ("VSplitContainer", splitConstants)
  , ( "FoldableContainer"
    , [ Style "title_panel" (flat titleColor (all4 baseMargin) 0) {radius = (cornerRadius, cornerRadius, 0, 0)}
      , Style "title_hover_panel" (flat titleHoverColor (all4 baseMargin) 0) {radius = (cornerRadius, cornerRadius, 0, 0)}
      , Style "title_collapsed_panel" (flat titleColor (all4 baseMargin) 0)
      , Style "title_collapsed_hover_panel" (flat titleHoverColor (all4 baseMargin) 0)
      , Style "panel" (flat dark1 (all4 baseMargin) 0) {radius = (0, 0, cornerRadius, cornerRadius)}
      , Style "focus" focusStyle
      , Colour "font_color" fontColor
      , Colour "hover_font_color" fontHover
      , Colour "collapsed_font_color" fontPressed
      , Colour "font_outline_color" fontOutline
      , Const "outline_size" 0
      , Const "h_separation" separation
      ]
    )
  , -- Windows and dialogs.
    ("Window", [Colour "title_color" fontColor])
  , ( "AcceptDialog"
    , [ Style "panel" baseStyle {contentMargin = all4 popupMargin, radius = all4 0}
      , Const "buttons_separation" 8
      , Const "buttons_min_width" 105
      , Const "buttons_min_height" 34
      ]
    )
  , ( "FileDialog"
    , [ Colour "folder_icon_color" (lerp (if pDark p then white else rgb 4.25 4.25 4.25) (pAccent p) 0.7)
      , Colour "file_disabled_color" fontDisabled
      ]
    )
  , ( "PopupMenu"
    , [ Style "panel" baseStyle {bg = surfacePopup, contentMargin = all4 popupMargin, radius = all4 0}
      , Style "hover" flatHover {bg = baseColor p (-0.5) 0.75}
      , Style "separator" popupSeparator
      , Style "labeled_separator_left" popupSeparator
      , Style "labeled_separator_right" popupSeparator
      , Colour "font_color" fontColor
      , Colour "font_hover_color" fontHover
      , Colour "font_accelerator_color" fontDisabled
      , Colour "font_disabled_color" fontDisabled
      , Colour "font_separator_color" fontDisabled
      , Colour "font_outline_color" fontOutline
      , Const "h_separation" (round (baseMargin * 1.75))
      , Const "v_separation" (round (baseMargin * 1.75))
      , Const "search_bar_separation" (round (baseMargin * 1.75))
      , Const "outline_size" 0
      , Const "item_start_padding" (round popupMargin)
      , Const "item_end_padding" (round popupMargin)
      , IconItem "submenu" (icon "ArrowRight")
      , IconItem "submenu_mirrored" (icon "ArrowLeft")
      ]
        ++ checkIcons
    )
  , -- Sliders and scroll bars.
    ("HScrollBar", scrollBar (Empty (0, scrollMargin, 0, scrollMargin)) ("padding_top", "padding_bottom"))
  , ("VScrollBar", scrollBar (Empty (scrollMargin, 0, scrollMargin, 0)) ("padding_left", "padding_right"))
  , ("HSlider", slider (0, 2, 0, 2) (0, sliderMargin, 0, sliderMargin))
  , ("VSlider", slider (2, 0, 2, 0) (sliderMargin, 0, sliderMargin, 0))
  , -- Labels.
    ( "RichTextLabel"
    , [ -- The editor puts rich text (its help) in a box. godot-imui's is
        -- formatted text among the rest, so lay it out like a Label.
        Style "normal" labelStyle
      , Style "focus" (Empty unset)
      , Colour "default_color" fontColor
      , Colour "font_shadow_color" transparent
      , Colour "font_outline_color" fontOutline
      , Colour "selection_color" selection
      , Const "shadow_offset_x" 1
      , Const "shadow_offset_y" 1
      , Const "shadow_outline_size" 1
      , Const "outline_size" 0
      ]
    )
  , ( "Label"
    , [ Style "normal" labelStyle
      , Style "focus" focusStyle
      , Colour "font_color" fontColor
      , Colour "font_shadow_color" transparent
      , Colour "font_outline_color" fontOutline
      , Const "shadow_offset_x" 1
      , Const "shadow_offset_y" 1
      , Const "shadow_outline_size" 1
      , Const "line_spacing" 3
      , Const "outline_size" 0
      ]
    )
  , -- SpinBox.
    ( "SpinBox"
    , [IconItem "updown" NoIcon]
        ++ [IconItem ("up" ++ s) (icon "GuiSpinboxUp") | s <- ["", "_hover", "_pressed", "_disabled"]]
        ++ [IconItem ("down" ++ s) (icon "GuiSpinboxDown") | s <- ["", "_hover", "_pressed", "_disabled"]]
        ++ concat
          [ [ Style (dir ++ "_background") (Empty unset)
            , Style (dir ++ "_background_hovered") buttonHover
            , Style (dir ++ "_background_pressed") buttonPressed
            , Style (dir ++ "_background_disabled") (Empty unset)
            , Colour (dir ++ "_icon_modulate") iconNormal
            , Colour (dir ++ "_hover_icon_modulate") iconHover
            , Colour (dir ++ "_pressed_icon_modulate") iconPressed
            , Colour (dir ++ "_disabled_icon_modulate") iconDisabled
            ]
          | dir <- ["up", "down"]
          ]
        ++ [ Style "field_and_buttons_separator" (Empty unset)
           , Style "up_down_buttons_separator" (Empty unset)
           , Const "buttons_vertical_separation" 0
           , Const "field_and_buttons_separation" 2
           , Const "buttons_width" 16
           , Const "set_min_buttons_width_from_icons" 1
           ]
    )
  , -- ProgressBar.
    ( "ProgressBar"
    , [ Style "background" progressBackground
      , Style "fill" progressBackground {bg = buttonNormalColor}
      , Colour "font_color" fontColor
      , Colour "font_outline_color" fontOutline
      , Const "outline_size" 0
      ]
    )
  , -- ColorPicker.
    ( "ColorPicker"
    , [ Const "margin" (round baseMargin)
      , Const "sv_width" 256
      , Const "sv_height" 256
      , Const "h_width" 30
      , Const "label_width" 10
      , Const "center_slider_grabbers" 1
      , Style "sample_focus" focusStyle
      , Style "picker_focus_rectangle" focusStyle
      , Style "picker_focus_circle" baseStyle {borderColor = fade 0.3 mono, drawCenter = False, radius = all4 256, cornerDetail = 32}
      , Colour "focused_not_editing_cursor_color" (withAlpha 0.275 (pAccent p))
      ]
    )
  , ("ColorPresetButton", [Style "preset_fg" (flat white (all4 2) 2)])
  ]
  where
    dark = pDark p
    base = pBase p
    accent = pAccent p

    -- Base colours.
    mono = if dark then white else black
    monoFont = mono
    monoInv = if dark then black else white
    dark1 = clampColor (lerp base black (pContrast p * 1.15))
    dark2 = if dark then RGBA 0 0 0 0.3 else RGBA 1 1 1 0.3
    contrast1 = lerp base mono (max (pContrast p * 1.15) (0.3 * 1.15))

    -- Font colours.
    fontColor = fade 0.75 monoFont
    fontSecondary = fade 0.55 monoFont
    fontFocus = monoFont
    fontHover = fade 0.85 monoFont
    fontPressed = fade 0.85 monoFont
    fontHoverPressed = monoFont
    fontDisabled = fade (if dark then 0.35 else 0.5) monoFont
    fontReadOnly = withAlpha 0.65 monoFont
    fontOutline = transparent
    fontColors =
      [ Colour "font_color" fontColor
      , Colour "font_hover_color" fontHover
      , Colour "font_hover_pressed_color" fontHoverPressed
      , Colour "font_focus_color" fontFocus
      , Colour "font_pressed_color" fontPressed
      , Colour "font_disabled_color" fontDisabled
      , Colour "font_outline_color" fontOutline
      ]

    -- Icon colours.
    iconNormal = RGBA 1 1 1 (if dark then 0.85 else 0.95)
    iconSecondary = RGBA 1 1 1 (if dark then 0.6 else 0.75)
    iconFocus = white
    iconHover = white
    iconPressed = withAlpha 1 (scale (if dark then 1.15 else 3.5) accent)
    iconDisabled = RGBA 1 1 1 (if dark then 0.35 else 0.5)

    -- Surfaces.
    surfacePopup = baseColor p 1.9 0.9
    surfaceLowest = baseColor p 1.7 0.9
    surfaceLower = baseColor p 1.1 0.9
    surfaceBase = baseColor p 0 1
    surfaceHigh = baseColor p (-1.3) 0.8
    buttonNormalColor = baseColor p (-2.0) 0.85
    selection = fade 0.4 accent
    scrollHint = RGBA 0 0 0 (if dark then 1 else 0.5)
    relationshipLine = fade 0.1 mono

    -- Shared styles.
    baseStyle = flat base (all4 (increasedMargin * 1.5)) cornerRadius
    focusStyle = baseStyle {drawCenter = False, borderColor = fade 0.8 accent, borderWidth = all4 2}
    emptyWide = Empty (baseMargin * 1.5, baseMargin, baseMargin * 1.5, baseMargin)
    buttonStyle =
      baseStyle
        { contentMargin = (baseMargin * 2, baseMargin * 1.5, baseMargin * 2, baseMargin * 1.5)
        , bg = buttonNormalColor
        , borderWidth = all4 1
        , shadowColor = if dark then RGBA 0 0 0 0.005 else RGBA 1 1 1 0.005
        , shadowSize = 8
        , shadowOffset = (0, 4)
        , borderColor = baseColor p (-2.5) 0.75
        }
    buttonDisabled = buttonStyle {bg = baseColor p (-1.4) 0.75, borderWidth = all4 0}
    buttonPressed = buttonStyle {bg = baseColor p (-3.2) 0.75, borderColor = baseColor p (-3.7) 0.75}
    buttonHover = buttonStyle {bg = baseColor p (-2.9) 0.75, borderColor = baseColor p (-3.4) 0.75}
    flatHover =
      baseStyle
        { bg = baseColor p (-1.2) 0.75
        , contentMargin = (baseMargin * 1.5, baseMargin * 0.9, baseMargin * 1.5, baseMargin * 0.9)
        }
    flatPressed = flatHover {bg = baseColor p (-2.0) 0.75}
    flatHoverPressed = flatHover {bg = baseColor p (-2.4) 0.75}
    flatButton = flatHover {drawCenter = False}
    popupPanel =
      baseStyle
        { bg = surfacePopup
        , shadowColor = RGBA 0 0 0 0.3
        , shadowSize = round (baseMargin * 0.75)
        , contentMargin = all4 popupMargin
        , radius = all4 0
        }
    panelContainerStyle = buttonStyle {drawCenter = False, borderWidth = all4 0}
    checkboxStyle = panelContainerStyle {contentMargin = (baseMargin * 1.5, baseMargin * 0.75, baseMargin * 1.5, baseMargin * 0.75)}

    -- The styles of a button in each state.
    buttonStyles normal hover pressed hoverPressed disabled =
      concat
        [ [Style name s, Style (name ++ "_mirrored") s]
        | (name, s) <- [("normal", normal), ("hover", hover), ("pressed", pressed), ("hover_pressed", hoverPressed), ("disabled", disabled)]
        ]

    checkIcons =
      [ IconItem "checked" (accentIcon "GuiChecked")
      , IconItem "unchecked" (icon "GuiUnchecked")
      , IconItem "radio_checked" (accentIcon "GuiRadioChecked")
      , IconItem "radio_unchecked" (icon "GuiRadioUnchecked")
      , IconItem "checked_disabled" (icon "GuiCheckedDisabled")
      , IconItem "unchecked_disabled" (icon "GuiUncheckedDisabled")
      , IconItem "radio_checked_disabled" (icon "GuiRadioCheckedDisabled")
      , IconItem "radio_unchecked_disabled" (icon "GuiRadioUncheckedDisabled")
      ]

    -- Trees.
    treeButtonPressed = flatPressed {contentMargin = (baseMargin, 0, baseMargin, 0)}
    treeHover = flatHover {contentMargin = all4 0}
    cursor = baseStyle {bg = fade 0.04 mono}
    treeTitle = baseStyle {bg = surfaceLower, borderColor = withAlpha 0 surfaceLower, borderWidth = (1, 0, 1, 0)}

    -- Tabs.
    tabSelected =
      baseStyle
        { contentMargin = (baseMargin * 4, baseMargin * 2.1, baseMargin * 4, baseMargin * 2.1)
        , radius = (cornerRadius, cornerRadius, 0, 0)
        }
    tabUnselected = tabSelected {bg = surfaceLowest, borderWidth = all4 0}
    tabStyles =
      [ Style "tab_selected" tabSelected
      , Style "tab_hovered" tabUnselected {bg = fade 0.6 surfaceBase}
      , Style "tab_unselected" tabUnselected
      , Style "tab_disabled" tabUnselected
      , Style "tab_focus" focusStyle
      ]
    tabColors =
      [ Colour "font_selected_color" fontColor
      , Colour "font_hovered_color" fontHover
      , Colour "font_unselected_color" fontSecondary
      , Colour "font_disabled_color" (fade 0.55 fontDisabled)
      , Colour "font_outline_color" fontOutline
      , Colour "drop_mark_color" (lerp dark2 accent 0.75)
      , Colour "icon_selected_color" iconNormal
      , Colour "icon_hovered_color" iconHover
      , Colour "icon_unselected_color" iconSecondary
      , Colour "icon_disabled_color" (fade 0.55 iconDisabled)
      ]

    separatorLine = Line {lineColor = if dark then RGBA 0 0 0 0.4 else RGBA 0 0 0 0.2, thickness = 2, grow = -baseMargin, vertical = False}
    popupSeparator = Line {lineColor = fade (if dark then 0.075 else 0.125) mono, thickness = 2, grow = -baseMargin * 2, vertical = False}

    textStyle =
      baseStyle
        { bg = surfaceLower
        , contentMargin = (baseMargin * 2, baseMargin * 0.75 + 1, baseMargin * 2, baseMargin * 0.75 + 1)
        }
    labelStyle = Empty (baseMargin * 2, baseMargin, baseMargin * 2, baseMargin)
    textReadOnly = textStyle {bg = if dark then RGBA 0 0 0 0.2 else RGBA 1 1 1 0.5}

    separation = round separationMargin
    flowSeparation = [Const "h_separation" separation, Const "v_separation" separation]
    splitConstants =
      [ Const "separation" (round (baseMargin * 0.75))
      , Const "autohide" 1
      , Const "minimum_grab_thickness" (round (baseMargin * 2))
      ]
    titleColor = darkened 0.125 dark1
    titleHoverColor = lerp dark1 base 0.4

    scrollMargin = 3
    scrollBar scroll (padding1, padding2) =
      [ Style "scroll" scroll
      , Style "scroll_focus" focusStyle
      , Style "grabber" baseStyle {bg = fade 0.225 mono}
      , Style "grabber_highlight" baseStyle {bg = fade 0.5 mono}
      , Style "grabber_pressed" baseStyle {bg = fade 0.5 mono}
      , Const padding1 (round baseMargin)
      , Const padding2 (round baseMargin)
      ]
        ++ [IconItem (dir ++ s) NoIcon | dir <- ["increment", "decrement"], s <- ["", "_highlight", "_pressed"]]
    sliderMargin = max 2 (baseMargin / 2)
    slider margins areaMargins =
      [ IconItem "grabber" (icon "GuiSliderGrabber")
      , IconItem "grabber_highlight" (icon "GuiSliderGrabberHl")
      , Style "slider" baseStyle {bg = fade 0.35 monoInv, contentMargin = margins}
      , Style "grabber_area" (flat contrast1 areaMargins cornerRadius)
      , Style "grabber_area_highlight" (flat contrast1 areaMargins 0)
      , Const "center_grabber" 0
      , Const "grabber_offset" 0
      ]

    progressBackground =
      baseStyle
        { bg = surfaceLowest
        , expandMargin = (0, baseMargin * 0.5, 0, baseMargin * 0.5)
        , contentMargin = all4 baseMargin
        }

    -- The editor recolours its icons: the accent colour of checked boxes
    -- etc. is the theme's, and light themes darken white and light grey.
    icon name = Svg (iconSource name) (if dark then [] else [(white, rgb256 0x41 0x41 0x41), (rgb256 0xe0 0xe0 0xe0, rgb256 0x5a 0x5a 0x5a)])
    accentIcon name =
      Svg (iconSource name) $
        (rgb256 0x69 0x9c 0xe8, accent)
          : [(white, rgb 0.2 0.2 0.2) | luminance accent > 0.75]
          ++ [(white, rgb256 0x41 0x41 0x41) | not dark, name `elem` ["GuiToggleOn"]]

rgb256 :: Int -> Int -> Int -> RGBA
rgb256 r g b = rgb (c r) (c g) (c b)
  where
    c x = fromIntegral x / 255

--------------------------------------------------------------------------------
-- Writing the resource

-- | A Godot text resource for the theme, with each distinct stylebox and
-- icon as a sub-resource.
render :: [(String, [Item])] -> String
render types = unlines (header : concatMap subResource (reverse resources) ++ "[resource]" : properties)
  where
    header = "[gd_resource type=\"Theme\" format=3]\n"
    items = [(ty, item) | (ty, is) <- types, item <- is]
    -- Sub-resources in order of first use, keyed by their contents.
    (resources, ids) = foldl collect ([], Map.empty) [r | (_, i) <- items, Just r <- [resourceOf i]]
    collect (rs, m) r
      | Map.member r m = (rs, m)
      | otherwise = let rid = "r" ++ show (Map.size m + 1) in ((rid, r) : rs, Map.insert r rid m)
    subResource (rid, (ty, body)) = ("[sub_resource type=\"" ++ ty ++ "\" id=\"" ++ rid ++ "\"]") : body ++ [""]
    ref r = "SubResource(\"" ++ ids Map.! r ++ "\")"
    properties = [ty ++ "/" ++ property ref item | (ty, item) <- items]

resourceOf :: Item -> Maybe (String, [String])
resourceOf item = case item of
  Style _ box -> Just (boxResource box)
  IconItem _ i -> Just (iconResource i)
  _ -> Nothing

property :: ((String, [String]) -> String) -> Item -> String
property ref item = case item of
  Style name box -> "styles/" ++ name ++ " = " ++ ref (boxResource box)
  IconItem name i -> "icons/" ++ name ++ " = " ++ ref (iconResource i)
  Colour name c -> "colors/" ++ name ++ " = " ++ showColor c
  Const name n -> "constants/" ++ name ++ " = " ++ show n
  BaseType ty -> "base_type = &" ++ quoted ty

boxResource :: Box -> (String, [String])
boxResource box = case box of
  Flat {} ->
    ( "StyleBoxFlat"
    , margins "content_margin_" (contentMargin box)
        ++ ["bg_color = " ++ showColor (bg box)]
        ++ ["draw_center = false" | not (drawCenter box)]
        ++ sides "border_width_" (borderWidth box)
        ++ ["border_color = " ++ showColor (borderColor box)]
        ++ corners (radius box)
        ++ ["corner_detail = " ++ show (cornerDetail box)]
        ++ margins "expand_margin_" (expandMargin box)
        ++ [ "shadow_color = " ++ showColor (shadowColor box)
           , "shadow_size = " ++ show (shadowSize box)
           , "shadow_offset = Vector2(" ++ showNum (fst (shadowOffset box)) ++ ", " ++ showNum (snd (shadowOffset box)) ++ ")"
           ]
    )
  Empty m -> ("StyleBoxEmpty", margins "content_margin_" m)
  Line {} ->
    ( "StyleBoxLine"
    , [ "color = " ++ showColor (lineColor box)
      , "grow_begin = " ++ showNum (grow box)
      , "grow_end = " ++ showNum (grow box)
      , "thickness = " ++ show (thickness box)
      ]
        ++ ["vertical = true" | vertical box]
    )
  where
    margins prefix (l, t, r, b) = zipWith (\side v -> prefix ++ side ++ " = " ++ showNum v) ["left", "top", "right", "bottom"] [l, t, r, b]
    sides prefix (l, t, r, b) = zipWith (\side v -> prefix ++ side ++ " = " ++ show v) ["left", "top", "right", "bottom"] [l, t, r, b]
    corners (tl, tr, br, bl) =
      zipWith (\c v -> "corner_radius_" ++ c ++ " = " ++ show v) ["top_left", "top_right", "bottom_right", "bottom_left"] [tl, tr, br, bl]

-- | Icons are SVG, rendered by Godot at whatever scale they are drawn at.
iconResource :: Icon -> (String, [String])
iconResource i = case i of
  NoIcon -> ("ImageTexture", [])
  Svg source colours ->
    ( "DPITexture"
    , ("_source = " ++ quoted source)
        : [ "color_map = {" ++ intercalate ", " [showColor from ++ ": " ++ showColor to | (from, to) <- colours] ++ "}"
          | not (null colours)
          ]
    )

showColor :: RGBA -> String
showColor (RGBA r g b a) = "Color(" ++ intercalate ", " (map showNum [r, g, b, a]) ++ ")"

showNum :: Double -> String
showNum x = showFFloat Nothing x ""

quoted :: String -> String
quoted s = "\"" ++ concatMap escape s ++ "\""
  where
    escape '"' = "\\\""
    escape '\\' = "\\\\"
    escape c = [c]

--------------------------------------------------------------------------------
-- Icons

-- | The SVG source of an icon of Godot's editor (editor/icons/*.svg, MIT
-- licensed, Copyright (c) 2014-present Godot Engine contributors).
iconSource :: String -> String
iconSource name = case name of
  "GuiChecked" -> "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"16\" height=\"16\"><rect width=\"14\" height=\"14\" x=\"1\" y=\"1\" fill=\"#699ce8\" rx=\"2.33\"/><path fill=\"#fff\" d=\"M11.5 3.734 5.89 9.346 4.185 7.665l-1.5 1.499 3.204 3.18L13 5.235z\"/></svg>"
  "GuiCheckedDisabled" -> "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"16\" height=\"16\"><rect width=\"14\" height=\"14\" x=\"1\" y=\"1\" fill=\"gray\" rx=\"2.33\"/><path fill=\"#b3b3b3\" d=\"M11.5 3.734 5.89 9.346 4.185 7.665l-1.5 1.499 3.204 3.18L13 5.235z\"/></svg>"
  "GuiUnchecked" -> "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"16\" height=\"16\"><rect width=\"14\" height=\"14\" x=\"1\" y=\"1\" fill=\"#e0e0e0\" fill-opacity=\".2\" rx=\"2.333\"/></svg>"
  "GuiUncheckedDisabled" -> "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"16\" height=\"16\"><rect width=\"14\" height=\"14\" x=\"1\" y=\"1\" fill=\"gray\" fill-opacity=\".2\" rx=\"2.333\"/></svg>"
  "GuiRadioChecked" -> "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"16\" height=\"16\"><circle cx=\"8\" cy=\"8\" r=\"7\" fill=\"#699ce8\"/><circle cx=\"8\" cy=\"8\" r=\"4\" fill=\"#fff\"/></svg>"
  "GuiRadioCheckedDisabled" -> "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"16\" height=\"16\"><circle cx=\"8\" cy=\"8\" r=\"7\" fill=\"gray\"/><circle cx=\"8\" cy=\"8\" r=\"4\" fill=\"#b3b3b3\"/></svg>"
  "GuiRadioUnchecked" -> "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"16\" height=\"16\"><circle cx=\"8\" cy=\"8\" r=\"7\" fill=\"#e0e0e0\" fill-opacity=\".2\"/></svg>"
  "GuiRadioUncheckedDisabled" -> "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"16\" height=\"16\"><circle cx=\"8\" cy=\"8\" r=\"7\" fill=\"gray\" fill-opacity=\".2\"/></svg>"
  "GuiIndeterminate" -> "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"16\" height=\"16\"><rect width=\"14\" height=\"14\" x=\"1\" y=\"1\" fill=\"#699ce8\" rx=\"2.33\"/><path fill=\"#fff\" d=\"M3 7h10v2H3z\"/></svg>"
  "GuiIndeterminateDisabled" -> "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"16\" height=\"16\"><rect width=\"14\" height=\"14\" x=\"1\" y=\"1\" fill=\"gray\" rx=\"2.33\"/><path fill=\"#b3b3b3\" d=\"M3 7h10v2H3z\"/></svg>"
  "GuiToggleOn" -> "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"38\" height=\"16\"><rect width=\"36\" height=\"14\" x=\"1\" y=\"1\" fill=\"#699ce8\" rx=\"7\"/><circle cx=\"30\" cy=\"8\" r=\"5\" fill=\"#fff\"/></svg>"
  "GuiToggleOnDisabled" -> "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"38\" height=\"16\"><rect width=\"36\" height=\"14\" x=\"1\" y=\"1\" fill=\"gray\" rx=\"7\"/><circle cx=\"30\" cy=\"8\" r=\"5\" fill=\"#b3b3b3\"/></svg>"
  "GuiToggleOff" -> "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"38\" height=\"16\"><g fill=\"#e0e0e0\"><rect width=\"36\" height=\"14\" x=\"1\" y=\"1\" fill-opacity=\".2\" rx=\"7\"/><circle cx=\"8\" cy=\"8\" r=\"5\"/></g></svg>"
  "GuiToggleOffDisabled" -> "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"38\" height=\"16\"><g fill=\"gray\"><rect width=\"36\" height=\"14\" x=\"1\" y=\"1\" fill-opacity=\".2\" rx=\"7\"/><circle cx=\"8\" cy=\"8\" r=\"5\"/></g></svg>"
  "GuiSliderGrabber" -> "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"16\" height=\"16\"><circle cx=\"8\" cy=\"8\" r=\"3\" fill=\"#fff\" fill-opacity=\".9\"/><circle cx=\"8\" cy=\"8\" r=\"6\" fill=\"none\" stroke=\"#e0e0e0\" stroke-opacity=\".29\" stroke-width=\"2\"/></svg>"
  "GuiSliderGrabberHl" -> "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"16\" height=\"16\"><circle cx=\"8\" cy=\"8\" r=\"3\" fill=\"#fff\" fill-opacity=\".9\"/><circle cx=\"8\" cy=\"8\" r=\"6\" fill=\"none\" stroke=\"#e0e0e0\" stroke-width=\"2\"/></svg>"
  "GuiOptionArrow" -> "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"12\" height=\"12\"><path fill=\"none\" stroke=\"#e0e0e0\" stroke-linecap=\"round\" stroke-linejoin=\"round\" stroke-width=\"2\" d=\"m2 4 4 4 4-4\"/></svg>"
  "GuiTreeArrowDown" -> "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"12\" height=\"12\"><path fill=\"none\" stroke=\"#fff\" stroke-linecap=\"round\" stroke-linejoin=\"round\" stroke-opacity=\".4\" stroke-width=\"2\" d=\"m3 5 3 3 3-3\"/></svg>"
  "GuiTreeArrowRight" -> "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"12\" height=\"12\"><path fill=\"none\" stroke=\"#fff\" stroke-linecap=\"round\" stroke-linejoin=\"round\" stroke-opacity=\".4\" stroke-width=\"2\" d=\"m5 9 3-3-3-3\"/></svg>"
  "GuiTreeArrowLeft" -> "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"12\" height=\"12\"><path fill=\"none\" stroke=\"#fff\" stroke-linecap=\"round\" stroke-linejoin=\"round\" stroke-opacity=\".4\" stroke-width=\"2\" d=\"M7 3 4 6l3 3\"/></svg>"
  "GuiTreeUpdown" -> "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"16\" height=\"16\"><path fill=\"none\" stroke=\"#fff\" stroke-linecap=\"round\" stroke-linejoin=\"round\" stroke-opacity=\".6\" stroke-width=\"2\" d=\"m5 6 3-3 3 3m0 4-3 3-3-3\"/></svg>"
  "GuiDropdown" -> "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"14\" height=\"14\"><path fill=\"none\" stroke=\"#fff\" stroke-linecap=\"round\" stroke-linejoin=\"round\" stroke-opacity=\".6\" stroke-width=\"2\" d=\"m4 7 3 3 3-3\"/></svg>"
  "GuiSpinboxUp" -> "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"16\" height=\"8\"><path fill=\"none\" stroke=\"#e0e0e0\" stroke-linecap=\"round\" stroke-linejoin=\"round\" stroke-opacity=\".8\" stroke-width=\"2\" d=\"m4 6 4-4 4 4\"/></svg>"
  "GuiSpinboxDown" -> "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"16\" height=\"8\"><path fill=\"none\" stroke=\"#e0e0e0\" stroke-linecap=\"round\" stroke-linejoin=\"round\" stroke-opacity=\".8\" stroke-width=\"2\" d=\"m12 2-4 4-4-4\"/></svg>"
  "ArrowRight" -> "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"16\" height=\"16\"><path fill=\"none\" stroke=\"#e0e0e0\" stroke-linecap=\"round\" stroke-linejoin=\"round\" stroke-width=\"2\" d=\"M5 8h7L8 4m4 4-4 4\"/></svg>"
  "ArrowLeft" -> "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"16\" height=\"16\"><path fill=\"none\" stroke=\"#e0e0e0\" stroke-linecap=\"round\" stroke-linejoin=\"round\" stroke-width=\"2\" d=\"M11 8H4l4 4M4 8l4-4\"/></svg>"
  _ -> error ("Godot.ImUI.Theme: no icon " ++ name)
