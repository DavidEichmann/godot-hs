# godot-imui

A simple immediate mode UI library for Haskell, rendered by Godot (through
[godot-low](../godot-low)).

```haskell
import Godot.ImUI

main :: IO ()
main = runUI defaultWindow (0 :: Int) $ \count -> do
  label ("Count: " ++ show count)
  clicked <- button "Increment"
  pure (if clicked then count + 1 else count)
```

- **Code first.** That is the whole program: there is no Godot project,
  scene or editor involved.
- **Immediate mode.** The UI is a function of your state that runs every
  frame. Widgets return what the user did (`button` returns whether it was
  clicked), so there are no callbacks and nothing to keep in sync.
- **UI only.** Godot is a full game engine; this exposes just its 2D UI
  controls.

Run the tour of the widgets with `cabal run godot-imui-example`
([example/Main.hs](example/Main.hs)).

## Using it

`runUI window initialState ui` opens a window and calls `ui` with the current
state every frame, until the window is closed or `quit` is called. `ui`
describes the interface and returns the next state.

```haskell
import Data.Char (toUpper)
import Godot.ImUI

data State = State {name :: String, shout :: Bool}

ui :: State -> UI State
ui s = do
  name' <- hbox $ do
    label "Name:"
    placeholder "Your name" (textInput (name s))
  shout' <- checkbox "Shout" (shout s)
  let greeting = "Hello, " ++ name' ++ "!"
  label (if shout' then map toUpper greeting else greeting)
  pure s {name = name', shout = shout'}

main :: IO ()
main = runUI defaultWindow {windowTitle = "Greeter"} (State "" False) ui
```

- Widgets that edit a value take the current value and return the new one.
  What a widget shows is always the value you pass it: return the state
  unchanged and the edit is undone.
- Ordinary control flow decides what is shown: `when`, `case`, `forM`, ...
  Widgets that are not created in a frame disappear.
- `WindowConfig` sets the window's title, size and look:
  `defaultWindow {windowTitle = "Greeter", windowTheme = ModernLight}`.
  The themes are `ModernDark` (the default), `ModernLight` and
  `GodotDefault` (see [The look](#the-look)).
- `UI` is a `MonadIO`, so `liftIO` runs any `IO` (keep it short: it runs on
  the UI thread, every frame unless you guard it). If you prefer mutable
  state, use `()` as the state and keep yours in `IORef`s.

### Widgets

| Widget | Description |
| --- | --- |
| `label`, `heading`, `paragraph` | text: one line, large, wrapping |
| `richText :: String -> UI ()` | formatted text, written in Godot's BBCode |
| `image :: FilePath -> UI ()` | an image file; scaled to fit with `minSize` or `expand` |
| `button`, `link :: String -> UI Bool` | `True` when clicked |
| `checkbox`, `toggle :: String -> Bool -> UI Bool` | a labelled checkbox; an on/off switch |
| `radioButtons :: [String] -> Int -> UI Int` | pick one option, by index |
| `textInput`, `passwordInput :: String -> UI String` | single-line text field |
| `textArea :: String -> UI String` | multi-line text editor |
| `slider :: (Double, Double) -> Double -> UI Double` | a number in a range |
| `spinBox :: (Int, Int) -> Int -> UI Int` | a whole number in a range |
| `dropdown`, `listBox :: [String] -> Int -> UI Int` | pick one option, by index |
| `tree :: Forest String -> Maybe [Int] -> UI (Maybe [Int])` | collapsible tree (`Data.Tree`); the selection is the path of indices to the item |
| `colorPicker :: Color -> UI Color` | a button that opens a colour picker |
| `tabBar :: [String] -> Int -> UI Int` | tabs; show the current tab's content yourself |
| `progressBar :: Double -> UI ()` | a fraction from 0 to 1 |
| `separator`, `spacer` | a dividing line; empty space that pushes widgets apart |

### Menus

A menu is a block of items, and an item returns `True` when it is chosen. A
`menu` inside a `menu` is a submenu; outside a `menuBar` it is a button that
opens the menu.

```haskell
menuBar $ do
  menu "File" $ do
    open <- menuItem "Open..."
    menuSeparator
    close <- menuItem "Quit"
    when close quit
  menu "View" $ do
    wrap' <- menuCheck "Wrap lines" wrap
    ...
```

### Dialogs

A dialog is a window of its own that is open for as long as you create it:
keep whether it is open in your state.

```haskell
answer <- if asking then confirm "Delete" "Delete this item?" else pure Nothing
case answer of
  Nothing -> ...       -- not asked, or not answered yet
  Just ok -> ...       -- stop asking; delete if ok
```

| Dialog | Description |
| --- | --- |
| `dialog`, `window :: String -> UI a -> UI (Maybe a)` | a window holding a block of widgets (`dialog` blocks the rest of the UI); `Nothing` when the user closes it |
| `messageBox :: String -> String -> UI Bool` | a message; `True` when dismissed |
| `confirm :: String -> String -> UI (Maybe Bool)` | OK / Cancel; `Nothing` until answered |
| `openFileDialog`, `saveFileDialog :: String -> [String] -> UI (Choice FilePath)` | pick a file, given a title and patterns like `"*.png"` |
| `directoryDialog :: String -> UI (Choice FilePath)` | pick a directory |

`Choice` is `Choosing` (still open), `Cancelled` or `Chose path`.

### Layout

Containers take a block of widgets and return its result. The top level is a
`vbox`.

| Container | Description |
| --- | --- |
| `vbox`, `hbox` | stack vertically; lay out in a row |
| `grid n` | `n` columns, filled row by row |
| `flow` | a row that wraps onto further rows |
| `panel` | a framed group |
| `collapsible "title"` | a group the user can fold away |
| `scroll` | scrolls if the content does not fit; takes the space left |
| `hsplit a b`, `vsplit a b` | two groups with a draggable divider; returns both results |
| `center` | centres its content in the space left |
| `padding n` | `n` pixels of empty space around the content |

### Modifiers

Modifiers wrap a block and apply to the widgets created directly in it:

| Modifier | Description |
| --- | --- |
| `enabled False` | grey out (also applies to nested widgets) |
| `expand` | grow to take the space left in the container |
| `minSize w h` | minimum size in pixels |
| `tooltip "..."` | text shown on hover |
| `placeholder "..."` | hint in empty text fields |
| `keyed "..."` | identity, see below |

```haskell
ok <- enabled (not (null name)) (tooltip "Save and close" (button "OK"))
```

### Widget identity

The Godot controls behind your widgets live on from frame to frame, which is
what keeps keyboard focus, text cursors and scroll positions. A widget is
matched with last frame's control by its position among the widgets of the
same kind in its container. That is usually what you want. For a list whose
items can be removed or reordered, key each item's widgets by something that
identifies the item, so that the controls follow the items:

```haskell
forM items $ \item ->
  keyed (show (itemId item)) $ hbox $ do ...
```

## How it works

Godot's UI is retained: a tree of `Control` nodes. Each frame, godot-imui
runs your function and reconciles the widgets it creates with the controls
from the previous frame: matching ones are reused, new ones are created, and
the rest are freed. Properties are sent to Godot only when they change, and
clicks arrive through Godot's signals.

Menu items and tree items are not nodes in Godot, so those are collected
while their block runs and synced with the popup or tree afterwards.

`runUI` starts Godot inside your process (libgodot from
[godot-sys](../godot-sys)). Godot can only run a project, so an empty one is
written to a temporary directory (with the theme) and removed on exit. Godot is started
with its OpenGL ("Compatibility") renderer, which renders on the GPU that
drives the desktop (on a hybrid graphics laptop, presenting frames rendered
on the discrete GPU can freeze the desktop), without audio, and in its low
processor mode, in which it only redraws when something changed. Your function is still called for every frame (about 100
times a second when idle), so keep it cheap.

## The look

The controls look like Godot's editor (its "Modern" style), in its default
dark colours (`ModernDark`) or its light preset (`ModernLight`). Godot only
has that style in its editor, which is not part of libgodot; applications get
Godot's older default theme (`GodotDefault`). So
[Godot.ImUI.Theme](src/Godot/ImUI/Theme.hs) ports the editor's style: it
computes the colours, margins and styleboxes the way the editor does, embeds
the editor's SVG icons (checkboxes, toggles, tree arrows, ...), and writes it
all as a Godot theme resource in the temporary project. It differs from the
editor in a few places to suit an application: `panel` is outlined,
`richText` has no box around it, and menu bars are flat like the editor's main
menu. The font is Godot's default font, not the editor's.

## Other controls

[Godot.ImUI.Internal](src/Godot/ImUI/Internal.hs) has the building blocks
the widgets are made of, for wrapping any other Godot `Control` with
godot-low's bindings:

```haskell
import Godot.Api (toGString)
import qualified Godot.Api.Class.LinkButton as LinkButton
import Godot.ImUI.Internal

link :: String -> UI Bool
link text = do
  (w, b) <- widget "LinkButton" natural LinkButton.construct
  prop w "text" text (\t -> toGString t >>= LinkButton.setText b)
  fired w "pressed"
```

## Limitations

- `runUI` must be called from the main thread, and only once per process
  (Godot cannot be restarted). Link with `-threaded`.
- No custom drawing, keyboard shortcuts for menu items, icons on buttons or
  multi-column trees yet, and no styling beyond the three themes.
- File dialogs are Godot's own, not the desktop's.
- Widget values are read back from Godot every frame, so a very large
  `textArea` costs time even when idle.

## Tests

`cabal test godot-imui` runs a UI headless and plays the user by changing
the Godot controls between frames ([test/Main.hs](test/Main.hs)).
