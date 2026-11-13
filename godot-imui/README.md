# godot-imui

A simple immediate mode UI library for Haskell, rendered by Godot (through
[godot-low](../godot-low)).

```haskell
import Control.Monad (when)
import Godot.ImUI

main :: IO ()
main = runUI defaultWindow $ do
  (count, setCount) <- state (0 :: Int)
  label ("Count: " ++ show count)
  clicked <- button "Increment"
  when clicked (liftIO (setCount (count + 1)))
```

- **Code first.** That is the whole program: there is no Godot project,
  scene or editor involved.
- **Immediate mode.** The UI is code that runs every frame. Widgets return what the user did (`button` returns whether it was
  clicked), so there are no callbacks and nothing to keep in sync.
- **UI only.** Godot is a full game engine; this exposes just its 2D UI
  controls.

Run the tour of the widgets with `cabal run godot-imui-example`
([example/Main.hs](example/Main.hs)).

## Using it

`runUI window ui` opens a window and runs `ui`, which describes the
interface, every frame until the window is closed or `quit` is called. The
UI keeps its state with `state`, like React's `useState`: `state initial`
returns the current value and a function that sets it, from the next frame
on (see [State](#state)).

```haskell
{-# LANGUAGE DuplicateRecordFields #-}

import Data.Char (toUpper)
import Godot.ImUI

ui :: UI ()
ui = do
  (name, setName) <- state ""
  (shout, setShout) <- state False
  name' <- hbox $ do
    label "Name:"
    textInputWith textInputOpts {placeholder = "Your name"} name
  shout' <- checkbox "Shout" shout
  liftIO (setName name' >> setShout shout')
  let greeting = "Hello, " ++ name' ++ "!"
  label (if shout' then map toUpper greeting else greeting)

main :: IO ()
main = runUI defaultWindow {windowTitle = "Greeter"} ui
```

- Widgets that edit a value take the current value and return the new one.
  What a widget shows is always the value you pass it: don't keep the new
  value and the edit is undone.
- Each widget has a variant ending in `With` that takes its options, such
  as `buttonWith buttonOpts {enabled = False} "OK"` (see
  [Options](#options)).
- Ordinary control flow decides what is shown: `when`, `case`, `forM`, ...
  Widgets that are not created in a frame disappear.
- `WindowConfig` sets the window's title, size and look:
  `defaultWindow {windowTitle = "Greeter", windowTheme = ModernLight}`.
  The themes are `ModernDark` (the default), `ModernLight` and
  `GodotDefault` (see [The look](#the-look)).
- `UI` is a `MonadIO`, so `liftIO` runs any `IO` (keep it short: it runs on
  the UI thread, every frame unless you guard it).

### State

A part of the UI can keep its own state, such as this counter:

```haskell
counter :: UI ()
counter = hbox $ do
  (count, setCount) <- state (0 :: Int)
  clicked <- button "+1"
  when clicked (liftIO (setCount (count + 1)))
  label (show count)
```

A state belongs to the container it is created in, and is matched from frame
to frame by the order in which that container creates its states. So, as
with React's hooks, create them in the same order every frame (not under a
condition that changes, unless they are in a container of their own). The
state is gone when its container is no longer created, and starts from the
initial value if it is created again: the `counter` above starts again
from 0 when it is hidden and shown. Key a list item's container to keep its
state with the item.

### Work in the background

`asyncState` runs `IO` work in a thread of its own, so that the UI keeps
going while it runs: a download, an install, a slow computation, ...

```haskell
(number, start, stop) <- asyncState Nothing
go <- button "Think of a number"
when go (liftIO (start slowNumber))
label $ case number of
  None -> ""
  Running _ -> "Thinking..."
  Done n -> "I thought of " ++ show n
  Failed e _ -> "Failed: " ++ show e
```

- It takes the work to start when it is created (`Nothing` for none), and
  returns where the work is, an action that starts other work, and one that
  cancels the work running. Starting work cancels the work running, if any.
- `Running` and `Failed` keep the value of the last work that finished
  (`latestValue` gets it from any of them), and cancelling keeps it too.
- The result shows from the frame after the work finishes.
- Like a `state`, it belongs to its container: when the container is no
  longer created, the work is cancelled.
- The work must not use Godot or the UI, which only work on the UI's
  thread: it returns its result for the UI to show.

For other things to clean up when their part of the UI goes away,
`Godot.ImUI.Internal.stateRef` is a state with a cleanup action.

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
| `progressBar :: Double -> UI ()` | a fraction from 0 to 1 (or, with the `indeterminate` option, an animation for work whose progress is not known) |
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
keep whether it is open in a `state`.

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

### Options

Each widget function has a variant ending in `With` that takes the widget's
options first: a record of type `ButtonOpts`, `TextInputOpts`, ..., written
as the defaults (`buttonOpts`, `textInputOpts`, ...) with the options you
want changed:

```haskell
ok <- buttonWith buttonOpts {enabled = not (null name), tooltip = "Save and close"} "OK"
```

Every widget `foo` has its options type `FooOpts` and defaults `fooOpts`.
Variants of a widget share a record, and their names are aliases:
`ParagraphOpts` is `LabelOpts`, `LinkOpts` is `ButtonOpts`, `HBoxOpts` is
`BoxOpts`, `ConfirmOpts` is `DialogOpts`, ... Most widgets have these
options:

| Option | Description |
| --- | --- |
| `enabled :: Bool` | `False` greys out the widget (not the ones in a container) |
| `expand :: Bool` | grow to take the space left in the container |
| `minSize :: (Int, Int)` | minimum width and height in pixels |
| `tooltip :: String` | text shown on hover |
| `key :: String` | identity, see below |

and `textInput` and `textArea` have a `placeholder`, the hint shown while
they are empty.

The option records share field names, so a module that sets options needs
`DuplicateRecordFields`. GHC knows which record an update is of from the
`With` function it is passed to, but warns that it will stop doing that in
a future version (`-Wambiguous-fields`); turn the warning off with
`{-# OPTIONS_GHC -Wno-ambiguous-fields #-}`.

### Widget identity

The Godot controls behind your widgets live on from frame to frame, which is
what keeps keyboard focus, text cursors and scroll positions. A widget is
matched with last frame's control by its position among the widgets of the
same kind in its container. That is usually what you want. For a list whose
items can be removed or reordered, key each item's widgets by something that
identifies the item, so that the controls follow the items:

```haskell
forM items $ \item ->
  hboxWith hboxOpts {key = show (itemId item)} $ do ...
```

The widgets inside a keyed container go with it.

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

## Smaller programs

A program embeds all of Godot, about 120 MB. godot-imui only needs Godot's 2D
UI, so godot-sys can build a Godot without the rest, which halves that. In
your `cabal.project`:

```cabal
package godot-sys
  flags:
    +release +optimize-size
    +disable-3d +disable-physics-2d +disable-navigation-2d -vulkan -sdl
    -module-astcenc-enabled -module-basis-universal-enabled
    -module-bcdec-enabled -module-betsy-enabled -module-bmp-enabled
    -module-camera-enabled -module-cvtt-enabled -module-dds-enabled
    -module-enet-enabled -module-etcpak-enabled -module-gdscript-enabled
    -module-glslang-enabled -module-hdr-enabled
    -module-interactive-music-enabled -module-jsonrpc-enabled
    -module-ktx-enabled -module-lightmapper-rd-enabled -module-mbedtls-enabled
    -module-mp3-enabled -module-msdfgen-enabled -module-multiplayer-enabled
    -module-noise-enabled -module-objectdb-profiler-enabled -module-ogg-enabled
    -module-raycast-enabled -module-regex-enabled -module-tga-enabled
    -module-theora-enabled -module-tinyexr-enabled -module-upnp-enabled
    -module-visual-shader-enabled -module-vorbis-enabled
    -module-webrtc-enabled -module-websocket-enabled
    -module-xatlas-unwrap-enabled -module-zip-enabled
```

The modules left are FreeType and the text server (fonts), and SVG, JPEG and
WebP images (PNG is built in); the 3D, physics and navigation modules go
with the `disable-*` flags. GDScript, regular expressions, audio formats and
networking are gone. The example is then 55 MB (37 MB stripped) instead of
120 MB. godot-sys's
`disable-advanced-gui` and `-opengl3` cannot be used: godot-imui's menus,
trees, text areas and dialogs are Godot's advanced controls, and it renders
with OpenGL. See [godot-sys](../godot-sys#build-options) for the options.

## Limitations

- `runUI` must be called from the main thread, and only once per process
  (Godot cannot be restarted). Link with `-threaded`.
- No custom drawing, keyboard shortcuts for menu items, icons on buttons or
  multi-column trees yet, and no styling beyond the three themes.
- `enabled` cannot disable a whole group of widgets at once: set it on each.
- File dialogs are Godot's own, not the desktop's.
- Widget values are read back from Godot every frame, so a very large
  `textArea` costs time even when idle.

## Tests

`cabal test godot-imui` runs a UI headless and plays the user by changing
the Godot controls between frames ([test/Main.hs](test/Main.hs)).
