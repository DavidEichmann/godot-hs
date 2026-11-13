{-# LANGUAGE TypeApplications #-}

-- | Hello world with godot-low: embed Godot, open a window and show a
-- centred "Hello, World!" label. Runs until the window is closed.
module Main (main) where

import Control.Monad (unless)
import Godot.Api
import Godot.Api.Embed
import qualified Godot.Api.Class.Control as Control
import qualified Godot.Api.Class.Engine as Engine
import qualified Godot.Api.Class.GodotInstance as GodotInstance
import qualified Godot.Api.Class.Label as Label
import qualified Godot.Api.Class.Node as Node
import qualified Godot.Api.Class.SceneTree as SceneTree
import qualified Godot.Api.Class.Window as Window

main :: IO ()
main = withGodot project [] $ \inst -> do
  -- The scene tree's root is the main window.
  Just mainLoop <- Engine.getMainLoop =<< Engine.singleton
  Just tree <- cast @SceneTree mainLoop
  Just root <- SceneTree.getRoot tree
  Window.setTitle root =<< toGString "Hello from Haskell"

  -- A label filling the window, with its text centred.
  label <- Label.construct
  Label.setText label =<< toGString "Hello, World!"
  Label.setHorizontalAlignment label HORIZONTAL_ALIGNMENT_CENTER
  Label.setVerticalAlignment label VERTICAL_ALIGNMENT_CENTER
  fontSize <- toStringName "font_size"
  Control.addThemeFontSizeOverride label fontSize 48
  Node.addChild root (upcast label) False Node_INTERNAL_MODE_DISABLED
  Control.setAnchorsAndOffsetsPreset label Control_PRESET_FULL_RECT Control_PRESET_MODE_MINSIZE 0

  -- Run frames until Godot asks to quit (e.g. the window is closed).
  let loop = do
        quit <- GodotInstance.iteration inst
        unless quit loop
  loop

-- | The UI is built from Haskell above, so the project only has settings.
project :: Project
project =
  defaultProject
    { projectSettings =
        [ "config/name=\"godot-low example\""
        , ""
        , -- The OpenGL renderer renders on the GPU that drives the desktop.
          -- (Godot's default Vulkan renderer prefers a discrete GPU, which
          -- can freeze the desktop on hybrid graphics laptops.)
          "[rendering]"
        , "renderer/rendering_method=\"gl_compatibility\""
        , ""
        , "[display]"
        , "window/size/viewport_width=640"
        , "window/size/viewport_height=360"
        ]
    }
