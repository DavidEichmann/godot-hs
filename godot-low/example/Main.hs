{-# LANGUAGE TypeApplications #-}

-- | Hello world with godot-low: embed Godot, open a window and show a
-- centred "Hello, World!" label. Runs until the window is closed.
module Main (main) where

import Control.Monad (unless, when)
import Foreign.Ptr (freeHaskellFunPtr, nullPtr)
import Foreign.Storable (poke)
import Godot.Api
import qualified Godot.Api.Class.Control as Control
import qualified Godot.Api.Class.Engine as Engine
import qualified Godot.Api.Class.GodotInstance as GodotInstance
import qualified Godot.Api.Class.Label as Label
import qualified Godot.Api.Class.Node as Node
import qualified Godot.Api.Class.SceneTree as SceneTree
import qualified Godot.Api.Class.Window as Window
import qualified Godot.GDExtension as GDE
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory, removeDirectoryRecursive)
import System.Exit (exitFailure)
import System.FilePath ((</>))

main :: IO ()
main = do
  -- Godot calls this while creating the instance. We only need to load the
  -- GDExtension interface; no classes are registered.
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

  -- libgodot is an export template, so it needs a project with a main
  -- scene. An empty one is enough: the UI is built from Haskell below.
  project <- (</> "godot-low-example-project") <$> getTemporaryDirectory
  createDirectoryIfMissing True project
  writeFile (project </> "project.godot") $
    unlines
      [ "config_version=5"
      , ""
      , "[application]"
      , "config/name=\"godot-low example\""
      , "run/main_scene=\"res://main.tscn\""
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
  writeFile (project </> "main.tscn") $
    unlines ["[gd_scene format=3]", "", "[node name=\"Main\" type=\"Node\"]"]

  GDE.withGodotArgs ["godot-low-example", "--path", project] $ \argc argv -> do
    instPtr <- GDE.createGodotInstance argc argv initFn
    when (instPtr == GDE.GDExtensionObjectPtr nullPtr) $ do
      putStrLn "createGodotInstance returned NULL"
      exitFailure
    let inst = unsafeFromObjectPtr @GodotInstance instPtr
    started <- GodotInstance.start inst
    unless started $ do
      putStrLn "GodotInstance.start failed"
      exitFailure

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

    GDE.destroyGodotInstance instPtr

  removeDirectoryRecursive project
  freeHaskellFunPtr initFn
  freeHaskellFunPtr onLevel
