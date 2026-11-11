{-# LANGUAGE TypeApplications #-}

-- | End-to-end test: embed Godot, load the GDExtension interface from a
-- Haskell initialization function, start the engine and drive it through the
-- generated Godot.Api bindings.
module Main (main) where

import Control.Monad (unless, when)
import Data.IORef
import Foreign.C.String (peekCString)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (freeHaskellFunPtr, nullPtr)
import Foreign.Storable (peek, poke)
import Godot.Api
import qualified Godot.Api.Builtin.Vector3 as Vector3
import qualified Godot.Api.Class.Engine as Engine
import qualified Godot.Api.Class.GodotInstance as GodotInstance
import qualified Godot.Api.Class.Node as Node
import qualified Godot.Api.Class.Object as Object
import qualified Godot.Api.Class.SceneTree as SceneTree
import qualified Godot.Api.Utility as Utility
import qualified Godot.GDExtension as GDE
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory, removeDirectoryRecursive)
import System.Exit (exitFailure)
import System.FilePath ((</>))

check :: String -> Bool -> IO ()
check what ok = do
  putStrLn ((if ok then "ok   " else "FAIL ") ++ what)
  unless ok exitFailure

main :: IO ()
main = do
  versionRef <- newIORef Nothing
  levels <- newIORef []

  onLevel <- GDE.wrapGDExtensionInitializeCallback $ \_ level -> modifyIORef levels (level :)
  initFn <- GDE.wrapGDExtensionInitializationFunction $ \gpa library rInit -> do
    -- Low level: the GDExtension interface directly.
    iface <- initialize gpa library
    version <- alloca $ \pv -> GDE.get_godot_version2 iface pv >> peek pv
    str <- peekCString (GDE.gdExtensionGodotVersion2_string version)
    writeIORef versionRef (Just (version, str))
    poke
      rInit
      GDE.GDExtensionInitialization
        { GDE.gdExtensionInitialization_minimum_initialization_level = GDE.GDEXTENSION_INITIALIZATION_CORE
        , GDE.gdExtensionInitialization_userdata = nullPtr
        , GDE.gdExtensionInitialization_initialize = onLevel
        , GDE.gdExtensionInitialization_deinitialize = onLevel
        }
    pure 1

  -- Export templates need a project with a main scene.
  tmp <- getTemporaryDirectory
  let project = tmp </> "godot-low-test-project"
  createDirectoryIfMissing True project
  writeFile (project </> "project.godot") $
    unlines ["config_version=5", "", "[application]", "run/main_scene=\"res://main.tscn\""]
  writeFile (project </> "main.tscn") $
    unlines ["[gd_scene format=3]", "", "[node name=\"Main\" type=\"Node\"]"]

  GDE.withGodotArgs ["godot-low-test", "--headless", "--path", project] $ \argc argv -> do
    instPtr <- GDE.createGodotInstance argc argv initFn
    when (instPtr == GDE.GDExtensionObjectPtr nullPtr) $ do
      putStrLn "createGodotInstance returned NULL"
      exitFailure

    Just (v, str) <- readIORef versionRef
    putStrLn ("Godot reports version " ++ str)
    check "GDExtension get_godot_version2"
      ((GDE.gdExtensionGodotVersion2_major v, GDE.gdExtensionGodotVersion2_minor v, GDE.gdExtensionGodotVersion2_patch v) == (4, 7, 2))

    -- Start the engine through the generated GodotInstance bindings.
    let inst = unsafeFromObjectPtr @GodotInstance instPtr
    started <- GodotInstance.start inst
    check "GodotInstance.start" started

    -- Engine classes: create a node, set and read its name.
    node <- Node.construct
    Node.setName node =<< toStringName "Hello"
    name <- fromStringName =<< Node.getName node
    check ("Node.setName / getName: " ++ show name) (name == "Hello")

    -- Builtin type method.
    len <- Vector3.length (Vector3 1 2 2)
    check ("Vector3.length: " ++ show len) (len == 3)

    -- Utility functions.
    a <- Utility.absi (-5)
    r <- Utility.sqrt 9
    check ("Utility.absi / sqrt: " ++ show (a, r)) (a == 5 && r == 3)

    -- Singletons.
    engine <- Engine.singleton
    arch <- fromGString =<< Engine.getArchitectureName engine
    check ("Engine.getArchitectureName: " ++ arch) (not (null arch))

    -- Vararg method with variants (Object.call), on a subclass via Inherits.
    viaCall <- stringify =<< Object.call node . pure =<< toVariant =<< toStringName "get_name"
    check ("Object.call \"get_name\": " ++ viaCall) (viaCall == "Hello")

    -- The running scene tree: add the node under the root window.
    Just mainLoop <- Engine.getMainLoop engine
    Just tree <- cast @SceneTree mainLoop
    Just root <- SceneTree.getRoot tree
    Node.addChild root node False Node_INTERNAL_MODE_DISABLED
    path <- stringify =<< toVariant =<< Node.getPath node
    check ("Node.addChild / getPath: " ++ path) (path == "/root/Hello")
    children <- Node.getChildCount root False
    check ("Node.getChildCount root: " ++ show children) (children >= 2)

    _ <- GodotInstance.iteration inst
    GDE.destroyGodotInstance instPtr

  removeDirectoryRecursive project
  freeHaskellFunPtr initFn
  seen <- readIORef levels
  putStrLn ("initialization levels seen: " ++ show (reverse seen))
  check "initialize callback ran" (not (null seen))
  freeHaskellFunPtr onLevel
