{-# LANGUAGE TypeApplications #-}

-- | End-to-end test: embed Godot with Godot.Api.Embed, use the GDExtension
-- interface directly, and drive the engine through the generated Godot.Api
-- bindings.
module Main (main) where

import Control.Monad (unless)
import Foreign.C.String (peekCString)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable (peek)
import Godot.Api
import Godot.Api.Embed
import Godot.Api.Internal (getInterface)
import qualified Godot.Api.Builtin.Vector3 as Vector3
import qualified Godot.Api.Class.Engine as Engine
import qualified Godot.Api.Class.GodotInstance as GodotInstance
import qualified Godot.Api.Class.Node as Node
import qualified Godot.Api.Class.Object as Object
import qualified Godot.Api.Class.SceneTree as SceneTree
import qualified Godot.Api.Utility as Utility
import qualified Godot.GDExtension as GDE
import System.Exit (exitFailure)

check :: String -> Bool -> IO ()
check what ok = do
  putStrLn ((if ok then "ok   " else "FAIL ") ++ what)
  unless ok exitFailure

main :: IO ()
main = withGodot defaultProject ["--headless"] $ \inst -> do
  -- Low level: the GDExtension interface directly.
  iface <- getInterface
  v <- alloca $ \pv -> GDE.get_godot_version2 iface pv >> peek pv
  str <- peekCString (GDE.gdExtensionGodotVersion2_string v)
  putStrLn ("Godot reports version " ++ str)
  check "GDExtension get_godot_version2"
    ((GDE.gdExtensionGodotVersion2_major v, GDE.gdExtensionGodotVersion2_minor v, GDE.gdExtensionGodotVersion2_patch v) == (4, 7, 2))

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
  notControl <- cast @Control node
  check "cast: a Node is not a Control" (null notControl)
  Just root <- SceneTree.getRoot tree
  Node.addChild root node False Node_INTERNAL_MODE_DISABLED
  path <- stringify =<< toVariant =<< Node.getPath node
  check ("Node.addChild / getPath: " ++ path) (path == "/root/Hello")
  children <- Node.getChildCount root False
  check ("Node.getChildCount root: " ++ show children) (children >= 2)

  _ <- GodotInstance.iteration inst
  pure ()
