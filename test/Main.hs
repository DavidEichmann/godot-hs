module Main (main) where

import Control.Monad (when)
import Foreign.Ptr (nullPtr)
import Godot.Sys
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory, removeDirectoryRecursive)
import System.Exit (exitFailure)
import System.FilePath ((</>))

foreign import ccall "&godot_sys_test_init"
  testInit :: GDExtensionInitializationFunction

main :: IO ()
main = do
  putStrLn ("godot-sys: Godot " ++ godotVersion)
  -- Export templates refuse to start without a project, so give it an empty one.
  tmp <- getTemporaryDirectory
  let project = tmp </> "godot-sys-test-project"
  createDirectoryIfMissing True project
  writeFile (project </> "project.godot") $
    unlines ["config_version=5", "", "[application]", "run/main_scene=\"res://main.tscn\""]
  writeFile (project </> "main.tscn") $
    unlines ["[gd_scene format=3]", "", "[node name=\"Main\" type=\"Node\"]"]
  withGodotArgs ["godot-sys-test", "--headless", "--path", project] $ \argc argv -> do
    inst <- libgodotCreateGodotInstance argc argv testInit
    when (inst == nullPtr) $ do
      putStrLn "libgodot_create_godot_instance returned NULL"
      exitFailure
    putStrLn "created Godot instance"
    libgodotDestroyGodotInstance inst
    putStrLn "destroyed Godot instance"
  removeDirectoryRecursive project
