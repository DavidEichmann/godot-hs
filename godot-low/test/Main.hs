-- | End-to-end test: embed Godot, load the generated interface from inside a
-- Haskell GDExtension initialization function, and ask Godot for its version.
module Main (main) where

import Control.Monad (unless, when)
import Data.IORef
import Foreign.C.String (peekCString)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (freeHaskellFunPtr, nullPtr)
import Foreign.Storable (peek, poke)
import Godot.Low
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory, removeDirectoryRecursive)
import System.Exit (exitFailure)
import System.FilePath ((</>))

main :: IO ()
main = do
  result <- newIORef Nothing
  levels <- newIORef []

  onLevel <- wrapGDExtensionInitializeCallback $ \_ level -> modifyIORef levels (level :)
  initFn <- wrapGDExtensionInitializationFunction $ \gpa _library rInit -> do
    iface <- loadInterface gpa
    version <- alloca $ \pv -> do
      get_godot_version2 iface pv
      peek pv
    str <- peekCString (gdExtensionGodotVersion2_string version)
    writeIORef result (Just (version, str))
    poke
      rInit
      GDExtensionInitialization
        { gdExtensionInitialization_minimum_initialization_level = GDEXTENSION_INITIALIZATION_CORE
        , gdExtensionInitialization_userdata = nullPtr
        , gdExtensionInitialization_initialize = onLevel
        , gdExtensionInitialization_deinitialize = onLevel
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

  withGodotArgs ["godot-low-test", "--headless", "--path", project] $ \argc argv -> do
    inst <- createGodotInstance argc argv initFn
    when (inst == GDExtensionObjectPtr nullPtr) $ do
      putStrLn "createGodotInstance returned NULL"
      exitFailure
    destroyGodotInstance inst
  removeDirectoryRecursive project
  freeHaskellFunPtr initFn

  readIORef result >>= \r -> case r of
    Nothing -> putStrLn "initialization function was never called" >> exitFailure
    Just (v, str) -> do
      putStrLn ("Godot reports version " ++ str)
      let triple =
            ( gdExtensionGodotVersion2_major v
            , gdExtensionGodotVersion2_minor v
            , gdExtensionGodotVersion2_patch v
            )
      unless (triple == (4, 7, 2)) $ do
        putStrLn ("unexpected version " ++ show triple)
        exitFailure
  seen <- readIORef levels
  putStrLn ("initialization levels seen: " ++ show (reverse seen))
  when (null seen) $ putStrLn "initialize callback never ran" >> exitFailure
  freeHaskellFunPtr onLevel
