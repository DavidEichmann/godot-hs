{-# LANGUAGE TypeApplications #-}

-- | Run Godot embedded in this process.
--
-- @
-- main = withGodot defaultProject [] $ \\inst -> do
--   -- build the scene with "Godot.Api", then run frames until Godot quits
--   let loop = do
--         quit <- GodotInstance.iteration inst
--         unless quit loop
--   loop
-- @
module Godot.Api.Embed
  ( withGodot
  , Project (..)
  , defaultProject
  ) where

import Control.Exception (bracket, finally, throwIO, try)
import Control.Monad (unless, when)
import Data.IORef
import Foreign.Ptr (freeHaskellFunPtr, nullPtr)
import Foreign.Storable (poke)
import GHC.Clock (getMonotonicTimeNSec)
import Godot.Api.Classes (GodotInstance)
import qualified Godot.Api.Class.GodotInstance as GodotInstance
import Godot.Api.Internal (GodotClass (..), initialize)
import qualified Godot.GDExtension as GDE
import System.Directory
import System.Environment (getProgName)
import System.FilePath
import System.IO.Error (isAlreadyExistsError)
import System.IO.Unsafe (unsafePerformIO)

-- | The Godot project to run. libgodot is an export template, which only
-- runs a project with a main scene: 'withGodot' writes one with an empty
-- scene, plus these.
data Project = Project
  { projectSettings :: [String]
  -- ^ More lines of @project.godot@, after its @[application]@ section's
  -- @run/main_scene@: more settings of that section, then other sections.
  , projectFiles :: [(FilePath, String)]
  -- ^ More files: their paths in the project, and contents.
  }

-- | An empty project.
defaultProject :: Project
defaultProject = Project [] []

-- | Run Godot embedded in this process: write the project to a fresh
-- temporary directory, create a Godot instance for it, load the GDExtension
-- interface (see 'Godot.Api.initialize'), start the instance and run the
-- action with it. Afterwards the instance is destroyed and the project
-- removed.
--
-- The arguments are Godot command line arguments, e.g. @["--headless"]@.
-- The working directory is restored once Godot is running (Godot changes it
-- to the project's), so relative paths keep meaning what they did.
--
-- Call it from the main thread, and only once per process: Godot cannot be
-- started again (and a second call fails, unless the first failed before
-- creating the instance). The project is removed however it ends.
withGodot :: Project -> [String] -> (GodotInstance -> IO a) -> IO a
withGodot project args body = do
  ran <- readIORef alreadyRun
  when ran $ fail "Godot.Api.Embed.withGodot: Godot can only be started once per process"
  progName <- getProgName
  cwd <- getCurrentDirectory
  -- Each resource is released however the rest ends, in reverse order: the
  -- instance, the callbacks, the working directory (which Godot changes to
  -- the project), and last the project.
  bracket freshDirectory removeDirectoryRecursive $ \dir ->
    (`finally` setCurrentDirectory cwd) $ do
      writeProject dir project
      -- Godot calls this while creating the instance. Nothing is registered:
      -- only the GDExtension interface is needed.
      bracket (GDE.wrapGDExtensionInitializeCallback (\_ _ -> pure ())) freeHaskellFunPtr $ \onLevel ->
        bracket (GDE.wrapGDExtensionInitializationFunction (initialization onLevel)) freeHaskellFunPtr $ \initFn ->
          GDE.withGodotArgs ([progName, "--path", dir] ++ args) $ \argc argv ->
            bracket (create argc argv initFn) GDE.destroyGodotInstance $ \instPtr -> do
              let inst = unsafeFromObjectPtr @GodotInstance instPtr
              started <- GodotInstance.start inst
              unless started $ fail "Godot.Api.Embed.withGodot: could not start Godot"
              setCurrentDirectory cwd
              body inst
  where
    initialization onLevel gpa library rInit = do
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
    create argc argv initFn = do
      writeIORef alreadyRun True
      instPtr <- GDE.createGodotInstance argc argv initFn
      when (instPtr == GDE.GDExtensionObjectPtr nullPtr) $
        fail "Godot.Api.Embed.withGodot: could not create the Godot instance"
      pure instPtr

-- | Godot cannot be restarted: "Godot.Api" caches pointers into the engine.
alreadyRun :: IORef Bool
alreadyRun = unsafePerformIO (newIORef False)
{-# NOINLINE alreadyRun #-}

-- | Create a fresh, empty temporary directory for the project.
freshDirectory :: IO FilePath
freshDirectory = do
  tmp <- getTemporaryDirectory
  progName <- getProgName
  let fresh = do
        n <- getMonotonicTimeNSec
        let dir = tmp </> (progName ++ "-godot-" ++ show n)
        r <- try (createDirectory dir)
        case r of
          Right () -> pure dir
          Left e
            | isAlreadyExistsError e -> fresh
            | otherwise -> throwIO e
  fresh

-- | Write the project into a directory.
writeProject :: FilePath -> Project -> IO ()
writeProject dir project = do
  writeFile (dir </> "project.godot") . unlines $
    ["config_version=5", "", "[application]", "run/main_scene=\"res://main.tscn\""]
      ++ projectSettings project
  writeFile (dir </> "main.tscn") $
    unlines ["[gd_scene format=3]", "", "[node name=\"Main\" type=\"Node\"]"]
  -- Godot reports an error if an exported project has no script class cache.
  createDirectory (dir </> ".godot")
  writeFile (dir </> ".godot" </> "global_script_class_cache.cfg") "list=[]\n"
  mapM_
    ( \(path, contents) -> do
        createDirectoryIfMissing True (takeDirectory (dir </> path))
        writeFile (dir </> path) contents
    )
    (projectFiles project)
