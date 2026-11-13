-- | ghcup-gui: GHCup's TUI as a desktop application.
module Main (main) where

import Control.Monad (when)
import Data.IORef
import GHCup.GUI.App (app, newLogBuffer)
import Godot.ImUI
import System.Environment (getArgs, getExecutablePath)
import System.Posix.Process (executeFile)

main :: IO ()
main = do
  logBuf <- newLogBuffer
  restart <- newIORef False
  runUI defaultWindow {windowTitle = "GHCup", windowSize = (1100, 680)} (app logBuf (writeIORef restart True))
  -- Restarting: once the window is closed and Godot has shut down (Godot
  -- cannot be started twice in a process), run this program again.
  again <- readIORef restart
  when again $ do
    exe <- getExecutablePath
    args <- getArgs
    executeFile exe False args Nothing
