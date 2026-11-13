-- | ghcup-gui: GHCup's TUI as a desktop application.
module Main (main) where

import GHCup.GUI.App (app, newLogBuffer)
import Godot.ImUI

main :: IO ()
main = do
  logBuf <- newLogBuffer
  runUI defaultWindow {windowTitle = "GHCup", windowSize = (1000, 680)} (app logBuf)
