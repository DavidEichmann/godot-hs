-- | A tour of godot-imui: cabal run godot-imui-example
module Main (main) where

import Control.Monad (forM, when)
import Godot.ImUI

data State = State
  { tab :: Int
  , clicks :: Int
  , name :: String
  , volume :: Double
  , copies :: Int
  , size :: Int
  , subscribe :: Bool
  , todos :: [Todo]
  , newTodo :: String
  , nextId :: Int
  , notes :: String
  }

data Todo = Todo {todoId :: Int, todoDone :: Bool, todoText :: String}

main :: IO ()
main =
  runUI defaultWindow {windowTitle = "godot-imui example", windowSize = (520, 520)} initial ui
  where
    initial =
      State
        { tab = 0
        , clicks = 0
        , name = ""
        , volume = 40
        , copies = 1
        , size = 1
        , subscribe = False
        , todos = [Todo 0 True "Write a UI library", Todo 1 False "Write a UI with it"]
        , newTodo = ""
        , nextId = 2
        , notes = "Edit me.\n"
        }

-- | The whole UI. Runs every frame with the current state, and returns the
-- state for the next frame.
ui :: State -> UI State
ui s = do
  heading "godot-imui"
  tab' <- tabBar ["Widgets", "To do", "Notes"] (tab s)
  -- Each tab's content is keyed, so that the widgets of one tab are not
  -- reused for another, and expands, to keep the footer at the bottom.
  s' <- case tab' of
    0 -> keyed "widgets" (expand (vbox (widgetsTab s)))
    1 -> keyed "todo" (expand (vbox (todoTab s)))
    _ -> keyed "notes" (expand (vbox (notesTab s)))
  separator
  hbox $ do
    label "Rendered by Godot, written in Haskell."
    spacer
    close <- button "Quit"
    when close quit
  pure s' {tab = tab'}

widgetsTab :: State -> UI State
widgetsTab s = do
  clicked <- hbox $ do
    clicked <- button "Click me"
    label ("Clicked " ++ show (clicks s) ++ " times")
    pure clicked
  s' <- panel $ grid 2 $ do
    label "Name"
    name' <- placeholder "Your name" (textInput (name s))
    label "Volume"
    volume' <- slider (0, 100) (volume s)
    label "Copies"
    copies' <- spinBox (1, 10) (copies s)
    label "Size"
    size' <- dropdown ["Small", "Medium", "Large"] (size s)
    pure s {name = name', volume = volume', copies = copies', size = size'}
  subscribe' <- checkbox "Subscribe to the newsletter" (subscribe s)
  progressBar (volume s' / 100)
  paragraph $
    "Hello, "
      ++ (if null (name s') then "stranger" else name s')
      ++ "! You want "
      ++ show (copies s')
      ++ (if copies s' == 1 then " copy" else " copies")
      ++ " in "
      ++ ["small", "medium", "large"] !! size s'
      ++ (if subscribe' then ", and the newsletter." else ".")
  pure s' {clicks = if clicked then clicks s + 1 else clicks s, subscribe = subscribe'}

todoTab :: State -> UI State
todoTab s = do
  (text', add) <- hbox $ do
    text' <- placeholder "What needs doing?" (textInput (newTodo s))
    add <- enabled (not (null text')) (button "Add")
    pure (text', add)
  -- One row per item, keyed so that each row's widgets stay with its item
  -- when an earlier one is removed.
  todos' <- scroll $ fmap concat $ forM (todos s) $ \todo ->
    keyed (show (todoId todo)) $ hbox $ do
      done <- checkbox (todoText todo) (todoDone todo)
      spacer
      remove <- button "Remove"
      pure [todo {todoDone = done} | not remove]
  label (show (length (filter todoDone todos')) ++ " of " ++ show (length todos') ++ " done")
  pure $
    if add
      then s {todos = todos' ++ [Todo (nextId s) False text'], nextId = nextId s + 1, newTodo = ""}
      else s {todos = todos', newTodo = text'}

notesTab :: State -> UI State
notesTab s = do
  notes' <- expand (textArea (notes s))
  label (show (length (words notes')) ++ " words")
  pure s {notes = notes'}
