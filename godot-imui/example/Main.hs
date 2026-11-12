{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

-- | A tour of godot-imui: cabal run godot-imui-example
module Main (main) where

import Control.Monad (forM, when)
import Godot.ImUI
import Paths_godot_imui (getDataFileName)

data State = State
  { tab :: Int,
    clicks :: Int,
    name :: String,
    volume :: Double,
    copies :: Int,
    size :: Int,
    subscribe :: Bool,
    todos :: [Todo],
    newTodo :: String,
    nextId :: Int,
    -- | the path to the selected item of the tree
    selected :: Maybe [Int],
    picture :: FilePath,
    choosingPicture :: Bool,
    notes :: String,
    showFooter :: Bool,
    aboutOpen :: Bool
  }

data Todo = Todo {todoId :: Int, todoDone :: Bool, todoText :: String}

main :: IO ()
main = do
  logo <- getDataFileName "example/lambda.png"
  runUI
    defaultWindow {windowTitle = "godot-imui example", windowSize = (560, 560)}
    State
      { tab = 0,
        clicks = 0,
        name = "",
        volume = 40,
        copies = 1,
        size = 1,
        subscribe = False,
        todos = [Todo 0 True "Write a UI library", Todo 1 False "Write a UI with it"],
        newTodo = "",
        nextId = 2,
        selected = Just [0, 1],
        picture = logo,
        choosingPicture = False,
        notes = "Edit me.\n",
        showFooter = True,
        aboutOpen = False
      }
    ui

-- | The whole UI. Runs every frame with the current state, and returns the
-- state for the next frame.
ui :: State -> UI State
ui s0 = do
  s <- menus s0
  tab' <- tabBar ["Widgets", "To do", "Browse", "Notes"] (tab s)
  -- Each tab's content is keyed, so that the widgets of one tab are not
  -- reused for another, and expands, to keep the footer at the bottom.
  s' <- case tab' of
    0 -> vboxWith vboxOpts {key = "widgets", expand = True} (widgetsTab s)
    1 -> vboxWith vboxOpts {key = "todo", expand = True} (todoTab s)
    2 -> vboxWith vboxOpts {key = "browse", expand = True} (browseTab s)
    _ -> vboxWith vboxOpts {key = "notes", expand = True} (notesTab s)
  when (showFooter s') $ do
    separator
    hbox $ do
      label "Rendered by Godot, written in Haskell."
      spacer
      close <- button "Quit"
      when close quit
  dialogs s' {tab = tab'}

-- | The menu bar. Choosing an item just changes the state.
menus :: State -> UI State
menus s = menuBar $ do
  choose <- menu "File" $ do
    choose <- menuItem "Choose picture..."
    menuSeparator
    close <- menuItem "Quit"
    when close quit
    pure choose
  (footer', tab') <- menu "View" $ do
    footer' <- menuCheck "Show footer" (showFooter s)
    menuSeparator
    -- A submenu, with an item per tab.
    tab' <- menu "Go to" $ do
      chosen <- forM (zip [0 ..] ["Widgets", "To do", "Browse", "Notes"]) $ \(i, title) -> do
        go <- menuItem title
        pure [i | go]
      pure (case concat chosen of i : _ -> i; [] -> tab s)
    pure (footer', tab')
  about <- menu "Help" (menuItem "About")
  pure
    s
      { choosingPicture = choosingPicture s || choose,
        showFooter = footer',
        tab = tab',
        aboutOpen = aboutOpen s || about
      }

-- | Dialogs are open for as long as they are created.
dialogs :: State -> UI State
dialogs s = do
  aboutOpen' <-
    if not (aboutOpen s)
      then pure False
      else do
        ok <- dialog "About" $ do
          hbox $ do
            imageWith imageOpts {minSize = (64, 64)} (picture s)
            vbox $ do
              heading "godot-imui"
              richText "A [b]simple[/b] immediate mode UI,\nrendered by [color=#6ab0f3]Godot[/color]."
          hbox (spacer >> button "OK")
        -- Open until OK is clicked or the window is closed.
        pure (ok == Just False)
  let s' = s {aboutOpen = aboutOpen'}
  if not (choosingPicture s)
    then pure s'
    else do
      choice <- openFileDialog "Choose a picture" ["*.png", "*.jpg", "*.jpeg", "*.svg", "*.webp"]
      pure $ case choice of
        Choosing -> s'
        Cancelled -> s' {choosingPicture = False}
        Chose path -> s' {choosingPicture = False, picture = path, tab = 2}

widgetsTab :: State -> UI State
widgetsTab s = do
  clicked <- hbox $ do
    clicked <- button "Click me"
    label ("Clicked " ++ show (clicks s) ++ " times")
    pure clicked
  s' <- panel $ grid 2 $ do
    label "Name"
    name' <- textInputWith textInputOpts {placeholder = "Your name"} (name s)
    label "Volume"
    volume' <- slider (0, 100) (volume s)
    label "Copies"
    copies' <- spinBox (1, 10) (copies s)
    label "Size"
    size' <- hbox (radioButtons ["Small", "Medium", "Large"] (size s))
    pure s {name = name', volume = volume', copies = copies', size = size'}
  subscribe' <- toggle "Subscribe to the newsletter" (subscribe s)
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
    text' <- textInputWith textInputOpts {placeholder = "What needs doing?"} (newTodo s)
    add <- buttonWith buttonOpts {enabled = not (null text')} "Add"
    pure (text', add)
  -- One row per item, keyed so that each row's widgets stay with its item
  -- when an earlier one is removed.
  todos' <- scroll $ fmap concat $ forM (todos s) $ \todo ->
    hboxWith hboxOpts {key = show (todoId todo)} $ do
      done <- checkbox (todoText todo) (todoDone todo)
      spacer
      remove <- button "Remove"
      pure [todo {todoDone = done} | not remove]
  label (show (length (filter todoDone todos')) ++ " of " ++ show (length todos') ++ " done")
  pure $
    if add
      then s {todos = todos' ++ [Todo (nextId s) False text'], nextId = nextId s + 1, newTodo = ""}
      else s {todos = todos', newTodo = text'}

-- | A tree on the left, and something about its selected item on the right.
browseTab :: State -> UI State
browseTab s = do
  (selected', choose) <-
    hsplit
      (treeWith treeOpts {expand = True} library (selected s))
      ( do
          heading (maybe "Nothing selected" (titleAt library) (selected s))
          imageWith imageOpts {expand = True} (picture s)
          hbox (spacer >> button "Choose picture...")
      )
  pure s {selected = selected', choosingPicture = choosingPicture s || choose}

library :: Forest String
library =
  [ Node "Languages" [Node "Haskell" [], Node "GDScript" [], Node "C++" []],
    Node "Engines" [Node "Godot" [Node "2D" [], Node "3D" [], Node "UI" []]],
    Node "Libraries" [Node "godot-sys" [], Node "godot-low" [], Node "godot-imui" []]
  ]

-- | The label of the item at a path.
titleAt :: Forest String -> [Int] -> String
titleAt forest path = case path of
  [] -> ""
  [i] -> rootLabel (forest !! i)
  i : rest -> titleAt (subForest (forest !! i)) rest

notesTab :: State -> UI State
notesTab s = do
  notes' <- textAreaWith textAreaOpts {expand = True} (notes s)
  label (show (length (words notes')) ++ " words")
  pure s {notes = notes'}
