{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

-- | A tour of godot-imui: cabal run godot-imui-example
module Main (main) where

import Control.Monad (forM, forM_, when)
import Godot.ImUI
import Paths_godot_imui (getDataFileName)

data Todo = Todo {todoId :: Int, todoDone :: Bool, todoText :: String}

main :: IO ()
main = do
  logo <- getDataFileName "example/lambda.png"
  runUI
    defaultWindow {windowTitle = "godot-imui example", windowSize = (560, 560)}
    (app logo)

-- | A piece of state and its setter, as 'state' returns them.
type State a = (a, a -> IO ())

-- | Show a widget for a piece of state, and set the state to the user's
-- edits. Returns the value after the edits.
edit :: (Eq a) => State a -> (a -> UI a) -> UI a
edit (value, set) widget = do
  value' <- widget value
  -- Only when it changed, so as not to undo a change set elsewhere in this
  -- frame (a menu item choosing a tab, say).
  when (value' /= value) (liftIO (set value'))
  pure value'

-- | The whole UI, run every frame. The state that more than one part of
-- the UI uses is kept here; the tabs keep their own.
app :: FilePath -> UI ()
app logo = do
  tab <- state 0
  picture <- state logo
  footer <- state True
  about <- state False
  choosing <- state False
  -- A tab's state is lost when another tab is shown (its widgets are no
  -- longer created), so the to-do list and the notes are kept here.
  todos <- state [Todo 0 True "Write a UI library", Todo 1 False "Write a UI with it"]
  notes <- state "Edit me.\n"

  menus (snd tab) footer (snd about) (snd choosing)
  tab' <- edit tab (tabBar tabs)
  -- Each tab's content is keyed, so that the widgets of one tab are not
  -- reused for another, and expands, to keep the footer at the bottom.
  case tab' of
    0 -> vboxWith vboxOpts {key = "widgets", expand = True} widgetsTab
    1 -> vboxWith vboxOpts {key = "todo", expand = True} (todoTab todos)
    2 -> vboxWith vboxOpts {key = "browse", expand = True} (browseTab (fst picture) (snd choosing))
    _ -> vboxWith vboxOpts {key = "notes", expand = True} (notesTab notes)
  when (fst footer) $ do
    separator
    hbox $ do
      label "Rendered by Godot, written in Haskell."
      spacer
      close <- button "Quit"
      when close quit
  aboutDialog about (fst picture)
  pictureChooser choosing (snd picture) (snd tab)

tabs :: [String]
tabs = ["Widgets", "To do", "Browse", "Notes"]

-- | The menu bar. Choosing an item just sets some state.
menus :: (Int -> IO ()) -> State Bool -> (Bool -> IO ()) -> (Bool -> IO ()) -> UI ()
menus setTab footer setAbout setChoosing = menuBar $ do
  menu "File" $ do
    choose <- menuItem "Choose picture..."
    when choose (liftIO (setChoosing True))
    menuSeparator
    close <- menuItem "Quit"
    when close quit
  menu "View" $ do
    _ <- edit footer (menuCheck "Show footer")
    menuSeparator
    -- A submenu, with an item per tab.
    menu "Go to" $
      forM_ (zip [0 ..] tabs) $ \(i, title) -> do
        go <- menuItem title
        when go (liftIO (setTab i))
  menu "Help" $ do
    chosen <- menuItem "About"
    when chosen (liftIO (setAbout True))

-- | Dialogs are open for as long as they are created.
aboutDialog :: State Bool -> FilePath -> UI ()
aboutDialog (open, setOpen) picture =
  when open $ do
    ok <- dialog "About" $ do
      hbox $ do
        imageWith imageOpts {minSize = (64, 64)} picture
        vbox $ do
          heading "godot-imui"
          richText "A [b]simple[/b] immediate mode UI,\nrendered by [color=#6ab0f3]Godot[/color]."
      hbox (spacer >> button "OK")
    -- Open until OK is clicked or the window is closed.
    when (ok /= Just False) (liftIO (setOpen False))

pictureChooser :: State Bool -> (FilePath -> IO ()) -> (Int -> IO ()) -> UI ()
pictureChooser (open, setOpen) setPicture setTab =
  when open $ do
    choice <- openFileDialog "Choose a picture" ["*.png", "*.jpg", "*.jpeg", "*.svg", "*.webp"]
    liftIO $ case choice of
      Choosing -> pure ()
      Cancelled -> setOpen False
      Chose path -> setOpen False >> setPicture path >> setTab 2

-- | Its state starts again whenever the tab is shown.
widgetsTab :: UI ()
widgetsTab = do
  (clicks, setClicks) <- state (0 :: Int)
  name <- state ""
  volume <- state 40
  copies <- state 1
  size <- state 1
  subscribe <- state False
  hbox $ do
    clicked <- button "Click me"
    when clicked (liftIO (setClicks (clicks + 1)))
    label ("Clicked " ++ show clicks ++ " times")
  (name', volume', copies', size') <- panel $ grid 2 $ do
    label "Name"
    name' <- edit name (textInputWith textInputOpts {placeholder = "Your name"})
    label "Volume"
    volume' <- edit volume (slider (0, 100))
    label "Copies"
    copies' <- edit copies (spinBox (1, 10))
    label "Size"
    size' <- hbox (edit size (radioButtons ["Small", "Medium", "Large"]))
    pure (name', volume', copies', size')
  subscribe' <- edit subscribe (toggle "Subscribe to the newsletter")
  progressBar (volume' / 100)
  paragraph $
    "Hello, "
      ++ (if null name' then "stranger" else name')
      ++ "! You want "
      ++ show copies'
      ++ (if copies' == 1 then " copy" else " copies")
      ++ " in "
      ++ ["small", "medium", "large"] !! size'
      ++ (if subscribe' then ", and the newsletter." else ".")

todoTab :: State [Todo] -> UI ()
todoTab (todos, setTodos) = do
  (draft, setDraft) <- state ""
  (text', add) <- hbox $ do
    text' <- textInputWith textInputOpts {placeholder = "What needs doing?"} draft
    add <- buttonWith buttonOpts {enabled = not (null text')} "Add"
    pure (text', add)
  -- One row per item, keyed so that each row's widgets stay with its item
  -- when an earlier one is removed.
  todos' <- scroll $ fmap concat $ forM todos $ \todo ->
    hboxWith hboxOpts {key = show (todoId todo)} $ do
      done <- checkbox (todoText todo) (todoDone todo)
      spacer
      remove <- button "Remove"
      pure [todo {todoDone = done} | not remove]
  label (show (length (filter todoDone todos')) ++ " of " ++ show (length todos') ++ " done")
  let nextId = 1 + maximum (-1 : map todoId todos)
  liftIO $
    if add
      then setTodos (todos' ++ [Todo nextId False text']) >> setDraft ""
      else setTodos todos' >> setDraft text'

-- | A tree on the left, and something about its selected item on the right.
browseTab :: FilePath -> (Bool -> IO ()) -> UI ()
browseTab picture setChoosing = do
  selected <- state (Just [0, 1])
  (_, choose) <-
    hsplit
      (edit selected (treeWith treeOpts {expand = True} library))
      ( do
          heading (maybe "Nothing selected" (titleAt library) (fst selected))
          imageWith imageOpts {expand = True} picture
          hbox (spacer >> button "Choose picture...")
      )
  when choose (liftIO (setChoosing True))

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

notesTab :: State String -> UI ()
notesTab notes = do
  notes' <- edit notes (textAreaWith textAreaOpts {expand = True})
  label (show (length (words notes')) ++ " words")
