# godot-low-example

A minimal "hello world" UI built with [godot-low](../godot-low): it embeds
Godot (via [godot-sys](../godot-sys)'s libgodot), opens a window and adds a
centred `Label` saying "Hello, World!" to the scene tree, all from Haskell.

```sh
cabal run godot-low-example
```

Close the window to exit. See [app/Main.hs](app/Main.hs).

The program writes a throwaway Godot project (just `project.godot` and an
empty main scene) to the temporary directory, because libgodot is an export
template and needs a project to start. The UI itself is created at runtime
through `Godot.Api`.
