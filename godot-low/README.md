# godot-low

Low-level Haskell bindings to Godot, generated at build time from the two
JSON descriptions installed by [godot-sys](../godot-sys), so they always match
the Godot version godot-sys pins:

- `gdextension_interface.json`: the GDExtension C interface
  → **`Godot.GDExtension.*`**
- `extension_api.json`: the engine API (classes like `Node` and `Node3D`,
  builtin types like `Vector3` and `String`, global enums, utility
  functions) → **`Godot.Api.*`**

## Godot.Api: the engine API

```haskell
import Godot.Api
import qualified Godot.Api.Class.Node as Node
import qualified Godot.Api.Class.Engine as Engine
import qualified Godot.Api.Builtin.Vector3 as Vector3
import qualified Godot.Api.Utility as Utility

-- after Godot.Api.initialize, and once the scene level is initialized:
node <- Node.construct
Node.setName node =<< toStringName "Player"
name <- fromStringName =<< Node.getName node
len  <- Vector3.length (Vector3 1 2 2)               -- 3.0
arch <- fromGString =<< Engine.getArchitectureName =<< Engine.singleton
r    <- Utility.sqrt 9                                -- 3.0
```

| Module | Contents |
|---|---|
| `Godot.Api` | `initialize`, class/variant helpers, string conversions; re-exports the types below |
| `Godot.Api.Classes` | one newtype per engine class (954 non-editor classes), the `Inherits` hierarchy, class enums (`Node_ProcessMode`, patterns `Node_PROCESS_MODE_INHERIT`, …) |
| `Godot.Api.Class.<Name>` | that class's methods (`Node.getChild`, …), constants (`Node.NOTIFICATION_READY`), `construct` / `singleton` where applicable |
| `Godot.Api.Builtin.Types` | builtin types: value types with a known layout are records (`Vector3 {vector3_x, …}`, `Color`, `Transform3D`, …); others are opaque, garbage-collected handles (`GString` for Godot's `String`, `StringName`, `Array`, `Dictionary`, …) |
| `Godot.Api.Builtin.<Name>` | constructors (`new`, `newFromFloatFloatFloat`, …), methods and operators (`opAddVector3`, …) of that builtin |
| `Godot.Api.GlobalEnums` | `Error`, `Key`, `Variant_Type`, … |
| `Godot.Api.Utility` | `sin`, `print`, `absi`, … (import qualified) |
| `Godot.Api.Internal` | the runtime the generated code is built on (method-bind caches, ptrcall marshalling) |

Conventions:

- Names are camelCased from Godot's snake_case (`get_child_count` →
  `getChildCount`); docs come from Godot's class reference.
- Methods take `self` as any subclass: `Node.getName :: Inherits self Node =>
  self -> IO StringName`. `upcast` converts explicitly, `cast` downcasts
  (checked).
- Objects returned by methods are `Maybe` (Godot may return null).
- `int`/`float` are `Int64`/`Double`; enums are `Int64` newtypes.
- Vararg methods (`Object.call`, `emit_signal`, `print`, …) take all
  arguments as `[Variant]`; use `toVariant` / `fromVariant`.
- Method binds are looked up on first call and cached. Call `initialize`
  first, and only use a class after Godot has registered it (scene classes
  after the scene initialization level / `GodotInstance.start`).
- Not (yet) covered: default arguments (all arguments are required),
  properties and signals as first-class values (use their getter/setter
  methods and `connect`/`emit_signal`), subclassing engine classes from
  Haskell (virtual methods), and reference counting of `RefCounted`
  objects (returned objects carry a reference you must release).

## Godot.GDExtension: the C interface

- **`Godot.GDExtension.Types`** (generated): the GDExtension types.
  - Aliases: `type GDExtensionInt = Int64`, `GDExtensionBool = Word8`, …
  - Handles (`GDExtensionVariantPtr`, `GDExtensionConstStringPtr`, …):
    newtypes over `Ptr ()`. Const / uninitialized variants are separate
    newtypes; convert with `coerce`.
  - Enums: newtypes over the C enum's integer type, with pattern synonyms
    using the C names (`GDEXTENSION_VARIANT_TYPE_NIL`, …). Bitfields derive
    `Bits`.
  - Structs: records with fields named `<lowerCamelStruct>_<c_field>`
    (e.g. `gdExtensionCallError_argument`) and `Storable` instances whose
    sizes and offsets come from the C header via hsc2hs.
  - Function pointer types: `FunPtr` synonyms, plus `wrapX` (Haskell function
    → C function pointer) and `unwrapX` (call a C function pointer).
- **`Godot.GDExtension.Interface`** (generated): a `GDExtensionInterface`
  record with one field per interface function, named as in C
  (`get_godot_version2`, `variant_new_copy`, …) and documented from the JSON.
  `loadInterface` fills it in from Godot's `get_proc_address`.
- **`Godot.GDExtension`**: re-exports both, plus `createGodotInstance` /
  `destroyGodotInstance` (libgodot) and `withGodotArgs`.

## Embedding Godot

```haskell
initFn <- wrapGDExtensionInitializationFunction $ \gpa library rInit -> do
  _ <- Godot.Api.initialize gpa library
  poke rInit GDExtensionInitialization { ... }
  pure 1

withGodotArgs ["app", "--headless", "--path", projectDir] $ \argc argv -> do
  instPtr <- createGodotInstance argc argv initFn
  let inst = unsafeFromObjectPtr @GodotInstance instPtr
  _ <- GodotInstance.start inst          -- engine classes now available
  ...                                    -- GodotInstance.iteration inst per frame
  destroyGodotInstance instPtr
```

See [test/Main.hs](test/Main.hs) for a complete program.

## How generation works

`Setup.hs` (build-type `Custom`):

1. **configure**: reads `extension_api.json` from godot-sys's include
   directories (via the package database) and adds the generated
   `Godot.Api.*` modules (one per class and builtin) to the library.
2. **build / haddock / repl**: generates `Godot/GDExtension/Types.hsc`,
   `Godot/GDExtension/Interface.hs` and the `Godot/Api/**` modules into the
   library's autogen directory, rewriting files only when their contents
   change. Cabal runs hsc2hs on `Types.hsc` (so struct layouts come from the
   C header) and compiles everything.

The first build compiles ~1000 generated modules; `cabal.project` enables
`ghc -j` for this package. To read the generated code, look under
`dist-newstyle/build/<arch>/<ghc>/godot-low-<version>/build/autogen/`.
