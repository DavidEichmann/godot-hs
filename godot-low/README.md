# godot-low

Low-level Haskell bindings to Godot's
[GDExtension](https://docs.godotengine.org/en/stable/tutorials/scripting/gdextension/what_is_gdextension.html)
interface, generated at build time from the `gdextension_interface.json`
installed by [godot-sys](../godot-sys). The bindings therefore always match
the Godot version godot-sys pins.

## Modules

- **`Godot.Low.Types`** (generated): the GDExtension types.
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
- **`Godot.Low.Interface`** (generated): a `GDExtensionInterface` record with
  one field per interface function, named as in C (`get_godot_version2`,
  `variant_new_copy`, …) and documented from the JSON (arguments, return
  value, `@since`, deprecations). `loadInterface` fills it in from Godot's
  `get_proc_address`.
- **`Godot.Low`**: re-exports both, plus `createGodotInstance` /
  `destroyGodotInstance` (libgodot, typed with the generated types) and
  `withGodotArgs`.

## Example

```haskell
initFn <- wrapGDExtensionInitializationFunction $ \gpa _library rInit -> do
  iface <- loadInterface gpa
  version <- alloca $ \pv -> get_godot_version2 iface pv >> peek pv
  peekCString (gdExtensionGodotVersion2_string version) >>= putStrLn
  poke rInit GDExtensionInitialization
    { gdExtensionInitialization_minimum_initialization_level = GDEXTENSION_INITIALIZATION_CORE
    , gdExtensionInitialization_userdata = nullPtr
    , gdExtensionInitialization_initialize = onLevel
    , gdExtensionInitialization_deinitialize = onLevel
    }
  pure 1

withGodotArgs ["app", "--headless", "--path", projectDir] $ \argc argv -> do
  inst <- createGodotInstance argc argv initFn
  ...
  destroyGodotInstance inst
```

See [test/Main.hs](test/Main.hs) for a complete program.

## How generation works

`Setup.hs` (build-type `Custom`) runs before `build`, `haddock` and `repl`:

1. Finds `gdextension_interface.json` in the include directories of the
   godot-sys dependency (from the package database).
2. Parses it with aeson and writes `Godot/Low/Types.hsc` and
   `Godot/Low/Interface.hs` into the package's autogen directory (only when
   the contents change, so rebuilds are incremental).
3. Cabal then runs hsc2hs on `Types.hsc` (with godot-sys's include path, so
   `#include <libgodot.h>` resolves) and compiles everything as usual.

To read the generated code, look under
`dist-newstyle/build/<arch>/<ghc>/godot-low-<version>/build/global-autogen/Godot/Low/`.
