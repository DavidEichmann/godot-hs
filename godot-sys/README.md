# godot-sys

Builds the [Godot Engine](https://godotengine.org) as a native library
(`libgodot`) and makes it available to Haskell packages.

- **Pinned version:** Godot `4.7.2-stable`, fetched from the official source
  release tarball and checked against its SHA-256 (see `godotTag`,
  `godotUrl`, `godotSha256` in [Setup.hs](Setup.hs)).
- **Static or shared:** static `libgodot.a` by default; `-f +shared` builds
  `libgodot.so` and adds an rpath to it.
- **Platform:** Linux (`platform=linuxbsd`).

## Requirements

Godot's own build dependencies: `scons`, `python3`, a C++17 compiler, and the
usual Linux development headers (see Godot's
[Linux build docs](https://docs.godotengine.org/en/stable/contributing/development/compiling/compiling_for_linuxbsd.html)).
The setup script also uses `curl`, `tar` (with xz), `sha256sum`, `ar` and
`ranlib`.

A full Godot build takes a while (10–30 minutes depending on the machine) and
several GB of disk.

## Usage

```sh
cabal build all                  # static libgodot.a, template_debug
cabal build all -f +shared       # shared libgodot.so
cabal build all -f +release      # template_release
cabal test all                   # create and destroy a headless Godot instance
```

Depending on `godot-sys` gives your package:

- `-lgodot` (plus `stdc++`, `m`, `pthread`, `dl`, `rt`, `atomic` when static),
- `libgodot.h`, `core/extension/gdextension_interface.gen.h`,
  `gdextension_interface.json` and `extension_api.json` (the engine class
  API) on the include path,
- the `Godot.Sys` module with `libgodotCreateGodotInstance` and
  `libgodotDestroyGodotInstance`.

### Build-time environment variables

| Variable                 | Effect                                                        |
| ------------------------ | ------------------------------------------------------------- |
| `GODOT_SYS_CACHE_DIR`    | Where to keep the Godot source and builds (default: `~/.cache/godot-sys`, see [The build cache](#the-build-cache)). |
| `GODOT_SYS_SOURCE_DIR`   | Build from an existing Godot source tree instead of the tarball (must be the pinned version). SCons builds incrementally in that tree; switching between static and shared triggers a full rebuild. Builds from such a tree are not cached. |
| `GODOT_SYS_TARBALL`      | Use a local copy of the pinned tarball (hash still verified).  |
| `GODOT_SYS_JOBS`         | Parallel SCons jobs (default: number of CPUs).                 |
| `GODOT_SYS_SCONS_FLAGS`  | Extra SCons arguments, e.g. `"module_mono_enabled=no lto=none"`. |

## The build cache

Building Godot takes a long time, so the unpacked source tree, SCons's
object files and each finished library (one per target, library kind and
set of SCons flags) are kept in `~/.cache/godot-sys` (or
`$GODOT_SYS_CACHE_DIR`), not in Cabal's build directory. Other build
directories then reuse the library instead of building Godot again: the
Haskell Language Server's (hie-bios builds in its own directory under
`~/.cache/hie-bios`), git worktrees, other projects, and `dist-newstyle`
after a `cabal clean`.

A lock in the cache lets only one build use it at a time, so an editor
starting HLS while `cabal build` runs waits instead of corrupting the tree.
Delete the directory to free its space (about 1.2 GB) or to force a rebuild;
`cabal clean` leaves it alone.

## Updating Godot

1. Change `godotTag` and `godotSha256` in `Setup.hs` (the hash is published
   next to the tarball as `godot-<tag>.tar.xz.sha256`).
2. Run `scripts/update-extension-api.sh` and commit the new
   `extension_api.json`. The build refuses an `extension_api.json` whose
   version doesn't match `godotTag`.

## extension_api.json

The engine API description (classes, builtin types, utility functions, with
documentation) is only produced by an *editor* build of Godot
(`--dump-extension-api`), while this package builds the export-template
engine. So it is vendored: `scripts/update-extension-api.sh` builds the editor
from the same pinned, hash-checked source and dumps the API. Set
`GODOT_SOURCE_DIR` to reuse an existing source tree; the static-library build
only merges archives for its own target, so the editor build can share the
tree.

## How it works

`Setup.hs` (build-type `Custom`):

1. **configure** adds `godot` to `extra-libraries`, and the build and install
   directories to `extra-lib-dirs` / `include-dirs` (and rpath for shared).
2. **build** uses the [cached](#the-build-cache) library if there is one.
   Otherwise it downloads and verifies the tarball, runs
   `scons platform=linuxbsd target=template_debug library_type=static_library ccflags=-fPIC disable_path_overrides=no`
   (or `shared_library` / `template_release`) and saves the library and
   headers in the cache. Either way it then copies them into the component
   build directory. `disable_path_overrides=no`
   keeps `--path` / `--main-pack` working so the host can point Godot at its
   project (later `GODOT_SYS_SCONS_FLAGS` values override it). For static builds, the
   per-subsystem archives SCons produces are merged into one `libgodot.a`.
3. **copy/install** copies the library and headers next to the installed
   Haskell library.

## Caveats

- Linux only for now (`platform=linuxbsd`); the configure/link logic assumes
  ELF and GNU `ar`.
- Cabal does not track `libgodot.{a,so}` as a link input, so after changing
  SCons flags in an existing build directory, dependents may need a forced
  relink (e.g. delete their executable, or `cabal build --ghc-options=-fforce-recomp`).
- A static build also embeds Godot in the Haskell library's own `.so`
  (used by GHCi / Template Haskell), so expect a large `libHSgodot-sys-*.so`.
