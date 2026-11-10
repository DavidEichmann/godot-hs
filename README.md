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
cabal build                      # static libgodot.a, template_debug
cabal build -f +shared           # shared libgodot.so
cabal build -f +release          # template_release
cabal test                       # create and destroy a headless Godot instance
```

Depending on `godot-sys` gives your package:

- `-lgodot` (plus `stdc++`, `m`, `pthread`, `dl`, `rt`, `atomic` when static),
- `libgodot.h`, `core/extension/gdextension_interface.gen.h` and
  `gdextension_interface.json` on the include path,
- the `Godot.Sys` module with `libgodotCreateGodotInstance` and
  `libgodotDestroyGodotInstance`.

### Build-time environment variables

| Variable                 | Effect                                                        |
| ------------------------ | ------------------------------------------------------------- |
| `GODOT_SYS_SOURCE_DIR`   | Build from an existing Godot source tree instead of the tarball (must be the pinned version). SCons builds incrementally in that tree; switching between static and shared triggers a full rebuild. |
| `GODOT_SYS_TARBALL`      | Use a local copy of the pinned tarball (hash still verified).  |
| `GODOT_SYS_JOBS`         | Parallel SCons jobs (default: number of CPUs).                 |
| `GODOT_SYS_SCONS_FLAGS`  | Extra SCons arguments, e.g. `"module_mono_enabled=no lto=none"`. |

## Updating Godot

Change `godotTag` and `godotSha256` in `Setup.hs` (the hash is published next
to the tarball as `godot-<tag>.tar.xz.sha256`), and `godotVersion` in
`src/Godot/Sys.hs`.

## How it works

`Setup.hs` (build-type `Custom`):

1. **configure** adds `godot` to `extra-libraries`, and the build and install
   directories to `extra-lib-dirs` / `include-dirs` (and rpath for shared).
2. **build** downloads and verifies the tarball, runs
   `scons platform=linuxbsd target=template_debug library_type=static_library ccflags=-fPIC disable_path_overrides=no`
   (or `shared_library` / `template_release`), then stages the library and
   headers in the component build directory. `disable_path_overrides=no`
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
