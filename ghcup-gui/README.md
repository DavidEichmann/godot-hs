# ghcup-gui

A desktop version of GHCup's terminal UI (`ghcup tui`), built with
[godot-imui](../godot-imui). It is only the user interface: listing,
installing, setting, removing and compiling tools is all done by the
[ghcup](https://hackage.haskell.org/package/ghcup) library, as in the TUI.

```sh
cabal run ghcup-gui
```

It works on your GHCup installation (`~/.ghcup`, or wherever your GHCup
settings put it), and reads your `config.yaml`, like the `ghcup` command. To
try it without touching your installation, point it somewhere else:

```sh
GHCUP_INSTALL_BASE_PREFIX=/tmp/ghcup-try cabal run ghcup-gui
```

## What it does

As in the TUI, the tools are on the left (GHC, cabal, HLS, Stack, GHCup and
any others the metadata has), and the selected tool's versions on the right,
newest first, with the TUI's marks (✔✔ in use, ✓ installed, ✗ not installed),
tags (recommended, latest, base-…) and notes (hls-powered, release day, …).

| `ghcup tui` | ghcup-gui |
| --- | --- |
| `i` on a version | a version's **Install** button (**Upgrade** for GHCup) |
| `s` on a version | **Set** (asks to install it first if needed) |
| `u` on a version | **Uninstall** |
| `c` on a version | **More** ▸ **Changelog** |
| Enter on a version: the advanced menu | **More** ▸ **Install with options…**, **Compile from source…** (GHC, HLS) |
| `i` on a tool | **Install recommended** (installs and sets it) |
| Enter on a tool: its details | **Details** |
| `a`: show all versions | **View** ▸ **Show all versions** |
| `h`: help | **Help** ▸ **Legend** |
| the terminal output of an action | the status bar and the **Log** panel |

The option forms have the TUI's fields and checks (each field is checked
with ghcup's own parsers, and the form's OK button is enabled once it is
valid), and remember what was entered, so that a typo can be fixed.

Only one action runs at a time, in the background, so the window keeps
responding; **Cancel** stops it. After each one the list is reloaded.

Differences from the TUI:

- Its command line options (`--tool`, `--show-criteria`, `--since`, ...)
  are not supported: it shows what `ghcup tui` shows by default.
- Download progress (curl's) goes to the terminal ghcup-gui was started
  from, not to the window; the log shows what is being downloaded.
- After upgrading GHCup, the `ghcup` command is the new version, but
  ghcup-gui itself is not restarted (it is a separate program).
- Custom key bindings in `config.yaml` do not apply.

## How it works

- [Setup](src/GHCup/GUI/Setup.hs) sets up ghcup's `AppState` as the `ghcup`
  executable does: its directories, the user's settings, logging (to
  ghcup's log file, and to the window), the platform and the download
  metadata.
- [Actions](src/GHCup/GUI/Actions.hs) is the TUI's `GHCup.Brick.Actions`
  without the terminal: each action calls the ghcup library and returns an
  error message or success. (The TUI's prompts are left to the window.)
- [Forms](src/GHCup/GUI/Forms.hs) are the TUI's install and compile menus.
- [App](src/GHCup/GUI/App.hs) is the window: actions run with
  `asyncState`.

The TUI itself is a private sub-library of the ghcup package, so it is
ported rather than reused.

## Building

`cabal.project` builds the ghcup library without its TUI and executable,
and with Haskell's `tar` instead of the C libarchive (`-tui +no-exe +tar`);
and the `zip` package (which ghcup uses with `+tar`) without bzip2 entries,
which would need the bzip2 C library's development files (`libbz2-dev`).

## License

LGPL-3.0-only, like ghcup, whose TUI this follows closely.
