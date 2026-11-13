#!/usr/bin/env bash
# Regenerate godot-sys/extension_api.json for the Godot version pinned in
# godot-sys/Setup.hs.
#
# Godot only produces extension_api.json from an *editor* build
# (--dump-extension-api), while godot-sys builds the export-template engine as a
# library. So this script builds the editor from the same pinned source
# (godotUrl / godotSha256 in Setup.hs) and dumps the API, with documentation.
# Run it whenever the pinned version changes and commit the result.
#
# By default it uses the source tree in godot-sys's cache (see "The build
# cache" in README.md), unpacking the pinned tarball there first if needed,
# and holds the cache's lock while it builds, as godot-sys's builds do. The
# editor build is incremental in that tree, and does not disturb godot-sys's
# builds of other targets in it.
#
# Environment:
#   GODOT_SOURCE_DIR     use this unpacked Godot source tree (must be the
#                        pinned version) instead.
#   GODOT_SYS_CACHE_DIR  godot-sys's cache (default: ~/.cache/godot-sys).
#   GODOT_JOBS           parallel SCons jobs (default: nproc).
#
# Requires: scons and Godot's Linux build dependencies, python3, curl, tar,
# sha256sum.
set -euo pipefail

pkg_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
setting() { sed -n "s/^$1 = \"\(.*\)\"$/\1/p" "$pkg_dir/Setup.hs"; }
tag="$(setting godotTag)"
sha256="$(setting godotSha256)"
if [[ -z "$tag" || -z "$sha256" ]]; then
  echo "could not read godotTag / godotSha256 from $pkg_dir/Setup.hs" >&2
  exit 1
fi

cache="${GODOT_SYS_CACHE_DIR:-${XDG_CACHE_HOME:-$HOME/.cache}/godot-sys}"
if [[ -z "${GODOT_SOURCE_DIR:-}" && -z "${GODOT_SYS_CACHE_LOCKED:-}" ]]; then
  # Run the rest with the cache locked. (Setup.hs locks it with GHC's hLock,
  # an fcntl lock, which conflicts with Python's lockf.)
  mkdir -p "$cache"
  export GODOT_SYS_CACHE_LOCKED=1
  exec python3 -c '
import fcntl, subprocess, sys
lock = open(sys.argv[1], "a")
fcntl.lockf(lock, fcntl.LOCK_EX)
sys.exit(subprocess.call(sys.argv[2:]))
' "$cache/lock" "$0" "$@"
fi

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT

if [[ -n "${GODOT_SOURCE_DIR:-}" ]]; then
  src="$(cd "$GODOT_SOURCE_DIR" && pwd)"
else
  # The same layout as Setup.hs's obtainSource / obtainTarball.
  src="$cache/godot-$tag"
  if [[ "$(cat "$src/.godot-sys-unpacked" 2>/dev/null)" != "$sha256" ]]; then
    tarball="$cache/godot-$tag.tar.xz"
    if ! echo "$sha256  $tarball" | sha256sum --check --quiet 2>/dev/null; then
      echo "Downloading Godot $tag source..."
      curl --fail --location --silent --show-error -o "$tarball.download" \
        "https://github.com/godotengine/godot-builds/releases/download/$tag/godot-$tag.tar.xz"
      mv "$tarball.download" "$tarball"
      echo "$sha256  $tarball" | sha256sum --check --quiet
    fi
    rm -rf "$src"
    mkdir -p "$src"
    tar -xJf "$tarball" -C "$src" --strip-components=1
    printf '%s' "$sha256" > "$src/.godot-sys-unpacked"
  fi
fi

echo "Building the Godot $tag editor in $src ..."
(cd "$src" && scons platform=linuxbsd target=editor "-j${GODOT_JOBS:-$(nproc)}")

editor="$(ls "$src"/bin/godot.linuxbsd.editor.* | head -n 1)"
mkdir "$work/dump"
(cd "$work/dump" && "$editor" --headless --dump-extension-api-with-docs >/dev/null 2>&1) || true
if [[ ! -s "$work/dump/extension_api.json" ]]; then
  echo "Godot did not produce extension_api.json" >&2
  exit 1
fi

cp "$work/dump/extension_api.json" "$pkg_dir/extension_api.json"
echo "Wrote $pkg_dir/extension_api.json ($(du -h "$pkg_dir/extension_api.json" | cut -f1)) for Godot $tag"
