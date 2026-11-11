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
# Environment:
#   GODOT_SOURCE_DIR  use this unpacked Godot source tree (must be the pinned
#                     version) instead of downloading the pinned tarball.
#                     The editor build is incremental in that tree.
#   GODOT_JOBS        parallel SCons jobs (default: nproc).
#
# Requires: scons and Godot's Linux build dependencies, curl, tar, sha256sum.
set -euo pipefail

pkg_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
setting() { sed -n "s/^$1 = \"\(.*\)\"$/\1/p" "$pkg_dir/Setup.hs"; }
tag="$(setting godotTag)"
sha256="$(setting godotSha256)"
if [[ -z "$tag" || -z "$sha256" ]]; then
  echo "could not read godotTag / godotSha256 from $pkg_dir/Setup.hs" >&2
  exit 1
fi

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT

if [[ -n "${GODOT_SOURCE_DIR:-}" ]]; then
  src="$(cd "$GODOT_SOURCE_DIR" && pwd)"
else
  tarball="godot-$tag.tar.xz"
  echo "Downloading Godot $tag source..."
  curl --fail --location --silent --show-error -o "$work/$tarball" \
    "https://github.com/godotengine/godot-builds/releases/download/$tag/$tarball"
  echo "$sha256  $work/$tarball" | sha256sum --check --quiet
  src="$work/godot-$tag"
  mkdir "$src"
  tar -xJf "$work/$tarball" -C "$src" --strip-components=1
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
