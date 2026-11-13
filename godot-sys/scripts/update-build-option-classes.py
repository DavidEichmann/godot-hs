#!/usr/bin/env python3
"""Regenerate godot-sys/build_option_classes.json for the Godot version pinned
in godot-sys/Setup.hs.

Godot's disable_* build options (disable_3d, disable_physics_2d, ...) remove
classes from the engine, but extension_api.json describes a full Godot. This
records which classes each option removes, so that godot-low can leave them
out of its bindings. It reads the pinned Godot source:

- SConstruct: the preprocessor macro each option defines (disable_3d defines
  _3D_DISABLED, ...) and the options it implies (disable_3d also sets
  disable_physics_3d, ...);
- every class registration (GDREGISTER_CLASS(...), ClassDB::register_class<>)
  outside the editor, with the #if conditions on those macros around it;
- each module's config.py, for the modules that are not built at all with an
  option (gltf without 3D, ...).

The result maps each class an option can remove to the sets of options that
remove it: it is removed when all the options of one of its sets are on
(after adding the implied ones). Run this whenever the pinned version changes
and commit the result.

Environment:
  GODOT_SOURCE_DIR  use this unpacked Godot source tree (must be the pinned
                    version) instead of downloading the pinned tarball.
"""

import itertools
import json
import os
import re
import subprocess
import sys
import tempfile

PKG_DIR = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))


def setting(name):
    with open(os.path.join(PKG_DIR, "Setup.hs")) as f:
        m = re.search(rf'^{name} = "(.*)"$', f.read(), re.M)
    if not m:
        sys.exit(f"could not read {name} from {PKG_DIR}/Setup.hs")
    return m.group(1)


def obtain_source(tag, sha256, work):
    if os.environ.get("GODOT_SOURCE_DIR"):
        return os.path.abspath(os.environ["GODOT_SOURCE_DIR"])
    tarball = os.path.join(work, f"godot-{tag}.tar.xz")
    print(f"Downloading Godot {tag} source...", file=sys.stderr)
    url = f"https://github.com/godotengine/godot-builds/releases/download/{tag}/godot-{tag}.tar.xz"
    subprocess.run(["curl", "--fail", "--location", "--silent", "--show-error", "-o", tarball, url], check=True)
    subprocess.run(["sha256sum", "--check", "--quiet"], input=f"{sha256}  {tarball}\n", text=True, check=True)
    src = os.path.join(work, f"godot-{tag}")
    os.mkdir(src)
    subprocess.run(["tar", "-xJf", tarball, "-C", src, "--strip-components=1"], check=True)
    return src


def option_macros(src):
    """{option: macro} and {option: [implied options]} from SConstruct's
    `if env["disable_x"]:` blocks."""
    with open(os.path.join(src, "SConstruct")) as f:
        lines = f.read().split("\n")
    macros, implies = {}, {}
    for i, line in enumerate(lines):
        m = re.match(r'if env\["(disable_\w+)"\]:\s*$', line)
        if not m:
            continue
        option = m.group(1)
        for body in itertools.takewhile(lambda l: l.startswith((" ", "\t")), lines[i + 1 :]):
            d = re.search(r'CPPDEFINES=\["(\w+_DISABLED)"\]', body)
            if d:
                macros[option] = d.group(1)
            s = re.search(r'env\["(disable_\w+)"\] = True', body)
            if s:
                implies.setdefault(option, []).append(s.group(1))
    return macros, implies


REGISTRATION = re.compile(
    r"(?:GDREGISTER_(?:ABSTRACT_|VIRTUAL_|INTERNAL_|RUNTIME_)?CLASS\s*\(|"
    r"ClassDB::register_(?:abstract_|virtual_|internal_|runtime_)?class\s*<)\s*([A-Za-z_][A-Za-z0-9_:]*)"
)
DIRECTIVE = re.compile(r"^\s*#\s*(if|ifdef|ifndef|elif|else|endif)\b(.*)")


def condition(kind, rest):
    """A #if condition as a Python expression over defined('MACRO')."""
    rest = rest.split("//")[0].strip()
    if kind == "ifdef":
        return f"defined('{rest}')"
    if kind == "ifndef":
        return f"not defined('{rest}')"
    expr = rest.replace("&&", " and ").replace("||", " or ").replace("!", " not ")
    return re.sub(r"defined\s*\(?\s*(\w+)\s*\)?", r"defined('\1')", expr)


def registrations(path):
    """(class, [conditions]) for each registration in a file, with the
    conditions on *_DISABLED macros of the #if blocks around it."""
    found = []
    stack = []  # per #if block: the conditions for its current branch
    with open(path, encoding="utf-8", errors="replace") as f:
        for line in f:
            m = DIRECTIVE.match(line)
            if m:
                kind, rest = m.groups()
                if kind in ("if", "ifdef", "ifndef"):
                    stack.append([condition(kind, rest)])
                elif kind == "elif" and stack:
                    stack[-1] = [f"not ({c})" for c in stack[-1]] + [condition("if", rest)]
                elif kind == "else" and stack:
                    stack[-1] = [f"not ({c})" for c in stack[-1][-1:]] + stack[-1][:-1]
                elif kind == "endif" and stack:
                    stack.pop()
                continue
            for name in REGISTRATION.findall(line):
                conds = [c for block in stack for c in block if "_DISABLED" in c]
                found.append((name.split("::")[-1], conds))
    return found


def removing_sets(conds, macro_option):
    """The minimal sets of options that make one of the conditions false."""
    macros = sorted({m for c in conds for m in re.findall(r"defined\('(\w+)'\)", c)})
    sets = []
    for n in range(1, len(macros) + 1):
        for subset in itertools.combinations(macros, n):
            if any(set(s) <= set(subset) for s in sets):
                continue
            defined = lambda m: m in subset  # noqa: E731
            if not all(eval(c, {"defined": defined}) for c in conds):
                sets.append(subset)
    unknown = [m for s in sets for m in s if m not in macro_option]
    if unknown:
        sys.exit(f"no build option defines {unknown}")
    return [sorted(macro_option[m] for m in s) for s in sets]


def main():
    tag, sha256 = setting("godotTag"), setting("godotSha256")
    with tempfile.TemporaryDirectory() as work:
        src = obtain_source(tag, sha256, work)
        macros, implies = option_macros(src)
        macro_option = {m: o for o, m in macros.items()}

        # Every registration of a class must be removed for the class to be.
        regs = {}
        modules = {}
        skip = ("thirdparty", "editor", "tests", "bin", "platform")
        for root, dirs, files in os.walk(src):
            rel = os.path.relpath(root, src)
            parts = rel.split(os.sep)
            if parts[0] in skip or "editor" in parts:
                dirs[:] = []
                continue
            for fn in files:
                if fn.endswith((".cpp", ".h")):
                    for name, conds in registrations(os.path.join(root, fn)):
                        regs.setdefault(name, []).append(conds)
                        if parts[0] == "modules" and len(parts) > 1:
                            modules.setdefault(parts[1], set()).add(name)

        classes = {}
        for name, condss in regs.items():
            per_reg = [removing_sets(conds, macro_option) for conds in condss]
            if all(per_reg):
                # Removed when every registration is: the unions of one set
                # from each, keeping the minimal ones.
                combos = {tuple(sorted(set().union(*c))) for c in itertools.product(*per_reg)}
                classes[name] = sorted(list(c) for c in combos if not any(set(o) < set(c) for o in combos))

        # Modules that are not built with an option: config.py's can_build
        # returns False when an option `env["disable_x"]` it reads is on.
        for module, names in modules.items():
            config = os.path.join(src, "modules", module, "config.py")
            if not os.path.exists(config):
                continue
            with open(config) as f:
                can_build = f.read().split("def configure")[0]
            for option in sorted(set(re.findall(r'env\["(disable_\w+)"\]', can_build))):
                for name in names:
                    sets = classes.setdefault(name, [])
                    if [option] not in sets:
                        sets.append([option])
                        sets.sort()

    result = {
        "godot_version": tag,
        "implies": dict(sorted(implies.items())),
        "classes": dict(sorted(classes.items())),
    }
    out = os.path.join(PKG_DIR, "build_option_classes.json")
    # One class per line, for readable diffs.
    with open(out, "w") as f:
        f.write("{\n")
        f.write(f' "godot_version": {json.dumps(result["godot_version"])},\n')
        f.write(f' "implies": {json.dumps(result["implies"])},\n')
        f.write(' "classes": {\n')
        f.write(",\n".join(f"  {json.dumps(k)}: {json.dumps(v)}" for k, v in result["classes"].items()))
        f.write("\n }\n}\n")
    print(f"Wrote {out} ({len(classes)} classes) for Godot {tag}", file=sys.stderr)


if __name__ == "__main__":
    main()
