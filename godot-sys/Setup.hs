-- | Custom setup for godot-sys.
--
-- * configure: works out where libgodot will live and adds it (plus its
--   system dependencies, headers and, for shared builds, an rpath) to the
--   library's build info, so that dependents link against it.
-- * build: fetches the pinned Godot source, builds it with SCons as a static
--   or shared library, and stages @libgodot.{a,so}@ and the C headers in the
--   component build directory. The source, SCons objects and finished
--   library are kept in a per-user cache (see 'cacheDir'), so that other
--   build directories (HLS's, git worktrees, other projects) reuse them
--   instead of building Godot again.
-- * copy/install: copies those artefacts next to the installed Haskell
--   library.
module Main (main) where

import Control.Exception (throwIO)
import Control.Monad (filterM, forM_, unless, when)
import Data.Char (isSpace)
import Data.List (isInfixOf, isPrefixOf, isSuffixOf, sort)
import Data.Maybe (fromMaybe)
import Distribution.Simple.PackageDescription (readGenericPackageDescription)
import Distribution.PackageDescription
  ( BuildInfo (..),
    FlagAssignment,
    GenericPackageDescription (..),
    HookedBuildInfo,
    Library (..),
    PackageDescription (..),
    PackageFlag (..),
    lookupFlagAssignment,
    mkFlagName,
    unFlagName,
  )
import Distribution.Simple
import Distribution.Simple.InstallDirs (CopyDest (..), InstallDirs (..))
import Distribution.Simple.LocalBuildInfo
  ( LocalBuildInfo (..),
    absoluteInstallDirs,
    buildDir,
    interpretSymbolicPathLBI,
  )
import Distribution.Simple.Setup
  ( BuildFlags,
    ConfigFlags,
    CopyFlags,
    InstallFlags,
    RegisterFlags,
    buildVerbosity,
    copyDest,
    copyVerbosity,
    fromFlagOrDefault,
    installVerbosity,
    regInPlace,
  )
import Distribution.Simple.Utils (info, notice)
import Distribution.Utils.Path (getSymbolicPath, makeSymbolicPath)
import Distribution.Verbosity (Verbosity, normal, silent)
import GHC.Conc (getNumProcessors)
import GHC.IO.Handle.Lock (LockMode (..), hLock, hTryLock)
import System.Directory
import System.Environment (getEnvironment, lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath
import System.IO (IOMode (..), withFile)
import System.Process

--------------------------------------------------------------------------------
-- Pinned Godot version

godotTag :: String
godotTag = "4.7.2-stable"

-- | Official source release tarball published by the Godot project.
godotUrl :: String
godotUrl =
  "https://github.com/godotengine/godot-builds/releases/download/"
    ++ godotTag
    ++ "/godot-"
    ++ godotTag
    ++ ".tar.xz"

-- | SHA-256 of 'godotUrl' (matches the upstream @.tar.xz.sha256@ file).
godotSha256 :: String
godotSha256 = "a18ce0ccec3ecc40b0dd6c4f5132ca934e9fb7c2979717940ff32aee1eb35481"

--------------------------------------------------------------------------------
-- Configuration derived from flags

data LibKind = Static | Shared deriving (Eq, Show)

data GodotConfig = GodotConfig
  { gcKind :: LibKind,
    -- | SCons @target=@
    gcTarget :: String,
    -- | SCons arguments for the build options whose flags differ from their
    -- defaults (see 'sconsOptions')
    gcOptions :: [String],
    -- | The modules that have a flag
    gcModules :: [String]
  }

-- | The configuration, from the flags. Their defaults (which 'sconsOptions'
-- compares them with) are read from godot-sys.cabal: the configured package
-- only has the flags' values.
godotConfig :: LocalBuildInfo -> IO GodotConfig
godotConfig lbi = do
  gpd <- readGenericPackageDescription silent Nothing (makeSymbolicPath (interpretSymbolicPathLBI lbi (makeSymbolicPath "godot-sys.cabal")))
  options <- either failWith pure (sconsOptions gpd (flagAssignment lbi))
  pure
    GodotConfig
      { gcKind = if flag "shared" then Shared else Static,
        gcTarget = if flag "release" then "template_release" else "template_debug",
        gcOptions = options,
        gcModules = flagModules gpd
      }
  where
    flag n = lookupFlagAssignment (mkFlagName n) (flagAssignment lbi) == Just True

-- | The SCons arguments for Godot's build options, from godot-sys's flags.
-- Every flag except @shared@ and @release@ is a build option of the same
-- name with @_@ for @-@, and Godot's default as its default (@disable-3d@ is
-- @disable_3d@, @module-svg-enabled@ is @module_svg_enabled@), except that
-- @optimize-\<level\>@ is @optimize=\<level\>@. Only the options whose
-- flags differ from their defaults are passed (and @optimize@ last), so that
-- the default build keeps its cache key.
sconsOptions :: GenericPackageDescription -> FlagAssignment -> Either String [String]
sconsOptions gpd assignment = case optimize of
  _ : _ : _ -> Left ("more than one optimize flag is on: " ++ unwords (map ("optimize-" ++) optimize))
  _ ->
    Right $
      [ sconsName name ++ "=" ++ (if on then "yes" else "no")
      | (name, def, on) <- options,
        not ("optimize-" `isPrefixOf` name),
        on /= def
      ]
        ++ ["optimize=" ++ sconsName level | level <- optimize]
  where
    options =
      [ (name, flagDefault f, fromMaybe (flagDefault f) (lookupFlagAssignment (flagName f) assignment))
      | f <- genPackageFlags gpd,
        let name = unFlagName (flagName f),
        name `notElem` ["shared", "release"]
      ]
    optimize = [drop (length "optimize-") name | (name, _, True) <- options, "optimize-" `isPrefixOf` name]

-- | The modules that have a flag (@module-\<name\>-enabled@).
flagModules :: GenericPackageDescription -> [String]
flagModules gpd =
  [ sconsName (take (length rest - length "-enabled") rest)
  | f <- genPackageFlags gpd,
    let name = unFlagName (flagName f),
    "module-" `isPrefixOf` name,
    "-enabled" `isSuffixOf` name,
    let rest = drop (length "module-") name
  ]

-- | A build option's name: the flag's with @_@ for @-@.
sconsName :: String -> String
sconsName = map (\c -> if c == '-' then '_' else c)

-- | Check that the modules that have flags are Godot's modules, so that the
-- flags keep up when the pinned version changes.
checkModules :: Verbosity -> FilePath -> GodotConfig -> IO ()
checkModules verb src cfg = do
  dirs <- listDirectory (src </> "modules")
  modules <- filterM (\m -> doesFileExist (src </> "modules" </> m </> "config.py")) dirs
  let unknown = filter (`notElem` modules) (gcModules cfg)
      missing = filter (`notElem` gcModules cfg) modules
  unless (null unknown) $
    failWith ("godot-sys.cabal has flags for modules Godot " ++ godotTag ++ " does not have: " ++ unwords unknown)
  unless (null missing) $
    notice verb ("godot-sys: Godot " ++ godotTag ++ " modules without a flag in godot-sys.cabal: " ++ unwords missing)

libFileName :: LibKind -> FilePath
libFileName Static = "libgodot.a"
libFileName Shared = "libgodot.so"

-- | Installed next to the headers: the classes the built Godot has, one per
-- line (see 'listClasses').
engineClassesFile :: FilePath
engineClassesFile = "engine-classes.txt"

-- | System libraries a static libgodot needs when linking the final program.
-- (X11, Wayland, Vulkan, GL, audio etc. are dlopen'ed by Godot at runtime.)
staticSystemLibs :: [String]
staticSystemLibs = ["stdc++", "m", "pthread", "dl", "rt", "atomic"]

absBuildDir :: LocalBuildInfo -> IO FilePath
absBuildDir lbi = makeAbsolute (interpretSymbolicPathLBI lbi (buildDir lbi))

--------------------------------------------------------------------------------
-- Hooks

main :: IO ()
main =
  defaultMainWithHooks
    simpleUserHooks
      { confHook = godotConfHook,
        buildHook = godotBuildHook,
        copyHook = godotCopyHook,
        instHook = godotInstHook,
        regHook = godotRegHook
      }

godotConfHook ::
  (GenericPackageDescription, HookedBuildInfo) -> ConfigFlags -> IO LocalBuildInfo
godotConfHook args flags = do
  lbi <- confHook simpleUserHooks args flags
  bdir <- absBuildDir lbi
  cfg <- godotConfig lbi
  let pkg = localPkgDescr lbi
      inst = absoluteInstallDirs pkg lbi NoCopyDest
      libDirs = [bdir, libdir inst]
      incDirs = [bdir </> "include", includedir inst]
      addGodot bi =
        bi
          { extraLibs =
              extraLibs bi
                ++ ["godot"]
                ++ (if gcKind cfg == Static then staticSystemLibs else []),
            extraLibDirs = extraLibDirs bi ++ map makeSymbolicPath libDirs,
            includeDirs = includeDirs bi ++ map makeSymbolicPath incDirs,
            ldOptions =
              ldOptions bi
                ++ (if gcKind cfg == Shared then ["-Wl,-rpath," ++ d | d <- libDirs] else [])
          }
      pkg' =
        pkg
          { library = fmap (\l -> l {libBuildInfo = addGodot (libBuildInfo l)}) (library pkg)
          }
  -- Cabal checks that extra-libraries are linkable at configure time, before
  -- Godot has been built. Satisfy it with an empty archive placeholder; the
  -- build step replaces it with the real library.
  createDirectoryIfMissing True bdir
  haveLib <- or <$> mapM (doesFileExist . (bdir </>) . libFileName) [Static, Shared]
  unless haveLib $ writeFile (bdir </> libFileName Static) "!<arch>\n"
  pure lbi {localPkgDescr = pkg'}

godotBuildHook :: PackageDescription -> LocalBuildInfo -> UserHooks -> BuildFlags -> IO ()
godotBuildHook pkg lbi hooks flags = do
  let verb = fromFlagOrDefault normal (buildVerbosity flags)
  buildGodot verb lbi
  buildHook simpleUserHooks pkg lbi hooks flags

godotCopyHook :: PackageDescription -> LocalBuildInfo -> UserHooks -> CopyFlags -> IO ()
godotCopyHook pkg lbi hooks flags = do
  copyHook simpleUserHooks pkg lbi hooks flags
  let verb = fromFlagOrDefault normal (copyVerbosity flags)
      dest = fromFlagOrDefault NoCopyDest (copyDest flags)
  installGodot verb pkg lbi dest

godotInstHook :: PackageDescription -> LocalBuildInfo -> UserHooks -> InstallFlags -> IO ()
godotInstHook pkg lbi hooks flags = do
  let lbi' = withoutBuildDirPaths lbi
  instHook simpleUserHooks (localPkgDescr lbi') lbi' hooks flags
  installGodot (fromFlagOrDefault normal (installVerbosity flags)) pkg lbi NoCopyDest

godotRegHook :: PackageDescription -> LocalBuildInfo -> UserHooks -> RegisterFlags -> IO ()
godotRegHook pkg lbi hooks flags = do
  if fromFlagOrDefault False (regInPlace flags)
    then regHook simpleUserHooks pkg lbi hooks flags
    else do
      let lbi' = withoutBuildDirPaths lbi
      regHook simpleUserHooks (localPkgDescr lbi') lbi' hooks flags

-- | An installed package must not refer to the (temporary) build directory:
-- keep only the install locations among the paths 'godotConfHook' added.
-- (The .cabal file itself declares no extra-lib-dirs, include-dirs or
-- ld-options, so everything else here came from us.)
withoutBuildDirPaths :: LocalBuildInfo -> LocalBuildInfo
withoutBuildDirPaths lbi =
  let pkg = localPkgDescr lbi
      inst = absoluteInstallDirs pkg lbi NoCopyDest
      keepDir d = getSymbolicPath d `elem` [libdir inst, includedir inst]
      keepOpt o = not ("-Wl,-rpath," `isPrefixOf` o) || o == "-Wl,-rpath," ++ libdir inst
      strip bi =
        bi
          { extraLibDirs = filter keepDir (extraLibDirs bi),
            includeDirs = filter keepDir (includeDirs bi),
            ldOptions = filter keepOpt (ldOptions bi)
          }
   in lbi
        { localPkgDescr =
            pkg {library = fmap (\l -> l {libBuildInfo = strip (libBuildInfo l)}) (library pkg)}
        }

--------------------------------------------------------------------------------
-- Building Godot

buildGodot :: Verbosity -> LocalBuildInfo -> IO ()
buildGodot verb lbi = do
  bdir <- absBuildDir lbi
  createDirectoryIfMissing True bdir
  cfg <- godotConfig lbi
  jobs <- maybe (show <$> getNumProcessors) pure =<< lookupEnv "GODOT_SYS_JOBS"
  extra <- maybe [] words <$> lookupEnv "GODOT_SYS_SCONS_FLAGS"
  let sconsArgs =
        [ "platform=linuxbsd",
          "target=" ++ gcTarget cfg,
          "library_type=" ++ (if gcKind cfg == Static then "static_library" else "shared_library"),
          -- Position independent code so the static archive can be linked
          -- into PIE executables and shared Haskell libraries.
          "ccflags=-fPIC",
          -- An embedding host must be able to say where its project / main
          -- pack lives (--path, --main-pack); export templates disable that
          -- by default. Can be turned back on via GODOT_SYS_SCONS_FLAGS.
          "disable_path_overrides=no"
        ]
          ++ gcOptions cfg
          ++ extra
      jobsArg = "-j" ++ jobs
  override <- lookupEnv "GODOT_SYS_SOURCE_DIR"
  out <- case override of
    -- A tree of the user's own may have local changes: build it every time,
    -- and do not cache the result.
    Just dir -> do
      ok <- doesFileExist (dir </> "SConstruct")
      unless ok $ failWith ("GODOT_SYS_SOURCE_DIR=" ++ dir ++ " is not a Godot source tree")
      src <- makeAbsolute dir
      let out = bdir </> "godot-sys-out"
      sconsBuild verb src cfg (sconsArgs ++ [jobsArg]) out
      listClasses verb lbi cfg out
      pure out
    Nothing -> do
      cache <- cacheDir
      createDirectoryIfMissing True cache
      digest <- sha256String (unlines sconsArgs)
      let key = gcTarget cfg ++ "-" ++ map toLowerKind (show (gcKind cfg)) ++ "-" ++ take 12 digest
          out = cache </> "lib" </> key
          complete = out </> ".complete"
      withCacheLock verb cache $ do
        done <- doesFileExist complete
        if done
          then notice verb ("godot-sys: using the cached Godot build in " ++ out)
          else do
            src <- obtainSource verb cache
            removeIfExists out
            sconsBuild verb src cfg (sconsArgs ++ [jobsArg]) out
            writeFile complete (unlines sconsArgs)
        -- (Also for a build cached before godot-sys listed the classes.)
        listed <- doesFileExist (out </> "include" </> engineClassesFile)
        unless listed $ listClasses verb lbi cfg out
      pure out
  -- Stage a copy (not a link, so that the build directory stays usable if
  -- the cache is cleared).
  let staged = bdir </> libFileName (gcKind cfg)
  -- Never leave a library of the other kind lying around in the build dir.
  forM_ [Static, Shared] $ \k -> removeIfExists (bdir </> libFileName k)
  copyFile (out </> libFileName (gcKind cfg)) staged
  removeIfExists (bdir </> "include")
  copyTree (out </> "include") (bdir </> "include")
  info verb ("godot-sys: staged " ++ staged)
  stageExtensionApi lbi (bdir </> "include")
  where
    toLowerKind c = if c >= 'A' && c <= 'Z' then toEnum (fromEnum c + 32) else c

-- | Run SCons in a Godot source tree and put @libgodot.{a,so}@ and the
-- C headers in the given directory.
sconsBuild :: Verbosity -> FilePath -> GodotConfig -> [String] -> FilePath -> IO ()
sconsBuild verb src cfg sconsArgs out = do
  notice verb ("godot-sys: building Godot " ++ godotTag ++ " (" ++ unwords sconsArgs ++ ")")
  checkModules verb src cfg
  -- The merge below takes every archive of this target in bin/obj, but SCons
  -- leaves those of an earlier configuration there (e.g. modules it enabled
  -- and this one disables), which then add dead weight or clash. Remove
  -- them, so that SCons archives just this configuration's (from the
  -- objects it kept, which is quick).
  let ofTarget f = (".linuxbsd." ++ gcTarget cfg ++ ".") `isInfixOf` takeFileName f
  mapM_ removeFile . filter ofTarget =<< findArchives (src </> "bin" </> "obj")
  run src "scons" sconsArgs
  built <- findGodotOutput src cfg
  createDirectoryIfMissing True out
  let lib = out </> libFileName (gcKind cfg)
  case gcKind cfg of
    Shared -> copyFile built lib
    Static -> do
      -- Godot's SCons build produces one archive per subsystem (core, scene,
      -- servers, modules, drivers, thirdparty, ...). Merge them all into a
      -- single libgodot.a so dependents only need -lgodot.
      -- Only this configuration's archives: the same tree may also hold
      -- other targets' builds (e.g. the editor built for extension_api.json).
      let suffix = dropWhile (/= '.') (takeFileName built) -- .linuxbsd.<target>.<arch>.a
      archives <- (built :) . filter (suffix `isSuffixOf`) <$> findArchives (src </> "bin" </> "obj")
      mergeArchives verb out archives lib
  stageHeaders src (out </> "include")

-- | Ask the built Godot which classes it has, and write them to
-- 'engineClassesFile' in @out/include@, where they are staged and installed
-- with the headers. extension_api.json describes a full Godot, but build
-- options (@disable_3d@, modules, ...) leave classes out; godot-low only
-- generates bindings for the classes listed.
--
-- This links tools/list-classes.c with the library and runs it: it starts
-- Godot headless, in a minimal project and with the XDG directories (where
-- Godot keeps logs and settings) in a temporary directory, and writes
-- ClassDB.get_class_list.
listClasses :: Verbosity -> LocalBuildInfo -> GodotConfig -> FilePath -> IO ()
listClasses verb lbi cfg out = do
  notice verb "godot-sys: listing the classes of the built Godot"
  tool <- makeAbsolute (interpretSymbolicPathLBI lbi (makeSymbolicPath ("tools" </> "list-classes.c")))
  let work = out </> "list-classes"
      project = work </> "project"
      exe = work </> "list-classes"
      lib = out </> libFileName (gcKind cfg)
  removeIfExists work
  createDirectoryIfMissing True project
  writeFile (project </> "project.godot") $
    unlines ["config_version=5", "", "[application]", "run/main_scene=\"res://main.tscn\""]
  writeFile (project </> "main.tscn") $
    unlines ["[gd_scene format=3]", "", "[node name=\"Main\" type=\"Node\"]"]
  run work "cc" ["-c", "-I", out </> "include", tool, "-o", exe <.> "o"]
  run work "c++" $
    [exe <.> "o", "-o", exe]
      ++ case gcKind cfg of
        Static -> lib : map ("-l" ++) staticSystemLibs
        Shared -> [lib, "-Wl,-rpath," ++ out]
  env <- getEnvironment
  let xdg = [(v, work </> d) | (v, d) <- [("XDG_DATA_HOME", "data"), ("XDG_CONFIG_HOME", "config"), ("XDG_CACHE_HOME", "cache")]]
      classes = out </> "include" </> engineClassesFile
  (code, stdout', stderr') <-
    readCreateProcessWithExitCode
      (proc exe [project, classes <.> "tmp"]) {cwd = Just work, env = Just (xdg ++ filter ((`notElem` map fst xdg) . fst) env)}
      ""
  unless (code == ExitSuccess) $
    failWith ("listing the classes of the built Godot failed (" ++ show code ++ "):\n" ++ stdout' ++ stderr')
  renameFile (classes <.> "tmp") classes
  removeIfExists work

-- | Where the Godot source, SCons objects and finished libraries are kept:
-- @$GODOT_SYS_CACHE_DIR@, or @~/.cache/godot-sys@.
cacheDir :: IO FilePath
cacheDir = do
  override <- lookupEnv "GODOT_SYS_CACHE_DIR"
  case override of
    Just dir | not (null dir) -> makeAbsolute dir
    _ -> getXdgDirectory XdgCache "godot-sys"

-- | Only one build at a time may use the cache (e.g. HLS and @cabal build@
-- starting together), since SCons cannot share a tree.
withCacheLock :: Verbosity -> FilePath -> IO a -> IO a
withCacheLock verb cache action =
  withFile (cache </> "lock") ReadWriteMode $ \h -> do
    free <- hTryLock h ExclusiveLock
    unless free $ do
      notice verb ("godot-sys: waiting for another build to finish with " ++ cache)
      hLock h ExclusiveLock
    action

-- | Locate (and if necessary download, verify and unpack) the Godot source
-- in the cache.
obtainSource :: Verbosity -> FilePath -> IO FilePath
obtainSource verb cache = do
  let src = cache </> ("godot-" ++ godotTag)
  done <- doesFileExist (src </> ".godot-sys-unpacked")
  unless done $ do
    tarball <- obtainTarball verb cache
    notice verb ("godot-sys: unpacking " ++ tarball)
    removeIfExists src
    createDirectoryIfMissing True src
    run cache "tar" ["-xJf", tarball, "-C", src, "--strip-components=1"]
    writeFile (src </> ".godot-sys-unpacked") godotSha256
  pure src

obtainTarball :: Verbosity -> FilePath -> IO FilePath
obtainTarball verb cache = do
  local <- lookupEnv "GODOT_SYS_TARBALL"
  path <- case local of
    Just p -> makeAbsolute p
    Nothing -> do
      let p = cache </> ("godot-" ++ godotTag ++ ".tar.xz")
      have <- doesFileExist p
      good <- if have then (== godotSha256) <$> sha256 p else pure False
      unless good $ do
        notice verb ("godot-sys: downloading " ++ godotUrl)
        let tmp = p <.> "download"
        run cache "curl" ["--fail", "--location", "--silent", "--show-error", "--output", tmp, godotUrl]
        renameFile tmp p
      pure p
  actual <- sha256 path
  when (actual /= godotSha256) $
    failWith $
      "SHA-256 mismatch for " ++ path ++ "\n  expected " ++ godotSha256 ++ "\n  actual   " ++ actual
  pure path

sha256 :: FilePath -> IO String
sha256 path = takeWhile (not . isSpace) <$> readProcess "sha256sum" [path] ""

sha256String :: String -> IO String
sha256String str = takeWhile (not . isSpace) <$> readProcess "sha256sum" [] str

-- | @bin/libgodot.linuxbsd.<target>.<arch>.{a,so}@
findGodotOutput :: FilePath -> GodotConfig -> IO FilePath
findGodotOutput src cfg = do
  let bin = src </> "bin"
      prefix = "libgodot.linuxbsd." ++ gcTarget cfg ++ "."
      ext = if gcKind cfg == Static then ".a" else ".so"
  files <- listDirectory bin
  case sort [f | f <- files, prefix `isPrefixOf` f, ext `isSuffixOf` f] of
    [f] -> pure (bin </> f)
    [] -> failWith ("SCons produced no " ++ prefix ++ "*" ++ ext ++ " in " ++ bin)
    fs -> failWith ("ambiguous Godot outputs in " ++ bin ++ ": " ++ unwords fs)

findArchives :: FilePath -> IO [FilePath]
findArchives dir = do
  exists <- doesDirectoryExist dir
  if not exists
    then pure []
    else do
      entries <- map (dir </>) . sort <$> listDirectory dir
      dirs <- filterM doesDirectoryExist entries
      let here = [e | e <- entries, ".a" `isSuffixOf` e, e `notElem` dirs]
      nested <- concat <$> mapM findArchives dirs
      pure (here ++ nested)

-- | Merge static archives with an @ar@ MRI script.
mergeArchives :: Verbosity -> FilePath -> [FilePath] -> FilePath -> IO ()
mergeArchives verb workDir archives out = do
  info verb ("godot-sys: merging " ++ show (length archives) ++ " archives into " ++ out)
  let tmp = out <.> "tmp"
      script =
        unlines $
          ["CREATE " ++ tmp]
            ++ ["ADDLIB " ++ a | a <- archives]
            ++ ["SAVE", "END"]
  removeIfExists tmp
  (code, _, err) <- readCreateProcessWithExitCode (proc "ar" ["-M"]) {cwd = Just workDir} script
  unless (code == ExitSuccess) $ failWith ("ar -M failed:\n" ++ err)
  run workDir "ranlib" [tmp]
  renameFile tmp out

-- | Copy the public C headers, preserving the relative include used by
-- libgodot.h (@core/extension/gdextension_interface.gen.h@).
stageHeaders :: FilePath -> FilePath -> IO ()
stageHeaders src inc = do
  let ext = "core" </> "extension"
      files =
        [ (ext </> "libgodot.h", "libgodot.h"),
          (ext </> "gdextension_interface.gen.h", ext </> "gdextension_interface.gen.h"),
          (ext </> "gdextension_interface.json", "gdextension_interface.json")
        ]
  forM_ files $ \(from, to) -> do
    createDirectoryIfMissing True (takeDirectory (inc </> to))
    copyFile (src </> from) (inc </> to)

-- | Install the vendored extension_api.json (the Godot class API, generated
-- by scripts/update-extension-api.sh) next to the headers, after checking it
-- was dumped from the pinned Godot version.
stageExtensionApi :: LocalBuildInfo -> FilePath -> IO ()
stageExtensionApi lbi inc = do
  let src = interpretSymbolicPathLBI lbi (makeSymbolicPath "extension_api.json")
      -- "4.7.2-stable" appears in the header as "Godot Engine v4.7.2.stable..."
      expected = "v" ++ map (\c -> if c == '-' then '.' else c) godotTag ++ "."
  header <- take 4096 <$> readFile src
  unless (expected `isInfixOf` header) $
    failWith $
      src
        ++ " was not generated from Godot "
        ++ godotTag
        ++ "; run scripts/update-extension-api.sh"
  length header `seq` copyFile src (inc </> "extension_api.json")

--------------------------------------------------------------------------------
-- Installing

installGodot :: Verbosity -> PackageDescription -> LocalBuildInfo -> CopyDest -> IO ()
installGodot verb pkg lbi dest = do
  bdir <- absBuildDir lbi
  cfg <- godotConfig lbi
  let inst = absoluteInstallDirs pkg lbi dest
      lib = libFileName (gcKind cfg)
  createDirectoryIfMissing True (libdir inst)
  notice verb ("godot-sys: installing " ++ lib ++ " to " ++ libdir inst)
  copyFile (bdir </> lib) (libdir inst </> lib)
  copyTree (bdir </> "include") (includedir inst)

copyTree :: FilePath -> FilePath -> IO ()
copyTree from to = do
  createDirectoryIfMissing True to
  entries <- listDirectory from
  forM_ entries $ \e -> do
    isDir <- doesDirectoryExist (from </> e)
    if isDir then copyTree (from </> e) (to </> e) else copyFile (from </> e) (to </> e)

--------------------------------------------------------------------------------
-- Utilities

run :: FilePath -> String -> [String] -> IO ()
run dir cmd args = do
  (_, _, _, ph) <- createProcess (proc cmd args) {cwd = Just dir}
  code <- waitForProcess ph
  unless (code == ExitSuccess) $
    failWith (unwords (cmd : args) ++ " failed (" ++ show code ++ ") in " ++ dir)

removeIfExists :: FilePath -> IO ()
removeIfExists p = do
  isFile <- doesFileExist p
  isDir <- doesDirectoryExist p
  when isFile (removeFile p)
  when isDir (removeDirectoryRecursive p)

failWith :: String -> IO a
failWith msg = throwIO (userError ("godot-sys: " ++ msg))
