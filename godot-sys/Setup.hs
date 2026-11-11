-- | Custom setup for godot-sys.
--
-- * configure: works out where libgodot will live and adds it (plus its
--   system dependencies, headers and, for shared builds, an rpath) to the
--   library's build info, so that dependents link against it.
-- * build: fetches the pinned Godot source, builds it with SCons as a static
--   or shared library, and stages @libgodot.{a,so}@ and the C headers in the
--   component build directory.
-- * copy/install: copies those artefacts next to the installed Haskell
--   library.
module Main (main) where

import Control.Exception (throwIO)
import Control.Monad (filterM, forM_, unless, when)
import Data.Char (isSpace)
import Data.List (isInfixOf, isPrefixOf, isSuffixOf, sort)
import Distribution.PackageDescription
  ( BuildInfo (..)
  , GenericPackageDescription
  , HookedBuildInfo
  , Library (..)
  , PackageDescription (..)
  , lookupFlagAssignment
  , mkFlagName
  )
import Distribution.Simple
import Distribution.Simple.InstallDirs (CopyDest (..), InstallDirs (..))
import Distribution.Simple.LocalBuildInfo
  ( LocalBuildInfo (..)
  , absoluteInstallDirs
  , buildDir
  , interpretSymbolicPathLBI
  )
import Distribution.Simple.Setup
  ( BuildFlags
  , ConfigFlags
  , CopyFlags
  , InstallFlags
  , RegisterFlags
  , buildVerbosity
  , copyDest
  , copyVerbosity
  , fromFlagOrDefault
  , installVerbosity
  , regInPlace
  )
import Distribution.Simple.Utils (info, notice)
import Distribution.Utils.Path (getSymbolicPath, makeSymbolicPath)
import Distribution.Verbosity (Verbosity, normal)
import GHC.Conc (getNumProcessors)
import System.Directory
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath
import System.Process

--------------------------------------------------------------------------------
-- Pinned Godot version

godotTag :: String
godotTag = "4.7.2-stable"

-- | Official source release tarball published by the Godot project.
godotUrl :: String
godotUrl =
  "https://github.com/godotengine/godot-builds/releases/download/"
    ++ godotTag ++ "/godot-" ++ godotTag ++ ".tar.xz"

-- | SHA-256 of 'godotUrl' (matches the upstream @.tar.xz.sha256@ file).
godotSha256 :: String
godotSha256 = "a18ce0ccec3ecc40b0dd6c4f5132ca934e9fb7c2979717940ff32aee1eb35481"

--------------------------------------------------------------------------------
-- Configuration derived from flags

data LibKind = Static | Shared deriving (Eq, Show)

data GodotConfig = GodotConfig
  { gcKind   :: LibKind
  , gcTarget :: String -- ^ SCons @target=@
  }

godotConfig :: LocalBuildInfo -> GodotConfig
godotConfig lbi =
  GodotConfig
    { gcKind   = if flag "shared" then Shared else Static
    , gcTarget = if flag "release" then "template_release" else "template_debug"
    }
  where
    flag n = lookupFlagAssignment (mkFlagName n) (flagAssignment lbi) == Just True

libFileName :: LibKind -> FilePath
libFileName Static = "libgodot.a"
libFileName Shared = "libgodot.so"

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
      { confHook  = godotConfHook
      , buildHook = godotBuildHook
      , copyHook  = godotCopyHook
      , instHook  = godotInstHook
      , regHook   = godotRegHook
      }

godotConfHook
  :: (GenericPackageDescription, HookedBuildInfo) -> ConfigFlags -> IO LocalBuildInfo
godotConfHook args flags = do
  lbi <- confHook simpleUserHooks args flags
  bdir <- absBuildDir lbi
  let cfg = godotConfig lbi
      pkg = localPkgDescr lbi
      inst = absoluteInstallDirs pkg lbi NoCopyDest
      libDirs = [bdir, libdir inst]
      incDirs = [bdir </> "include", includedir inst]
      addGodot bi =
        bi
          { extraLibs = extraLibs bi ++ ["godot"] ++
              (if gcKind cfg == Static then staticSystemLibs else [])
          , extraLibDirs = extraLibDirs bi ++ map makeSymbolicPath libDirs
          , includeDirs = includeDirs bi ++ map makeSymbolicPath incDirs
          , ldOptions = ldOptions bi ++
              (if gcKind cfg == Shared then ["-Wl,-rpath," ++ d | d <- libDirs] else [])
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
          { extraLibDirs = filter keepDir (extraLibDirs bi)
          , includeDirs = filter keepDir (includeDirs bi)
          , ldOptions = filter keepOpt (ldOptions bi)
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
  let cfg = godotConfig lbi
  src <- obtainSource verb bdir
  jobs <- maybe (show <$> getNumProcessors) pure =<< lookupEnv "GODOT_SYS_JOBS"
  extra <- maybe [] words <$> lookupEnv "GODOT_SYS_SCONS_FLAGS"
  let sconsArgs =
        [ "platform=linuxbsd"
        , "target=" ++ gcTarget cfg
        , "library_type=" ++ (if gcKind cfg == Static then "static_library" else "shared_library")
          -- Position independent code so the static archive can be linked
          -- into PIE executables and shared Haskell libraries.
        , "ccflags=-fPIC"
          -- An embedding host must be able to say where its project / main
          -- pack lives (--path, --main-pack); export templates disable that
          -- by default. Can be turned back on via GODOT_SYS_SCONS_FLAGS.
        , "disable_path_overrides=no"
        , "-j" ++ jobs
        ]
          ++ extra
  notice verb ("godot-sys: building Godot " ++ godotTag ++ " (" ++ unwords sconsArgs ++ ")")
  run src "scons" sconsArgs

  out <- findGodotOutput src cfg
  let staged = bdir </> libFileName (gcKind cfg)
  -- Never leave a library of the other kind lying around in the build dir.
  forM_ [Static, Shared] $ \k -> removeIfExists (bdir </> libFileName k)
  case gcKind cfg of
    Shared -> copyFile out staged
    Static -> do
      -- Godot's SCons build produces one archive per subsystem (core, scene,
      -- servers, modules, drivers, thirdparty, ...). Merge them all into a
      -- single libgodot.a so dependents only need -lgodot.
      -- Only this configuration's archives: the same tree may also hold
      -- other targets' builds (e.g. the editor built for extension_api.json).
      let suffix = dropWhile (/= '.') (takeFileName out) -- .linuxbsd.<target>.<arch>.a
      archives <- (out :) . filter (suffix `isSuffixOf`) <$> findArchives (src </> "bin" </> "obj")
      mergeArchives verb bdir archives staged
  info verb ("godot-sys: staged " ++ staged)
  stageHeaders src (bdir </> "include")
  stageExtensionApi lbi (bdir </> "include")

-- | Locate (and if necessary download, verify and unpack) the Godot source.
obtainSource :: Verbosity -> FilePath -> IO FilePath
obtainSource verb bdir = do
  override <- lookupEnv "GODOT_SYS_SOURCE_DIR"
  case override of
    Just dir -> do
      ok <- doesFileExist (dir </> "SConstruct")
      unless ok $ failWith ("GODOT_SYS_SOURCE_DIR=" ++ dir ++ " is not a Godot source tree")
      makeAbsolute dir
    Nothing -> do
      let src = bdir </> ("godot-" ++ godotTag)
          stamp = src </> ".godot-sys-unpacked"
      done <- doesFileExist stamp
      unless done $ do
        tarball <- obtainTarball verb bdir
        notice verb ("godot-sys: unpacking " ++ tarball)
        removeIfExists src
        createDirectoryIfMissing True src
        run bdir "tar" ["-xJf", tarball, "-C", src, "--strip-components=1"]
        writeFile stamp godotSha256
      pure src

obtainTarball :: Verbosity -> FilePath -> IO FilePath
obtainTarball verb bdir = do
  local <- lookupEnv "GODOT_SYS_TARBALL"
  path <- case local of
    Just p -> makeAbsolute p
    Nothing -> do
      let p = bdir </> ("godot-" ++ godotTag ++ ".tar.xz")
      have <- doesFileExist p
      good <- if have then (== godotSha256) <$> sha256 p else pure False
      unless good $ do
        notice verb ("godot-sys: downloading " ++ godotUrl)
        run bdir "curl" ["--fail", "--location", "--silent", "--show-error", "--output", p, godotUrl]
      pure p
  actual <- sha256 path
  when (actual /= godotSha256) $
    failWith $
      "SHA-256 mismatch for " ++ path ++ "\n  expected " ++ godotSha256 ++ "\n  actual   " ++ actual
  pure path

sha256 :: FilePath -> IO String
sha256 path = takeWhile (not . isSpace) <$> readProcess "sha256sum" [path] ""

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
        [ (ext </> "libgodot.h", "libgodot.h")
        , (ext </> "gdextension_interface.gen.h", ext </> "gdextension_interface.gen.h")
        , (ext </> "gdextension_interface.json", "gdextension_interface.json")
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
      src ++ " was not generated from Godot " ++ godotTag
        ++ "; run scripts/update-extension-api.sh"
  length header `seq` copyFile src (inc </> "extension_api.json")

--------------------------------------------------------------------------------
-- Installing

installGodot :: Verbosity -> PackageDescription -> LocalBuildInfo -> CopyDest -> IO ()
installGodot verb pkg lbi dest = do
  bdir <- absBuildDir lbi
  let cfg = godotConfig lbi
      inst = absoluteInstallDirs pkg lbi dest
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
