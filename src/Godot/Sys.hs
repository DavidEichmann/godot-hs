{-# LANGUAGE ForeignFunctionInterface #-}

-- | Raw bindings to the libgodot entry points (@core/extension/libgodot.h@).
--
-- Everything beyond creating and destroying an instance goes through the
-- GDExtension interface: the 'GDExtensionInitializationFunction' you pass to
-- 'libgodotCreateGodotInstance' receives Godot's @get_proc_address@, from which
-- the rest of the API is obtained.
--
-- The C headers (@libgodot.h@, @gdextension_interface.gen.h@) and
-- @gdextension_interface.json@ are installed with this package and are on the
-- include path of any C sources in packages that depend on it.
module Godot.Sys
  ( -- * Pinned version
    godotVersion
    -- * Types
  , GDExtensionObjectPtr
  , GDExtensionInitializationFunction
  , GodotInstance
    -- * libgodot API
  , libgodotCreateGodotInstance
  , libgodotDestroyGodotInstance
    -- * Convenience
  , withGodotArgs
  ) where

import Foreign.C.String (CString, newCString)
import Foreign.C.Types (CInt (..), CUChar)
import Foreign.Marshal.Alloc (free)
import Foreign.Marshal.Array (withArray0)
import Foreign.Ptr (FunPtr, Ptr, nullPtr)
import Control.Exception (bracket)

-- | The Godot version this package builds (see @Setup.hs@).
godotVersion :: String
godotVersion = "4.7.2-stable"

-- | Opaque Godot object.
data GodotInstance

-- | @GDExtensionObjectPtr@: pointer to a Godot object.
type GDExtensionObjectPtr = Ptr GodotInstance

-- | @GDExtensionBool (*)(GDExtensionInterfaceGetProcAddress,
--   GDExtensionClassLibraryPtr, GDExtensionInitialization *)@.
--
-- Left untyped on the argument side; implement it in C or with your own
-- @foreign import ccall "wrapper"@.
type GDExtensionInitializationFunction =
  FunPtr (Ptr () -> Ptr () -> Ptr () -> IO CUChar)

-- | @libgodot_create_godot_instance(argc, argv, init_func)@.
--
-- Returns 'nullPtr' on failure. Only one instance may exist per process.
foreign import ccall safe "libgodot_create_godot_instance"
  libgodotCreateGodotInstance
    :: CInt -> Ptr CString -> GDExtensionInitializationFunction
    -> IO GDExtensionObjectPtr

-- | @libgodot_destroy_godot_instance(instance)@.
foreign import ccall safe "libgodot_destroy_godot_instance"
  libgodotDestroyGodotInstance :: GDExtensionObjectPtr -> IO ()

-- | Marshal a command line (including @argv[0]@) into a NULL-terminated
-- @argv@ array for 'libgodotCreateGodotInstance'.
--
-- Godot may keep pointers into @argv@, so the strings stay alive until the
-- callback returns; destroy the instance within it.
withGodotArgs :: [String] -> (CInt -> Ptr CString -> IO a) -> IO a
withGodotArgs args k =
  bracket (mapM newCString args) (mapM_ free) $ \cargs ->
    withArray0 nullPtr cargs $ \argv ->
      k (fromIntegral (length args)) argv
