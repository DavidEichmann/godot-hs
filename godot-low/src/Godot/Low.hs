-- | Low-level GDExtension bindings to Godot.
--
-- Re-exports the generated "Godot.Low.Types" and "Godot.Low.Interface", and
-- wraps the libgodot entry points from "Godot.Sys" with the generated types.
module Godot.Low
  ( module Godot.Low.Types
  , module Godot.Low.Interface
    -- * Embedding Godot (libgodot)
  , createGodotInstance
  , destroyGodotInstance
  , withGodotArgs
  ) where

import Foreign.C.String (CString)
import Foreign.C.Types (CInt)
import Foreign.Ptr (Ptr, castFunPtr, castPtr)
import Godot.Low.Interface
import Godot.Low.Types
import Godot.Sys (withGodotArgs)
import qualified Godot.Sys as Sys

-- | @libgodot_create_godot_instance@: start Godot inside this process.
--
-- The initialization function is called during creation with Godot's
-- @get_proc_address@ (pass it to 'loadInterface'). Returns a null handle on
-- failure. Only one instance may exist per process. Use 'withGodotArgs' to
-- build @argc@ / @argv@.
createGodotInstance
  :: CInt -> Ptr CString -> GDExtensionInitializationFunction -> IO GDExtensionObjectPtr
createGodotInstance argc argv initFn =
  GDExtensionObjectPtr . castPtr <$> Sys.libgodotCreateGodotInstance argc argv (castFunPtr initFn)

-- | @libgodot_destroy_godot_instance@.
destroyGodotInstance :: GDExtensionObjectPtr -> IO ()
destroyGodotInstance (GDExtensionObjectPtr p) = Sys.libgodotDestroyGodotInstance (castPtr p)
