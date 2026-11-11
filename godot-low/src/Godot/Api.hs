-- | Bindings to the Godot API (engine classes, builtin types, utility
-- functions), generated from Godot's @extension_api.json@.
--
-- Call 'initialize' from your GDExtension initialization function, then:
--
-- * Types: classes (@Node@, @Node3D@, ...), builtin types (@Vector3@,
--   'GString' for Godot's @String@, @Array@, ...) and enums are re-exported
--   here.
-- * Functions live in one module per class or builtin type; import them
--   qualified:
--
-- @
-- import Godot.Api
-- import qualified Godot.Api.Class.Node as Node
-- import qualified Godot.Api.Builtin.Vector3 as Vector3
-- import qualified Godot.Api.Utility as Utility
--
-- example :: IO ()
-- example = do
--   node <- Node.construct
--   Node.setName node =<< toStringName \"Player\"
--   len <- Vector3.length (Vector3 1 2 2)       -- 3.0
--   ...
-- @
--
-- Methods of a class accept any subclass (@'Inherits' a Node@). Objects
-- returned from methods are 'Maybe' (Godot may return null).
module Godot.Api
  ( -- * Initialization
    initialize

    -- * Classes
  , GodotClass (..)
  , Inherits
  , upcast
  , cast
  , construct
  , singleton

    -- * Variants
  , Variant
  , ToVariant (..)
  , FromVariant (..)
  , variantType
  , stringify

    -- * Strings
  , toGString
  , fromGString
  , toStringName
  , fromStringName

    -- * Generated types
  , module Godot.Api.Builtin.Types
  , module Godot.Api.Classes
  , module Godot.Api.GlobalEnums
  ) where

import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal.Array (allocaArray)
import Foreign.Ptr (castPtr, nullPtr)
import qualified GHC.Foreign as GHC
import GHC.IO.Encoding (utf8)
import Godot.Api.Builtin.Types
import Godot.Api.Classes
import Godot.Api.GlobalEnums
import Godot.Api.Internal
import Godot.GDExtension.Interface
import Godot.GDExtension.Types

-- | A Godot @String@ from a Haskell string.
toGString :: String -> IO GString
toGString s = do
  iface <- getInterface
  constructWith $ \p ->
    GHC.withCStringLen utf8 s $ \(cs, len) -> do
      _ <- string_new_with_utf8_chars_and_len2 iface (GDExtensionUninitializedStringPtr p) cs (fromIntegral len)
      pure ()

-- | A Haskell string from a Godot @String@.
fromGString :: GString -> IO String
fromGString (GString fp) = do
  iface <- getInterface
  withForeignPtr fp $ \p -> do
    let str = GDExtensionConstStringPtr (castPtr p)
    len <- string_to_utf8_chars iface str nullPtr 0
    allocaArray (fromIntegral len) $ \buf -> do
      _ <- string_to_utf8_chars iface str buf len
      GHC.peekCStringLen utf8 (buf, fromIntegral len)

-- | A Godot @StringName@ from a Haskell string.
toStringName :: String -> IO StringName
toStringName s = do
  iface <- getInterface
  constructWith $ \p ->
    GHC.withCString utf8 s (string_name_new_with_utf8_chars iface (GDExtensionUninitializedStringNamePtr p))

-- | A Haskell string from a Godot @StringName@.
fromStringName :: StringName -> IO String
fromStringName sn = toVariant sn >>= stringify

-- | Godot's string representation of a variant (@str(value)@).
stringify :: Variant -> IO String
stringify (Variant fp) = do
  iface <- getInterface
  str <- withForeignPtr fp $ \pv ->
    -- variant_stringify assigns into an initialized String.
    withRet $ \ps ->
      variant_stringify iface (GDExtensionConstVariantPtr (castPtr pv)) (GDExtensionStringPtr ps)
  fromGString str
