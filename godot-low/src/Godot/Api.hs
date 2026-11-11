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
{-# LANGUAGE ForeignFunctionInterface #-}

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

    -- * Callables
  , newCallable

    -- * Generated types
  , module Godot.Api.Builtin.Types
  , module Godot.Api.Classes
  , module Godot.Api.GlobalEnums
  ) where

import Control.Exception (SomeException, try)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal.Array (allocaArray, peekArray)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (castPtr, nullFunPtr, nullPtr)
import Foreign.StablePtr
import Foreign.Storable (poke)
import qualified GHC.Foreign as GHC
import GHC.IO.Encoding (utf8)
import Godot.Api.Builtin.Types
import Godot.Api.Classes
import Godot.Api.GlobalEnums
import Godot.Api.Internal
import Godot.GDExtension.Interface
import Godot.GDExtension.Types
import System.IO (hPutStrLn, stderr)
import System.IO.Unsafe (unsafePerformIO)

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

-- | A Godot @Callable@ that runs a Haskell function, e.g. to connect to a
-- signal:
--
-- @
-- pressed  <- toStringName \"pressed\"
-- callback <- newCallable (\\_args -> putStrLn \"clicked\")
-- _ <- Object.connect button pressed callback 0
-- @
--
-- The function receives the call's arguments (the signal's arguments) and
-- the callable returns nil. Godot calls it on whichever thread makes the
-- call (the main thread, for signals emitted by the scene tree). Exceptions
-- are reported on stderr and otherwise ignored. The function is kept alive
-- until Godot drops its last copy of the callable (e.g. when the object it
-- is connected to is freed).
--
-- Do not let the function capture the object it is connected to if that
-- would be the only thing keeping it alive: the callable is a GC root.
newCallable :: ([Variant] -> IO ()) -> IO Callable
newCallable f = do
  iface <- getInterface
  GDExtensionClassLibraryPtr library <- getLibrary
  userdata <- newStablePtr f
  constructWith $ \p ->
    with
      GDExtensionCallableCustomInfo2
        { gdExtensionCallableCustomInfo2_callable_userdata = castStablePtrToPtr userdata
        , gdExtensionCallableCustomInfo2_token = library
        , gdExtensionCallableCustomInfo2_object_id = 0
        , gdExtensionCallableCustomInfo2_call_func = callableCall
        , gdExtensionCallableCustomInfo2_is_valid_func = nullFunPtr
        , gdExtensionCallableCustomInfo2_free_func = callableFree
        , gdExtensionCallableCustomInfo2_hash_func = nullFunPtr
        , gdExtensionCallableCustomInfo2_equal_func = nullFunPtr
        , gdExtensionCallableCustomInfo2_less_than_func = nullFunPtr
        , gdExtensionCallableCustomInfo2_to_string_func = nullFunPtr
        , gdExtensionCallableCustomInfo2_get_argument_count_func = nullFunPtr
        }
      (callable_custom_create2 iface (GDExtensionUninitializedTypePtr p))

-- | The @call_func@ shared by all 'newCallable' callables: the userdata is a
-- 'StablePtr' to the Haskell function.
callableCall :: GDExtensionCallableCustomCall
callableCall = unsafePerformIO $ wrapGDExtensionCallableCustomCall $ \userdata argv argc _ret perr -> do
  f <- deRefStablePtr (castPtrToStablePtr userdata) :: IO ([Variant] -> IO ())
  iface <- getInterface
  args <- peekArray (fromIntegral argc) argv
  -- The arguments are borrowed: copy them into variants we own.
  owned <- mapM (\arg -> newVariant (\pv -> variant_new_copy iface (GDExtensionUninitializedVariantPtr pv) arg)) args
  r <- try (f owned)
  case r of
    Left e -> hPutStrLn stderr ("Godot.Api.newCallable: uncaught exception: " ++ show (e :: SomeException))
    Right () -> pure ()
  poke perr (GDExtensionCallError GDEXTENSION_CALL_OK 0 0)
{-# NOINLINE callableCall #-}

-- | The @free_func@: releases the 'StablePtr'. This is the RTS's own C
-- function, not a Haskell callback, because Godot may drop the last copy of
-- a callable from a 'Callable' finalizer, i.e. during garbage collection,
-- when calling back into Haskell is not allowed.
foreign import ccall "&hs_free_stable_ptr" callableFree :: GDExtensionCallableCustomFree
