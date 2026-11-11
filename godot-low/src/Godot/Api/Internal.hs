{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Runtime support for the generated "Godot.Api" bindings.
--
-- Most users only need 'initialize' (re-exported from "Godot.Api"); the rest
-- is what the generated modules are built from, exposed for writing bindings
-- by hand.
module Godot.Api.Internal
  ( -- * Initialization
    initialize
  , getInterface
  , getLibrary

    -- * Engine classes
  , GodotClass (..)
  , Inherits
  , upcast
  , construct
  , singleton
  , cast
  , objectArg
  , objectRet
  , nullObjectPtr

    -- * Builtin types
  , Builtin (..)

    -- * Variant
  , Variant (..)
  , variantSize
  , newVariant
  , ToVariant (..)
  , FromVariant (..)
  , toVariantVia
  , fromVariantVia
  , variantType

    -- * Marshalling for ptrcalls
  , PtrArg (..)
  , PtrRet (..)
  , storableArg
  , storableRet
  , opaqueArg
  , opaqueRet
  , newOpaque
  , withArgs

    -- * Cached lookups (evaluate only after 'initialize')
  , methodBind
  , builtinMethod
  , builtinConstructor
  , operatorEvaluator
  , utilityFunction
  , withStringName
  , BuiltinMethod
  , UtilityFunction

    -- * Calls
  , ptrcall
  , varargCall
  , callBuiltin
  , callBuiltinVararg
  , callUtility
  , callUtilityVararg
  , callOperator
  ) where

import Control.Exception (bracket_)
import Control.Monad (unless, when)
import Data.IORef
import Data.Int (Int32, Int64)
import Data.Word (Word8)
import Foreign.C.String (withCString)
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc (alloca, allocaBytes)
import Foreign.Marshal.Array (withArrayLen)
import Foreign.Marshal.Utils (fillBytes, with)
import Foreign.Ptr
import Foreign.Storable
import Godot.GDExtension.Interface
import Godot.GDExtension.Types
import System.IO.Unsafe (unsafePerformIO)

--------------------------------------------------------------------------------
-- Global state

data Api = Api
  { apiInterface :: !GDExtensionInterface
  , apiLibrary :: !GDExtensionClassLibraryPtr
  , apiVariantDestroy :: !(FunPtr (Ptr Variant -> IO ()))
  }

{-# NOINLINE apiRef #-}
apiRef :: IORef (Maybe Api)
apiRef = unsafePerformIO (newIORef Nothing)

-- | Load the GDExtension interface and make it available to the generated
-- bindings. Call this from your GDExtension initialization function (the
-- function given to 'Godot.GDExtension.createGodotInstance', or exported from
-- an extension library) before using anything else in "Godot.Api".
--
-- Engine classes become available as Godot reaches the corresponding
-- initialization level (scene classes such as @Node@ after
-- 'GDEXTENSION_INITIALIZATION_SCENE'). Method lookups are cached on first use,
-- so do not call a binding before its class is registered.
initialize
  :: GDExtensionInterfaceGetProcAddress -> GDExtensionClassLibraryPtr -> IO GDExtensionInterface
initialize gpa library = do
  iface <- loadInterface gpa
  destroy <- getProcAddress gpa "variant_destroy"
  when (destroy == nullFunPtr) $ fail "Godot.Api: variant_destroy is unavailable"
  writeIORef apiRef (Just (Api iface library (castFunPtr destroy)))
  pure iface

getApi :: IO Api
getApi =
  readIORef apiRef
    >>= maybe (fail "Godot.Api: not initialized (call Godot.Api.initialize first)") pure

-- | The interface loaded by 'initialize'.
getInterface :: IO GDExtensionInterface
getInterface = apiInterface <$> getApi

-- | The class library pointer given to 'initialize'.
getLibrary :: IO GDExtensionClassLibraryPtr
getLibrary = apiLibrary <$> getApi

-- | Evaluate a lookup once. Used for top-level, NOINLINE caches.
cachedIO :: IO a -> a
cachedIO = unsafePerformIO
{-# NOINLINE cachedIO #-}

--------------------------------------------------------------------------------
-- StringName (minimal, for lookups)

stringNameType :: GDExtensionVariantType
stringNameType = GDEXTENSION_VARIANT_TYPE_STRING_NAME

-- | Run an action with a temporary @StringName@.
withStringName :: String -> (GDExtensionConstStringNamePtr -> IO a) -> IO a
withStringName s k = do
  iface <- getInterface
  destroy <- unwrapGDExtensionPtrDestructor <$> variant_get_ptr_destructor iface stringNameType
  allocaBytes 16 $ \p -> do
    fillBytes p 0 16
    bracket_
      (withCString s (string_name_new_with_utf8_chars iface (GDExtensionUninitializedStringNamePtr p)))
      (destroy (GDExtensionTypePtr p))
      (k (GDExtensionConstStringNamePtr p))

--------------------------------------------------------------------------------
-- Engine classes

-- | A Godot engine class: a handle to a Godot @Object@.
class GodotClass a where
  -- | The Godot class name.
  godotClassName :: String

  toObjectPtr :: a -> GDExtensionObjectPtr

  -- | Wrap an object pointer without checking its class.
  unsafeFromObjectPtr :: GDExtensionObjectPtr -> a

-- | @Inherits a b@: class @a@ is @b@ or one of its subclasses.
class (GodotClass a, GodotClass b) => Inherits a b

-- | Treat an object as one of its base classes.
upcast :: forall b a. Inherits a b => a -> b
upcast = unsafeFromObjectPtr . toObjectPtr

-- | Create a new instance of a class (@classdb_construct_object2@), then
-- send it @NOTIFICATION_POSTINITIALIZE@ as the GDExtension interface
-- requires. (Without it, e.g. a @Control@'s theme cache is never filled
-- and Godot crashes when the control enters the tree.)
--
-- For @RefCounted@ classes the caller owns the object; release it with
-- @RefCounted.unreference@ when done.
construct :: forall a. GodotClass a => IO a
construct = do
  iface <- getInterface
  p <- withStringName (godotClassName @a) (classdb_construct_object2 iface)
  when (p == GDExtensionObjectPtr nullPtr) $
    fail ("Godot.Api.construct: cannot instantiate " ++ godotClassName @a)
  -- Object.notification(NOTIFICATION_POSTINITIALIZE, reversed = false)
  with (0 :: Int64) $ \what -> with (0 :: Word8) $ \reversed ->
    ptrcall objectNotification p [castPtr what, castPtr reversed] nullPtr
  pure (unsafeFromObjectPtr p)

-- | @Object.notification@.
objectNotification :: GDExtensionMethodBindPtr
objectNotification = methodBind "Object" "notification" 4023243586
{-# NOINLINE objectNotification #-}

-- | Get an engine singleton (e.g. @Engine@, @Input@) by its class.
singleton :: forall a. GodotClass a => IO a
singleton = do
  iface <- getInterface
  p <- withStringName (godotClassName @a) (global_get_singleton iface)
  when (p == GDExtensionObjectPtr nullPtr) $
    fail ("Godot.Api.singleton: no singleton " ++ godotClassName @a)
  pure (unsafeFromObjectPtr p)

-- | Checked downcast (@object_cast_to@).
cast :: forall b a. (GodotClass a, GodotClass b) => a -> IO (Maybe b)
cast a = do
  iface <- getInterface
  tag <- withStringName (godotClassName @b) (classdb_get_class_tag iface)
  let GDExtensionObjectPtr p = toObjectPtr a
  GDExtensionObjectPtr r <- object_cast_to iface (GDExtensionConstObjectPtr p) tag
  pure (if r == nullPtr then Nothing else Just (unsafeFromObjectPtr (GDExtensionObjectPtr r)))

-- | Object arguments are passed as a pointer to the object pointer.
objectArg :: GodotClass a => a -> (Ptr () -> IO r) -> IO r
objectArg a k = let GDExtensionObjectPtr p = toObjectPtr a in with p (k . castPtr)

-- | Object results are written as an object pointer (null for none).
objectRet :: GodotClass a => (Ptr () -> IO ()) -> IO (Maybe a)
objectRet k = alloca $ \pp -> do
  poke pp nullPtr
  k (castPtr pp)
  p <- peek pp
  pure (if p == nullPtr then Nothing else Just (unsafeFromObjectPtr (GDExtensionObjectPtr p)))

nullObjectPtr :: GDExtensionObjectPtr
nullObjectPtr = GDExtensionObjectPtr nullPtr

-- | Object results may be null.
instance GodotClass a => PtrRet (Maybe a) where
  withRet = objectRet

--------------------------------------------------------------------------------
-- Builtin types

-- | A Godot builtin (variant) type such as @Vector3@ or @String@.
class (PtrArg a, PtrRet a) => Builtin a where
  -- | Its variant type.
  builtinVariantType :: GDExtensionVariantType

  -- | Allocate a value and initialize it in place with the given function
  -- (which receives a pointer to uninitialized memory, e.g. a constructor).
  constructWith :: (Ptr () -> IO ()) -> IO a

--------------------------------------------------------------------------------
-- Marshalling

-- | Types that can be passed to a ptrcall: the callee receives a pointer to
-- the encoded value.
class PtrArg a where
  withArg :: a -> (Ptr () -> IO r) -> IO r

-- | Types that can be returned from a ptrcall: the callee writes into a
-- buffer prepared by 'withRet'.
class PtrRet a where
  withRet :: (Ptr () -> IO ()) -> IO a

storableArg :: Storable a => a -> (Ptr () -> IO r) -> IO r
storableArg a k = with a (k . castPtr)

storableRet :: forall a. Storable a => (Ptr () -> IO ()) -> IO a
storableRet k = allocaBytes (sizeOf (undefined :: a)) $ \p -> do
  fillBytes p 0 (sizeOf (undefined :: a))
  k p
  peek (castPtr p)

instance PtrArg () where withArg _ k = k nullPtr
instance PtrRet () where withRet k = k nullPtr

-- | Godot encodes all integers as int64 in ptrcalls.
instance PtrArg Int64 where withArg = storableArg
instance PtrRet Int64 where withRet = storableRet

-- | Godot encodes all floats as double in ptrcalls.
instance PtrArg Double where withArg = storableArg
instance PtrRet Double where withRet = storableRet

instance PtrArg Bool where
  withArg b = storableArg (if b then 1 else 0 :: Word8)
instance PtrRet Bool where
  withRet k = (/= (0 :: Word8)) <$> storableRet k

-- | Raw pointer arguments (e.g. @const void*@) are passed by address too.
instance PtrArg (Ptr a) where withArg = storableArg
instance PtrRet (Ptr a) where withRet = storableRet

-- | Opaque builtin values live in Godot-owned memory behind a 'ForeignPtr'.
opaqueArg :: ForeignPtr a -> (Ptr () -> IO r) -> IO r
opaqueArg fp k = withForeignPtr fp (k . castPtr)

-- | Allocate @size@ bytes, initialize them with @initFn@ and attach Godot's
-- destructor for the variant type (if it has one).
newOpaque :: Int -> Maybe GDExtensionVariantType -> (Ptr () -> IO ()) -> IO (ForeignPtr a)
newOpaque size destructorType initFn = do
  fp <- mallocForeignPtrBytes size
  withForeignPtr fp $ \p -> fillBytes p 0 size >> initFn (castPtr p)
  case destructorType of
    Nothing -> pure ()
    Just vt -> do
      iface <- getInterface
      d <- variant_get_ptr_destructor iface vt
      unless (d == nullFunPtr) $ addForeignPtrFinalizer (castFunPtr d) fp
  pure fp

-- | Return buffer for an opaque builtin: default-constructed (pass the type's
-- cached constructor 0), because Godot assigns into it.
opaqueRet
  :: Int
  -> Maybe GDExtensionVariantType
  -- ^ type whose destructor to attach, if it has one
  -> (GDExtensionUninitializedTypePtr -> Ptr GDExtensionConstTypePtr -> IO ())
  -- ^ default constructor
  -> (ForeignPtr a -> b)
  -> (Ptr () -> IO ())
  -> IO b
opaqueRet size destructorType ctor wrap k = do
  fp <- newOpaque size destructorType $ \p ->
    withArgs [] (ctor (GDExtensionUninitializedTypePtr p))
  withForeignPtr fp (k . castPtr)
  pure (wrap fp)

-- | Build a @const void**@ argument array.
withArgs :: [Ptr ()] -> (Ptr GDExtensionConstTypePtr -> IO r) -> IO r
withArgs ps k = withArrayLen (map GDExtensionConstTypePtr ps) (\_ arr -> k arr)

--------------------------------------------------------------------------------
-- Variant

-- | A Godot @Variant@ (owned, destroyed when garbage collected).
newtype Variant = Variant (ForeignPtr Variant)

-- | @sizeof(Variant)@ for single-precision, 64-bit builds.
variantSize :: Int
variantSize = 24

-- | A new nil variant, initialized by the given function (which receives an
-- uninitialized variant pointer).
newVariant :: (Ptr () -> IO ()) -> IO Variant
newVariant initFn = do
  api <- getApi
  fp <- mallocForeignPtrBytes variantSize
  withForeignPtr fp $ \p -> do
    fillBytes p 0 variantSize
    variant_new_nil (apiInterface api) (GDExtensionUninitializedVariantPtr (castPtr p))
    initFn (castPtr p)
  addForeignPtrFinalizer (apiVariantDestroy api) fp
  pure (Variant fp)

instance PtrArg Variant where withArg (Variant fp) = opaqueArg fp
instance PtrRet Variant where withRet k = newVariant k

-- | The Godot type of a variant.
variantType :: Variant -> IO GDExtensionVariantType
variantType (Variant fp) = do
  iface <- getInterface
  withForeignPtr fp (variant_get_type iface . GDExtensionConstVariantPtr . castPtr)

class ToVariant a where
  toVariant :: a -> IO Variant

class FromVariant a where
  -- | Convert, without checking the variant's type.
  fromVariant :: Variant -> IO a

instance ToVariant Variant where toVariant = pure
instance FromVariant Variant where fromVariant = pure

-- | Convert with Godot's variant-from-type constructor.
toVariantVia :: PtrArg a => GDExtensionVariantType -> a -> IO Variant
toVariantVia vt a = do
  iface <- getInterface
  from <- unwrapGDExtensionVariantFromTypeConstructorFunc <$> get_variant_from_type_constructor iface vt
  withArg a $ \pa ->
    newVariant $ \pv -> from (GDExtensionUninitializedVariantPtr pv) (GDExtensionTypePtr pa)

-- | Convert with Godot's type-from-variant constructor.
fromVariantVia :: PtrRet a => GDExtensionVariantType -> Variant -> IO a
fromVariantVia vt (Variant fp) = do
  iface <- getInterface
  to <- unwrapGDExtensionTypeFromVariantConstructorFunc <$> get_variant_to_type_constructor iface vt
  withForeignPtr fp $ \pv ->
    withRet $ \pr -> to (GDExtensionUninitializedTypePtr pr) (GDExtensionVariantPtr (castPtr pv))

instance ToVariant Bool where toVariant = toVariantVia GDEXTENSION_VARIANT_TYPE_BOOL
instance FromVariant Bool where fromVariant = fromVariantVia GDEXTENSION_VARIANT_TYPE_BOOL
instance ToVariant Int64 where toVariant = toVariantVia GDEXTENSION_VARIANT_TYPE_INT
instance FromVariant Int64 where fromVariant = fromVariantVia GDEXTENSION_VARIANT_TYPE_INT
instance ToVariant Double where toVariant = toVariantVia GDEXTENSION_VARIANT_TYPE_FLOAT
instance FromVariant Double where fromVariant = fromVariantVia GDEXTENSION_VARIANT_TYPE_FLOAT

--------------------------------------------------------------------------------
-- Cached lookups

-- | @classdb_get_method_bind@, looked up once.
methodBind :: String -> String -> Int64 -> GDExtensionMethodBindPtr
methodBind cls name hash = cachedIO $ do
  iface <- getInterface
  mb <- withStringName cls $ \c -> withStringName name $ \n -> classdb_get_method_bind iface c n hash
  when (mb == GDExtensionMethodBindPtr nullPtr) $
    fail ("Godot.Api: method " ++ cls ++ "." ++ name ++ " (hash " ++ show hash ++ ") not found")
  pure mb
{-# NOINLINE methodBind #-}

type BuiltinMethod = GDExtensionTypePtr -> Ptr GDExtensionConstTypePtr -> GDExtensionTypePtr -> Int32 -> IO ()

-- | @variant_get_ptr_builtin_method@, looked up once.
builtinMethod :: GDExtensionVariantType -> String -> Int64 -> BuiltinMethod
builtinMethod vt name hash = cachedIO $ do
  iface <- getInterface
  f <- withStringName name $ \n -> variant_get_ptr_builtin_method iface vt n hash
  when (f == nullFunPtr) $
    fail ("Godot.Api: builtin method " ++ show vt ++ "." ++ name ++ " not found")
  pure (unwrapGDExtensionPtrBuiltInMethod f)
{-# NOINLINE builtinMethod #-}

-- | @variant_get_ptr_constructor@, looked up once.
builtinConstructor
  :: GDExtensionVariantType -> Int32 -> GDExtensionUninitializedTypePtr -> Ptr GDExtensionConstTypePtr -> IO ()
builtinConstructor vt idx = cachedIO $ do
  iface <- getInterface
  f <- variant_get_ptr_constructor iface vt idx
  when (f == nullFunPtr) $
    fail ("Godot.Api: constructor " ++ show idx ++ " of " ++ show vt ++ " not found")
  pure (unwrapGDExtensionPtrConstructor f)
{-# NOINLINE builtinConstructor #-}

-- | @variant_get_ptr_operator_evaluator@, looked up once.
operatorEvaluator
  :: GDExtensionVariantOperator -> GDExtensionVariantType -> GDExtensionVariantType
  -> GDExtensionConstTypePtr -> GDExtensionConstTypePtr -> GDExtensionTypePtr -> IO ()
operatorEvaluator op a b = cachedIO $ do
  iface <- getInterface
  f <- variant_get_ptr_operator_evaluator iface op a b
  when (f == nullFunPtr) $
    fail ("Godot.Api: operator " ++ show op ++ " for " ++ show (a, b) ++ " not found")
  pure (unwrapGDExtensionPtrOperatorEvaluator f)
{-# NOINLINE operatorEvaluator #-}

type UtilityFunction = GDExtensionTypePtr -> Ptr GDExtensionConstTypePtr -> Int32 -> IO ()

-- | @variant_get_ptr_utility_function@, looked up once.
utilityFunction :: String -> Int64 -> UtilityFunction
utilityFunction name hash = cachedIO $ do
  iface <- getInterface
  f <- withStringName name $ \n -> variant_get_ptr_utility_function iface n hash
  when (f == nullFunPtr) $ fail ("Godot.Api: utility function " ++ name ++ " not found")
  pure (unwrapGDExtensionPtrUtilityFunction f)
{-# NOINLINE utilityFunction #-}

--------------------------------------------------------------------------------
-- Calls

-- | @object_method_bind_ptrcall@ (null object for static methods).
ptrcall :: GDExtensionMethodBindPtr -> GDExtensionObjectPtr -> [Ptr ()] -> Ptr () -> IO ()
ptrcall mb obj args ret = do
  iface <- getInterface
  withArgs args $ \argv -> object_method_bind_ptrcall iface mb obj argv (GDExtensionTypePtr ret)

-- | @object_method_bind_call@ for vararg methods. Throws on call errors.
varargCall :: GDExtensionMethodBindPtr -> GDExtensionObjectPtr -> [Variant] -> IO Variant
varargCall mb obj args = do
  iface <- getInterface
  withVariantPtrs args $ \n argv ->
    alloca $ \perr -> do
      poke perr (GDExtensionCallError GDEXTENSION_CALL_OK 0 0)
      r <- newVariant $ \pr ->
        object_method_bind_call iface mb obj (castPtr argv) (fromIntegral n) (GDExtensionUninitializedVariantPtr pr) perr
      checkCallError perr
      pure r

withVariantPtrs :: [Variant] -> (Int -> Ptr (Ptr ()) -> IO r) -> IO r
withVariantPtrs vs k = go vs []
  where
    go [] acc = withArrayLen (reverse acc) k
    go (Variant fp : rest) acc = withForeignPtr fp $ \p -> go rest (castPtr p : acc)

checkCallError :: Ptr GDExtensionCallError -> IO ()
checkCallError perr = do
  e <- peek perr
  unless (gdExtensionCallError_error e == GDEXTENSION_CALL_OK) $
    fail ("Godot.Api: call failed: " ++ show e)

-- | Call a builtin method on @self@ (null for static methods).
callBuiltin :: BuiltinMethod -> Ptr () -> [Ptr ()] -> Ptr () -> IO ()
callBuiltin f self args ret =
  withArgs args $ \argv -> f (GDExtensionTypePtr self) argv (GDExtensionTypePtr ret) (fromIntegral (length args))

-- | Vararg builtin methods take every argument as a 'Variant'.
callBuiltinVararg :: BuiltinMethod -> Ptr () -> [Variant] -> Ptr () -> IO ()
callBuiltinVararg f self args ret =
  withVariantPtrs args $ \n argv -> f (GDExtensionTypePtr self) (castPtr argv) (GDExtensionTypePtr ret) (fromIntegral n)

callUtility :: UtilityFunction -> [Ptr ()] -> Ptr () -> IO ()
callUtility f args ret =
  withArgs args $ \argv -> f (GDExtensionTypePtr ret) argv (fromIntegral (length args))

callUtilityVararg :: UtilityFunction -> [Variant] -> Ptr () -> IO ()
callUtilityVararg f args ret =
  withVariantPtrs args $ \n argv -> f (GDExtensionTypePtr ret) (castPtr argv) (fromIntegral n)

callOperator
  :: (GDExtensionConstTypePtr -> GDExtensionConstTypePtr -> GDExtensionTypePtr -> IO ())
  -> Ptr () -> Ptr () -> Ptr () -> IO ()
callOperator f a b ret = f (GDExtensionConstTypePtr a) (GDExtensionConstTypePtr b) (GDExtensionTypePtr ret)
