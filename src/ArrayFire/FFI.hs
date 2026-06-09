{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
--------------------------------------------------------------------------------
-- |
-- Module      : ArrayFire.FFI
-- Copyright   : David Johnson (c) 2019-2026
-- License     : BSD 3
-- Maintainer  : David Johnson <code@dmj.io>
-- Stability   : Experimental
-- Portability : GHC
--
-- Internal marshalling combinators that bridge the high-level API modules and
-- the raw @ArrayFire.Internal.*@ FFI bindings. Each combinator unwraps the
-- managed handles ('Array', 'Window', 'Features', 'RandomEngine'), allocates
-- the output pointers, invokes the supplied C function, checks the returned
-- 'AFErr' with 'throwAFError', and attaches the appropriate finalizer to any
-- newly-created handle. These helpers are not part of the public API.
--------------------------------------------------------------------------------
module ArrayFire.FFI where

import ArrayFire.Exception
import ArrayFire.Types
import ArrayFire.Internal.Defines
import ArrayFire.Internal.Features
import ArrayFire.Internal.Array
import ArrayFire.Internal.Types

import Control.Exception
import Control.Monad
import Data.Int
import Foreign.ForeignPtr
import Foreign.Storable
import Foreign.Ptr
import Foreign.C
import Foreign.Marshal.Alloc
import System.IO.Unsafe

foreign import ccall unsafe "af_cast"
    af_cast :: Ptr AFArray -> AFArray -> AFDtype -> IO AFErr

foreign import ccall unsafe "af_release_array"
    af_release_array_ffi :: AFArray -> IO AFErr

-- | Applies a C function that takes three input 'Array's and produces a single
-- output 'Array'.
op3
  :: Array b
  -> Array a
  -> Array a
  -> (Ptr AFArray -> AFArray -> AFArray -> AFArray -> IO AFErr)
  -> Array a
{-# NOINLINE op3 #-}
op3 (Array fptr1) (Array fptr2) (Array fptr3) op =
  unsafePerformIO . mask_ $ do
    withForeignPtr fptr1 $ \ptr1 ->
      withForeignPtr fptr2 $ \ptr2 -> do
         withForeignPtr fptr3 $ \ptr3 -> do
           ptr <-
             alloca $ \ptrInput -> do
               throwAFError =<< op ptrInput ptr1 ptr2 ptr3
               peek ptrInput
           fptr <- newForeignPtr af_release_array_finalizer ptr
           pure (Array fptr)

-- | Like 'op3', but specialised to two 'Int32' index 'Array's alongside the
-- primary input.
op3Int
  :: Array a
  -> Array Int32
  -> Array Int32
  -> (Ptr AFArray -> AFArray -> AFArray -> AFArray -> IO AFErr)
  -> Array a
{-# NOINLINE op3Int #-}
op3Int (Array fptr1) (Array fptr2) (Array fptr3) op =
  unsafePerformIO . mask_ $ do
    withForeignPtr fptr1 $ \ptr1 ->
      withForeignPtr fptr2 $ \ptr2 -> do
         withForeignPtr fptr3 $ \ptr3 -> do
           ptr <-
             alloca $ \ptrInput -> do
               throwAFError =<< op ptrInput ptr1 ptr2 ptr3
               peek ptrInput
           fptr <- newForeignPtr af_release_array_finalizer ptr
           pure (Array fptr)

-- | Applies a C function that takes two input 'Array's and produces a single
-- output 'Array'.
op2
  :: Array b
  -> Array a
  -> (Ptr AFArray -> AFArray -> AFArray -> IO AFErr)
  -> Array c
{-# NOINLINE op2 #-}
op2 (Array fptr1) (Array fptr2) op =
  unsafePerformIO . mask_ $ do
    withForeignPtr fptr1 $ \ptr1 ->
      withForeignPtr fptr2 $ \ptr2 -> do
        ptr <-
          alloca $ \ptrInput -> do
            throwAFError =<< op ptrInput ptr1 ptr2
            peek ptrInput
        fptr <- newForeignPtr af_release_array_finalizer ptr
        pure (Array fptr)

-- | Like 'op2', but for comparison operations whose output 'Array' holds
-- boolean ('CBool') values.
op2bool
  :: Array b
  -> Array a
  -> (Ptr AFArray -> AFArray -> AFArray -> IO AFErr)
  -> Array CBool
{-# NOINLINE op2bool #-}
op2bool (Array fptr1) (Array fptr2) op =
  unsafePerformIO . mask_ $ do
    withForeignPtr fptr1 $ \ptr1 ->
      withForeignPtr fptr2 $ \ptr2 -> do
        ptr <-
          alloca $ \ptrInput -> do
            throwAFError =<< op ptrInput ptr1 ptr2
            peek ptrInput
        fptr <- newForeignPtr af_release_array_finalizer ptr
        pure (Array fptr)


-- | Applies a C function that takes one input 'Array' and produces a pair of
-- output 'Array's.
op2p
  :: Array a
  -> (Ptr AFArray -> Ptr AFArray -> AFArray -> IO AFErr)
  -> (Array a, Array b)
{-# NOINLINE op2p #-}
op2p (Array fptr1) op =
  unsafePerformIO . mask_ $ do
    (x,y) <- withForeignPtr fptr1 $ \ptr1 -> do
        alloca $ \ptrInput1 -> do
          alloca $ \ptrInput2 -> do
            throwAFError =<< op ptrInput1 ptrInput2 ptr1
            (,) <$> peek ptrInput1 <*> peek ptrInput2
    fptrA <- newForeignPtr af_release_array_finalizer x
    fptrB <- newForeignPtr af_release_array_finalizer y
    pure (Array fptrA, Array fptrB)

-- | Applies a C function that takes one input 'Array' and produces a triple of
-- output 'Array's (e.g. an SVD or LU decomposition).
op3p
  :: Array a
  -> (Ptr AFArray -> Ptr AFArray -> Ptr AFArray -> AFArray -> IO AFErr)
  -> (Array a, Array a, Array a)
{-# NOINLINE op3p #-}
op3p (Array fptr1) op =
  unsafePerformIO . mask_ $ do
    (x,y,z) <- withForeignPtr fptr1 $ \ptr1 -> do
        alloca $ \ptrInput1 -> do
          alloca $ \ptrInput2 -> do
            alloca $ \ptrInput3 -> do
              throwAFError =<< op ptrInput1 ptrInput2 ptrInput3 ptr1
              (,,) <$> peek ptrInput1 <*> peek ptrInput2 <*> peek ptrInput3
    fptrA <- newForeignPtr af_release_array_finalizer x
    fptrB <- newForeignPtr af_release_array_finalizer y
    fptrC <- newForeignPtr af_release_array_finalizer z
    pure (Array fptrA, Array fptrB, Array fptrC)

-- | Like 'op3p', but the C function also writes back a single 'Storable'
-- scalar in addition to the three output 'Array's.
op3p1
  :: Storable b
  => Array a
  -> (Ptr AFArray -> Ptr AFArray -> Ptr AFArray -> Ptr b -> AFArray -> IO AFErr)
  -> (Array a, Array a, Array a, b)
{-# NOINLINE op3p1 #-}
op3p1 (Array fptr1) op =
  unsafePerformIO . mask_ $ do
    (x,y,z,g) <- withForeignPtr fptr1 $ \ptr1 -> do
        alloca $ \ptrInput1 -> do
          alloca $ \ptrInput2 -> do
            alloca $ \ptrInput3 -> do
              alloca $ \ptrInput4 -> do
                throwAFError =<< op ptrInput1 ptrInput2 ptrInput3 ptrInput4 ptr1
                (,,,) <$> peek ptrInput1
                      <*> peek ptrInput2
                      <*> peek ptrInput3
                      <*> peek ptrInput4
    fptrA <- newForeignPtr af_release_array_finalizer x
    fptrB <- newForeignPtr af_release_array_finalizer y
    fptrC <- newForeignPtr af_release_array_finalizer z
    pure (Array fptrA, Array fptrB, Array fptrC, g)

-- | Applies a C function that takes two input 'Array's and produces a pair of
-- output 'Array's.
op2p2
  :: Array a
  -> Array a
  -> (Ptr AFArray -> Ptr AFArray -> AFArray -> AFArray -> IO AFErr)
  -> (Array a, Array a)
{-# NOINLINE op2p2 #-}
op2p2 (Array fptr1) (Array fptr2) op =
  unsafePerformIO . mask_ $ do
    (x,y) <-
      withForeignPtr fptr1 $ \ptr1 -> do
        withForeignPtr fptr2 $ \ptr2 -> do
          alloca $ \ptrInput1 -> do
            alloca $ \ptrInput2 -> do
              throwAFError =<< op ptrInput1 ptrInput2 ptr1 ptr2
              (,) <$> peek ptrInput1 <*> peek ptrInput2
    fptrA <- newForeignPtr af_release_array_finalizer x
    fptrB <- newForeignPtr af_release_array_finalizer y
    pure (Array fptrA, Array fptrB)

-- | Key/value variant of 'op2p2' used by sort-by-key operations. The input key
-- 'Array' is cast down to @s32@ before the C call (ArrayFire requires 32-bit
-- keys) and the resulting key 'Array' is cast back up to @s64@, releasing the
-- intermediate handles along the way.
op2p2kv
  :: Array Int
  -> Array a
  -> (Ptr AFArray -> Ptr AFArray -> AFArray -> AFArray -> IO AFErr)
  -> (Array Int, Array b)
{-# NOINLINE op2p2kv #-}
op2p2kv (Array fptr1) (Array fptr2) op =
  unsafePerformIO . mask_ $ do
    (x, y) <-
      withForeignPtr fptr1 $ \ptr1 ->
        withForeignPtr fptr2 $ \ptr2 -> do
          castedKey <- alloca $ \p -> do
            throwAFError =<< af_cast p ptr1 s32
            peek p
          alloca $ \ptrOutput1 ->
            alloca $ \ptrOutput2 -> do
              onException
                (throwAFError =<< op ptrOutput1 ptrOutput2 castedKey ptr2)
                (af_release_array_ffi castedKey)
              _ <- af_release_array_ffi castedKey
              outKey <- peek ptrOutput1
              outVal <- peek ptrOutput2
              finalKey <- alloca $ \p -> do
                onException
                  (throwAFError =<< af_cast p outKey s64)
                  (af_release_array_ffi outKey >> af_release_array_ffi outVal)
                peek p
              _ <- af_release_array_ffi outKey
              pure (finalKey, outVal)
    fptrA <- newForeignPtr af_release_array_finalizer x
    fptrB <- newForeignPtr af_release_array_finalizer y
    pure (Array fptrA, Array fptrB)

-- | Runs a C function that constructs a fresh 'Array' (taking no input
-- 'Array'), returning the result in 'IO'. The output pointer is zeroed before
-- the call so the finalizer is safe even if construction fails.
createArray'
  :: (Ptr AFArray -> IO AFErr)
  -> IO (Array a)
{-# NOINLINE createArray' #-}
createArray' op =
  mask_ $ do
    ptr <-
      alloca $ \ptrInput -> do
        zeroOutArray ptrInput
        throwAFError =<< op ptrInput
        peek ptrInput
    fptr <- newForeignPtr af_release_array_finalizer ptr
    pure (Array fptr)

-- | Pure counterpart of 'createArray'' for constructing an 'Array' from a C
-- function that takes no input 'Array'. The effect is hidden behind
-- 'unsafePerformIO'.
createArray
  :: (Ptr AFArray -> IO AFErr)
  -> Array a
{-# NOINLINE createArray #-}
createArray op =
  unsafePerformIO . mask_ $ do
    ptr <-
      alloca $ \ptrInput -> do
        zeroOutArray ptrInput
        throwAFError =<< op ptrInput
        peek ptrInput
    fptr <- newForeignPtr af_release_array_finalizer ptr
    pure (Array fptr)

-- | Runs a C function that constructs a 'Window' handle, attaching the
-- window-release finalizer to the result.
createWindow'
  :: (Ptr AFWindow -> IO AFErr)
  -> IO Window
createWindow' op =
  mask_ $ do
    ptr <-
      alloca $ \ptrInput -> do
        throwAFError =<< op ptrInput
        peek ptrInput
    fptr <- newForeignPtr af_release_window_finalizer ptr
    pure (Window fptr)

-- | Runs a C function against an existing 'Window' for its side effects,
-- returning unit.
opw
  :: Window
  -> (AFWindow -> IO AFErr)
  -> IO ()
opw (Window fptr) op
  = mask_ . withForeignPtr fptr $ (throwAFError <=< op)

-- | Runs a C function against an existing 'Window' that writes back a single
-- 'Storable' value, returning it.
opw1
  :: Storable a
  => Window
  -> (Ptr a -> AFWindow -> IO AFErr)
  -> IO a
{-# NOINLINE opw1 #-}
opw1 (Window fptr) op
  = mask_ . withForeignPtr fptr $ \ptr -> do
       alloca $ \p -> do
         throwAFError =<< op p ptr
         peek p

-- | Applies a C function that takes a single input 'Array' and produces a
-- single output 'Array'.
op1
  :: Array a
  -> (Ptr AFArray -> AFArray -> IO AFErr)
  -> Array b
{-# NOINLINE op1 #-}
op1 (Array fptr1) op =
  unsafePerformIO . mask_ $ do
    withForeignPtr fptr1 $ \ptr1 -> do
      ptr <-
        alloca $ \ptrInput -> do
          throwAFError =<< op ptrInput ptr1
          peek ptrInput
      fptr <- newForeignPtr af_release_array_finalizer ptr
      pure (Array fptr)

-- | Applies a C function that takes a single input 'Features' and produces a
-- new 'Features' handle.
op1f
  :: Features
  -> (Ptr AFFeatures -> AFFeatures -> IO AFErr)
  -> Features
{-# NOINLINE op1f #-}
op1f (Features x) op =
  unsafePerformIO . mask_ $ do
    withForeignPtr x $ \ptr1 -> do
      ptr <-
        alloca $ \ptrInput -> do
          throwAFError =<< op ptrInput ptr1
          peek ptrInput
      fptr <- newForeignPtr af_release_features ptr
      pure (Features fptr)

-- | Applies a C function that takes a single input 'RandomEngine' and produces
-- a new 'RandomEngine' handle, returned in 'IO'.
op1re
  :: RandomEngine
  -> (Ptr AFRandomEngine -> AFRandomEngine -> IO AFErr)
  -> IO RandomEngine
op1re (RandomEngine x) op = mask_ $
  withForeignPtr x $ \ptr1 -> do
    ptr <-
      alloca $ \ptrInput -> do
        throwAFError =<< op ptrInput ptr1
        peek ptrInput
    fptr <- newForeignPtr af_release_random_engine_finalizer ptr
    pure (RandomEngine fptr)

-- | Applies a C function that takes a single input 'Array' and produces both a
-- 'Storable' scalar and an output 'Array' (e.g. an operation returning a value
-- and its location).
op1b
  :: Storable b
  => Array a
  -> (Ptr AFArray -> Ptr b -> AFArray -> IO AFErr)
  -> (b, Array a)
{-# NOINLINE op1b #-}
op1b (Array fptr1) op =
  unsafePerformIO . mask_ $
    withForeignPtr fptr1 $ \ptr1 -> do
      (y,x) <-
        alloca $ \ptrInput1 -> do
          alloca $ \ptrInput2 -> do
            throwAFError =<< op ptrInput1 ptrInput2 ptr1
            (,) <$> peek ptrInput1 <*> peek ptrInput2
      fptr <- newForeignPtr af_release_array_finalizer y
      pure (x, Array fptr)

-- | Runs an 'AFErr'-returning C action purely for its side effects, throwing
-- on a non-success status.
afCall
  :: IO AFErr
  -> IO ()
afCall = mask_ . (throwAFError =<<)

-- | Loads an image from the given file path into a new 'Array'. The 'Bool'
-- flag selects whether the image is loaded in colour, and is marshalled to the
-- 'CBool' expected by the C function.
loadAFImage
  :: String
  -> Bool
  -> (Ptr AFArray -> CString -> CBool -> IO AFErr)
  -> IO (Array a)
loadAFImage s (fromIntegral . fromEnum -> b) op = mask_ $
  withCString s $ \cstr -> do
    p <- alloca $ \ptr -> do
      throwAFError =<< op ptr cstr b
      peek ptr
    fptr <- newForeignPtr af_release_array_finalizer p
    pure (Array fptr)

-- | Loads an image from the given file path into a new 'Array' in its native
-- format, without any colour-space conversion.
loadAFImageNative
  :: String
  -> (Ptr AFArray -> CString -> IO AFErr)
  -> IO (Array a)
loadAFImageNative s op = mask_ $
  withCString s $ \cstr -> do
    p <- alloca $ \ptr -> do
      throwAFError =<< op ptr cstr
      peek ptr
    fptr <- newForeignPtr af_release_array_finalizer p
    pure (Array fptr)

-- | Runs a C function that mutates an 'Array' in place, returning unit.
inPlace :: Array a -> (AFArray -> IO AFErr) -> IO ()
inPlace (Array fptr) op =
  mask_ . withForeignPtr fptr $ (throwAFError <=< op)

-- | Runs a C function that mutates a 'RandomEngine' in place, returning unit.
inPlaceEng :: RandomEngine -> (AFRandomEngine -> IO AFErr) -> IO ()
inPlaceEng (RandomEngine fptr) op =
  mask_ . withForeignPtr fptr $ (throwAFError <=< op)

-- | Runs a C function that writes back a single 'Storable' value through an
-- output pointer, returning that value in 'IO'.
afCall1
  :: Storable a
  => (Ptr a -> IO AFErr)
  -> IO a
afCall1 op =
  alloca $ \ptrInput -> do
    throwAFError =<< op ptrInput
    peek ptrInput

-- | Pure counterpart of 'afCall1' for reading back a single 'Storable' value.
-- The effect is hidden behind 'unsafePerformIO'.
afCall1'
  :: Storable a
  => (Ptr a -> IO AFErr)
  -> a
{-# NOINLINE afCall1' #-}
afCall1' op =
  unsafePerformIO . mask_ $ do
    alloca $ \ptrInput -> do
      throwAFError =<< op ptrInput
      peek ptrInput

-- | Note: We don't add a finalizer to 'Array' since the 'Features' finalizer frees 'Array'
-- under the hood.
featuresToArray
  :: Features
  -> (Ptr AFArray -> AFFeatures -> IO AFErr)
  -> Array a
{-# NOINLINE featuresToArray #-}
featuresToArray (Features fptr1) op =
  unsafePerformIO . mask_ $ do
    withForeignPtr fptr1 $ \ptr1 -> do
      alloca $ \ptrInput -> do
        throwAFError =<< op ptrInput ptr1
        alloca $ \retainedArray -> do
          throwAFError =<< af_retain_array retainedArray =<< peek ptrInput
          fptr <- newForeignPtr af_release_array_finalizer =<< peek retainedArray
          pure (Array fptr)

-- | Reads back a single 'Storable' scalar describing a 'Features' handle (for
-- example its feature count), hiding the effect behind 'unsafePerformIO'.
infoFromFeatures
  :: Storable a
  => Features
  -> (Ptr a -> AFFeatures -> IO AFErr)
  -> a
{-# NOINLINE infoFromFeatures #-}
infoFromFeatures (Features fptr1) op =
  unsafePerformIO . mask_ $ do
    withForeignPtr fptr1 $ \ptr1 -> do
      alloca $ \ptrInput -> do
        throwAFError =<< op ptrInput ptr1
        peek ptrInput

-- | Reads back a single 'Storable' scalar describing a 'RandomEngine' (for
-- example its seed or type), returning it in 'IO'.
infoFromRandomEngine
  :: Storable a
  => RandomEngine
  -> (Ptr a -> AFRandomEngine -> IO AFErr)
  -> IO a
infoFromRandomEngine (RandomEngine fptr1) op =
  mask_ $ do
    withForeignPtr fptr1 $ \ptr1 -> do
      alloca $ \ptrInput -> do
        throwAFError =<< op ptrInput ptr1
        peek ptrInput

-- | Saves an 'Array' to the given file path using the supplied C function.
afSaveImage
  :: Array b
  -> String
  -> (CString -> AFArray -> IO AFErr)
  -> IO ()
afSaveImage (Array fptr1) str op =
  withCString str $ \cstr ->
    withForeignPtr fptr1 $
      throwAFError <=< op cstr

-- | Reads back a single 'Storable' scalar describing an 'Array' (for example a
-- dimension or count), hiding the effect behind 'unsafePerformIO'.
infoFromArray
  :: Storable a
  => Array b
  -> (Ptr a -> AFArray -> IO AFErr)
  -> a
{-# NOINLINE infoFromArray #-}
infoFromArray (Array fptr1) op =
  unsafePerformIO . mask_ $ do
    withForeignPtr fptr1 $ \ptr1 -> do
      alloca $ \ptrInput -> do
        throwAFError =<< op ptrInput ptr1
        peek ptrInput

-- | Like 'infoFromArray', but reads back a pair of 'Storable' scalars from a
-- single input 'Array'.
infoFromArray2
  :: (Storable a, Storable b)
  => Array arr
  -> (Ptr a -> Ptr b -> AFArray -> IO AFErr)
  -> (a,b)
{-# NOINLINE infoFromArray2 #-}
infoFromArray2 (Array fptr1) op =
  unsafePerformIO . mask_ $ do
    withForeignPtr fptr1 $ \ptr1 -> do
      alloca $ \ptrInput1 -> do
        alloca $ \ptrInput2 -> do
          throwAFError =<< op ptrInput1 ptrInput2 ptr1
          (,) <$> peek ptrInput1 <*> peek ptrInput2

-- | Like 'infoFromArray2', but reads back a pair of 'Storable' scalars derived
-- from two input 'Array's.
infoFromArray22
  :: (Storable a, Storable b)
  => Array arr
  -> Array arr
  -> (Ptr a -> Ptr b -> AFArray -> AFArray -> IO AFErr)
  -> (a,b)
{-# NOINLINE infoFromArray22 #-}
infoFromArray22 (Array fptr1) (Array fptr2) op =
  unsafePerformIO . mask_ $ do
    withForeignPtr fptr1 $ \ptr1 -> do
     withForeignPtr fptr2 $ \ptr2 -> do
      alloca $ \ptrInput1 -> do
        alloca $ \ptrInput2 -> do
          throwAFError =<< op ptrInput1 ptrInput2 ptr1 ptr2
          (,) <$> peek ptrInput1 <*> peek ptrInput2

-- | Like 'infoFromArray', but reads back three 'Storable' scalars from a
-- single input 'Array'.
infoFromArray3
  :: (Storable a, Storable b, Storable c)
  => Array arr
  -> (Ptr a -> Ptr b -> Ptr c -> AFArray -> IO AFErr)
  -> (a,b,c)
{-# NOINLINE infoFromArray3 #-}
infoFromArray3 (Array fptr1) op =
  unsafePerformIO . mask_ $
    withForeignPtr fptr1 $ \ptr1 -> do
      alloca $ \ptrInput1 -> do
        alloca $ \ptrInput2 -> do
          alloca $ \ptrInput3 -> do
            throwAFError =<< op ptrInput1 ptrInput2 ptrInput3 ptr1
            (,,) <$> peek ptrInput1
                 <*> peek ptrInput2
                 <*> peek ptrInput3

-- | Like 'infoFromArray', but reads back four 'Storable' scalars from a single
-- input 'Array' (for example all four dimensions).
infoFromArray4
  :: (Storable a, Storable b, Storable c, Storable d)
  => Array arr
  -> (Ptr a -> Ptr b -> Ptr c -> Ptr d -> AFArray -> IO AFErr)
  -> (a,b,c,d)
{-# NOINLINE infoFromArray4 #-}
infoFromArray4 (Array fptr1) op =
  unsafePerformIO . mask_ $
    withForeignPtr fptr1 $ \ptr1 ->
      alloca $ \ptrInput1 ->
        alloca $ \ptrInput2 ->
          alloca $ \ptrInput3 ->
            alloca $ \ptrInput4 -> do
              throwAFError =<< op ptrInput1 ptrInput2 ptrInput3 ptrInput4 ptr1
              (,,,) <$> peek ptrInput1
                    <*> peek ptrInput2
                    <*> peek ptrInput3
                    <*> peek ptrInput4

foreign import ccall unsafe "zeroOutArray"
  zeroOutArray :: Ptr AFArray -> IO ()
