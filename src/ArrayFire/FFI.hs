{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
--------------------------------------------------------------------------------
-- |
-- Module      : ArrayFire.FFI
-- Copyright   : David Johnson (c) 2019-2020
-- License     : BSD 3
-- Maintainer  : David Johnson <djohnson.m@gmail.com>
-- Stability   : Experimental
-- Portability : GHC
--
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

op3
  :: Array b
  -> Array a
  -> Array a
  -> (Ptr AFArray -> AFArray -> AFArray -> AFArray -> IO AFErr)
  -> Array a
{-# NOINLINE op3 #-}
op3 (Array fptr1) (Array fptr2) (Array fptr3) op =
  unsafePerformIO $ do
    withForeignPtr fptr1 $ \ptr1 ->
      withForeignPtr fptr2 $ \ptr2 -> do
         withForeignPtr fptr3 $ \ptr3 -> do
           ptr <-
             alloca $ \ptrInput -> do
               throwAFError =<< op ptrInput ptr1 ptr2 ptr3
               peek ptrInput
           fptr <- newForeignPtr af_release_array_finalizer ptr
           pure (Array fptr)

op3Int
  :: Array a
  -> Array Int32
  -> Array Int32
  -> (Ptr AFArray -> AFArray -> AFArray -> AFArray -> IO AFErr)
  -> Array a
{-# NOINLINE op3Int #-}
op3Int (Array fptr1) (Array fptr2) (Array fptr3) op =
  unsafePerformIO $ do
    withForeignPtr fptr1 $ \ptr1 ->
      withForeignPtr fptr2 $ \ptr2 -> do
         withForeignPtr fptr3 $ \ptr3 -> do
           ptr <-
             alloca $ \ptrInput -> do
               throwAFError =<< op ptrInput ptr1 ptr2 ptr3
               peek ptrInput
           fptr <- newForeignPtr af_release_array_finalizer ptr
           pure (Array fptr)

op2
  :: Array b
  -> Array a
  -> (Ptr AFArray -> AFArray -> AFArray -> IO AFErr)
  -> Array a
{-# NOINLINE op2 #-}
op2 (Array fptr1) (Array fptr2) op =
  unsafePerformIO $ do
    withForeignPtr fptr1 $ \ptr1 ->
      withForeignPtr fptr2 $ \ptr2 -> do
        ptr <-
          alloca $ \ptrInput -> do
            throwAFError =<< op ptrInput ptr1 ptr2
            peek ptrInput
        fptr <- newForeignPtr af_release_array_finalizer ptr
        pure (Array fptr)

op2bool
  :: Array b
  -> Array a
  -> (Ptr AFArray -> AFArray -> AFArray -> IO AFErr)
  -> Array CBool
{-# NOINLINE op2bool #-}
op2bool (Array fptr1) (Array fptr2) op =
  unsafePerformIO $ do
    withForeignPtr fptr1 $ \ptr1 ->
      withForeignPtr fptr2 $ \ptr2 -> do
        ptr <-
          alloca $ \ptrInput -> do
            throwAFError =<< op ptrInput ptr1 ptr2
            peek ptrInput
        fptr <- newForeignPtr af_release_array_finalizer ptr
        pure (Array fptr)


op2p
  :: Array a
  -> (Ptr AFArray -> Ptr AFArray -> AFArray -> IO AFErr)
  -> (Array a, Array a)
{-# NOINLINE op2p #-}
op2p (Array fptr1) op =
  unsafePerformIO $ do
    (x,y) <- withForeignPtr fptr1 $ \ptr1 -> do
        alloca $ \ptrInput1 -> do
          alloca $ \ptrInput2 -> do
            throwAFError =<< op ptrInput1 ptrInput2 ptr1
            (,) <$> peek ptrInput1 <*> peek ptrInput2
    fptrA <- newForeignPtr af_release_array_finalizer x
    fptrB <- newForeignPtr af_release_array_finalizer y
    pure (Array fptrA, Array fptrB)

op3p
  :: Array a
  -> (Ptr AFArray -> Ptr AFArray -> Ptr AFArray -> AFArray -> IO AFErr)
  -> (Array a, Array a, Array a)
{-# NOINLINE op3p #-}
op3p (Array fptr1) op =
  unsafePerformIO $ do
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

op3p1
  :: Storable b
  => Array a
  -> (Ptr AFArray -> Ptr AFArray -> Ptr AFArray -> Ptr b -> AFArray -> IO AFErr)
  -> (Array a, Array a, Array a, b)
{-# NOINLINE op3p1 #-}
op3p1 (Array fptr1) op =
  unsafePerformIO $ do
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

op2p2
  :: Array a
  -> Array a
  -> (Ptr AFArray -> Ptr AFArray -> AFArray -> AFArray -> IO AFErr)
  -> (Array a, Array a)
{-# NOINLINE op2p2 #-}
op2p2 (Array fptr1) (Array fptr2) op =
  unsafePerformIO $ do
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

opw
  :: Window
  -> (AFWindow -> IO AFErr)
  -> IO ()
opw (Window fptr) op
  = mask_ . withForeignPtr fptr $ (throwAFError <=< op)

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

op1d
  :: Array a
  -> (Ptr AFArray -> AFArray -> IO AFErr)
  -> Array b
{-# NOINLINE op1d #-}
op1d (Array fptr1) op =
  unsafePerformIO $ do
    withForeignPtr fptr1 $ \ptr1 -> do
      ptr <-
        alloca $ \ptrInput -> do
          throwAFError =<< op ptrInput ptr1
          peek ptrInput
      fptr <- newForeignPtr af_release_array_finalizer ptr
      pure (Array fptr)


op1
  :: Array a
  -> (Ptr AFArray -> AFArray -> IO AFErr)
  -> Array a
{-# NOINLINE op1 #-}
op1 (Array fptr1) op =
  unsafePerformIO $ do
    withForeignPtr fptr1 $ \ptr1 -> do
      ptr <-
        alloca $ \ptrInput -> do
          throwAFError =<< op ptrInput ptr1
          peek ptrInput
      fptr <- newForeignPtr af_release_array_finalizer ptr
      pure (Array fptr)

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

op1b
  :: Storable b
  => Array a
  -> (Ptr AFArray -> Ptr b -> AFArray -> IO AFErr)
  -> (b, Array a)
{-# NOINLINE op1b #-}
op1b (Array fptr1) op =
  unsafePerformIO $
    withForeignPtr fptr1 $ \ptr1 -> do
      (y,x) <-
        alloca $ \ptrInput1 -> do
          alloca $ \ptrInput2 -> do
            throwAFError =<< op ptrInput1 ptrInput2 ptr1
            (,) <$> peek ptrInput1 <*> peek ptrInput2
      fptr <- newForeignPtr af_release_array_finalizer y
      pure (x, Array fptr)

afCall
  :: IO AFErr
  -> IO ()
afCall = mask_ . (throwAFError =<<)

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

inPlace :: Array a -> (AFArray -> IO AFErr) -> IO ()
inPlace (Array fptr) op =
  mask_ . withForeignPtr fptr $ (throwAFError <=< op)

inPlaceEng :: RandomEngine -> (AFRandomEngine -> IO AFErr) -> IO ()
inPlaceEng (RandomEngine fptr) op =
  mask_ . withForeignPtr fptr $ (throwAFError <=< op)

afCall1
  :: Storable a
  => (Ptr a -> IO AFErr)
  -> IO a
afCall1 op =
  alloca $ \ptrInput -> do
    throwAFError =<< op ptrInput
    peek ptrInput

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

infoFromFeatures
  :: Storable a
  => Features
  -> (Ptr a -> AFFeatures -> IO AFErr)
  -> a
{-# NOINLINE infoFromFeatures #-}
infoFromFeatures (Features fptr1) op =
  unsafePerformIO $ do
    withForeignPtr fptr1 $ \ptr1 -> do
      alloca $ \ptrInput -> do
        throwAFError =<< op ptrInput ptr1
        peek ptrInput

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

afSaveImage
  :: Array b
  -> String
  -> (CString -> AFArray -> IO AFErr)
  -> IO ()
afSaveImage (Array fptr1) str op =
  withCString str $ \cstr ->
    withForeignPtr fptr1 $
      throwAFError <=< op cstr

infoFromArray
  :: Storable a
  => Array b
  -> (Ptr a -> AFArray -> IO AFErr)
  -> a
{-# NOINLINE infoFromArray #-}
infoFromArray (Array fptr1) op =
  unsafePerformIO $ do
    withForeignPtr fptr1 $ \ptr1 -> do
      alloca $ \ptrInput -> do
        throwAFError =<< op ptrInput ptr1
        peek ptrInput

infoFromArray2
  :: (Storable a, Storable b)
  => Array arr
  -> (Ptr a -> Ptr b -> AFArray -> IO AFErr)
  -> (a,b)
{-# NOINLINE infoFromArray2 #-}
infoFromArray2 (Array fptr1) op =
  unsafePerformIO $ do
    withForeignPtr fptr1 $ \ptr1 -> do
      alloca $ \ptrInput1 -> do
        alloca $ \ptrInput2 -> do
          throwAFError =<< op ptrInput1 ptrInput2 ptr1
          (,) <$> peek ptrInput1 <*> peek ptrInput2

infoFromArray22
  :: (Storable a, Storable b)
  => Array arr
  -> Array arr
  -> (Ptr a -> Ptr b -> AFArray -> AFArray -> IO AFErr)
  -> (a,b)
{-# NOINLINE infoFromArray22 #-}
infoFromArray22 (Array fptr1) (Array fptr2) op =
  unsafePerformIO $ do
    withForeignPtr fptr1 $ \ptr1 -> do
     withForeignPtr fptr2 $ \ptr2 -> do
      alloca $ \ptrInput1 -> do
        alloca $ \ptrInput2 -> do
          throwAFError =<< op ptrInput1 ptrInput2 ptr1 ptr2
          (,) <$> peek ptrInput1 <*> peek ptrInput2

infoFromArray3
  :: (Storable a, Storable b, Storable c)
  => Array arr
  -> (Ptr a -> Ptr b -> Ptr c -> AFArray -> IO AFErr)
  -> (a,b,c)
{-# NOINLINE infoFromArray3 #-}
infoFromArray3 (Array fptr1) op =
  unsafePerformIO $
    withForeignPtr fptr1 $ \ptr1 -> do
      alloca $ \ptrInput1 -> do
        alloca $ \ptrInput2 -> do
          alloca $ \ptrInput3 -> do
            throwAFError =<< op ptrInput1 ptrInput2 ptrInput3 ptr1
            (,,) <$> peek ptrInput1
                 <*> peek ptrInput2
                 <*> peek ptrInput3

infoFromArray4
  :: (Storable a, Storable b, Storable c, Storable d)
  => Array arr
  -> (Ptr a -> Ptr b -> Ptr c -> Ptr d -> AFArray -> IO AFErr)
  -> (a,b,c,d)
{-# NOINLINE infoFromArray4 #-}
infoFromArray4 (Array fptr1) op =
  unsafePerformIO $
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
