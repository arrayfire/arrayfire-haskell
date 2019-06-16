{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE FlexibleContexts    #-}
module ArrayFire.Data where

import Control.Exception
import Control.Monad
import Data.Complex
import Data.Proxy
import Data.Word
import Foreign.ForeignPtr
import Foreign.Marshal         hiding (void)
import Foreign.Storable
import GHC.Int
import GHC.TypeLits
import System.IO.Unsafe

import ArrayFire.Exception
import ArrayFire.FFI
import ArrayFire.Types
import ArrayFire.Internal.Data

constant
  :: forall dims
   . (Dims dims)
  => Double -> IO (Array Double)
constant val = do
  ptr <- alloca $ \ptrPtr -> mask_ $ do
    dimArray <- newArray dimt
    throwAFError =<< af_constant ptrPtr val n dimArray typ
    free dimArray
    peek ptrPtr
  Array <$>
    newForeignPtr
      af_release_array_finalizer
        ptr
      where
        n = fromIntegral (length dimt)
        dimt = toDims (Proxy @ dims)
        typ = afType (Proxy @ Double)

constantComplex
  :: forall dims
   . (Dims dims)
  => Complex Double
  -> IO (Array (Complex Double))
constantComplex val = do
  ptr <- alloca $ \ptrPtr -> mask_ $ do
    dimArray <- newArray dimt
    throwAFError =<< af_constant_complex ptrPtr (realPart val) (imagPart val) n dimArray typ
    free dimArray
    peek ptrPtr
  Array <$>
    newForeignPtr
      af_release_array_finalizer
        ptr
      where
        n = fromIntegral (length dimt)
        dimt = toDims (Proxy @ dims)
        typ = afType (Proxy @ (Complex Double))

constantLong
  :: forall dims
   . (Dims dims)
  => Int64
  -> IO (Array Int64)
constantLong val = do
  ptr <- alloca $ \ptrPtr -> mask_ $ do
    dimArray <- newArray dimt
    throwAFError =<< af_constant_long ptrPtr (fromIntegral val) n dimArray
    free dimArray
    peek ptrPtr
  Array <$>
    newForeignPtr
      af_release_array_finalizer
        ptr
      where
        n = fromIntegral (length dimt)
        dimt = toDims (Proxy @ dims)

constantULong
  :: forall dims
   . (Dims dims)
  => Word64
  -> IO (Array Word64)
constantULong val = do
  ptr <- alloca $ \ptrPtr -> mask_ $ do
    dimArray <- newArray dimt
    throwAFError =<< af_constant_ulong ptrPtr (fromIntegral val) n dimArray
    free dimArray
    peek ptrPtr
  Array <$>
    newForeignPtr
      af_release_array_finalizer
        ptr
      where
        n = fromIntegral (length dimt)
        dimt = toDims (Proxy @ dims)

range
  :: forall dims a
   . (Dims dims, AFType a)
  => Int
  -> IO (Array a)
range k = do
  ptr <- alloca $ \ptrPtr -> mask_ $ do
    dimArray <- newArray dimt
    throwAFError =<< af_range ptrPtr n dimArray k typ
    free dimArray

    peek ptrPtr
  Array <$>
    newForeignPtr
      af_release_array_finalizer
        ptr
      where
        n = fromIntegral (length dimt)
        dimt = toDims (Proxy @ dims)
        typ = afType (Proxy @ a)

iota
  :: forall dims tdims a
   . (Dims dims, Dims tdims, AFType a, KnownNat tdims)
  => IO (Array a)
iota = do
  ptr <- alloca $ \ptrPtr -> mask_ $ do
    dimArray <- newArray dimt
    tdimArray <- newArray tdimt
    throwAFError =<< af_iota ptrPtr n dimArray tn tdimArray typ
    free dimArray
    peek ptrPtr
  Array <$>
    newForeignPtr
      af_release_array_finalizer
        ptr
      where
        n = fromIntegral (length dimt)
        dimt = toDims (Proxy @ dims)
        tn = fromIntegral (length dimt)
        tdimt = toDims (Proxy @ tdims)
        typ = afType (Proxy @ a)

identity
  :: forall dims a
   . (Dims dims, AFType a)
  => Array a
identity = unsafePerformIO . mask_ $ do
  ptr <- alloca $ \ptrPtr -> mask_ $ do
    dimArray <- newArray dimt
    throwAFError =<< af_identity ptrPtr n dimArray typ
    free dimArray
    peek ptrPtr
  Array <$>
    newForeignPtr
      af_release_array_finalizer
        ptr
      where
        n = fromIntegral (length dimt)
        dimt = toDims (Proxy @ dims)
        typ = afType (Proxy @ a)

diagCreate
  :: AFType (a :: *)
  => Array a
  -> Int
  -> Array a
diagCreate x n =
  x `op1` (\p a -> af_diag_create p a n)

diagExtract
  :: AFType (a :: *)
  => Array a
  -> Int
  -> Array a
diagExtract x n =
  x `op1` (\p a -> af_diag_extract p a n)

join
  :: Int
  -> Array (a :: *)
  -> Array a
  -> Array a
join n arr1 arr2 = op2 arr1 arr2 (\p a b -> af_join p n a b)

joinMany
  :: Int
  -> [Array a]
  -> Array a
joinMany n arrays = unsafePerformIO . mask_ $ do
  fptrs <- forM arrays $ \(Array fptr) -> pure fptr
  newPtr <-
    alloca $ \fPtrsPtr -> do
      forM_ fptrs $ \fptr ->
        withForeignPtr fptr (poke fPtrsPtr)
      alloca $ \aPtr -> do
        throwAFError =<< af_join_many aPtr n nArrays fPtrsPtr
        peek aPtr
  Array <$>
    newForeignPtr af_release_array_finalizer newPtr
  where
    nArrays = fromIntegral (length arrays)

tile
  :: Array (a :: *)
  -> Int
  -> Int
  -> Int
  -> Int
  -> Array a
tile a (fromIntegral -> x) (fromIntegral -> y) (fromIntegral -> z) (fromIntegral -> w) =
  a `op1` (\p k -> af_tile p k x y z w)

reorder
  :: Array (a :: *)
  -> Int
  -> Int
  -> Int
  -> Int
  -> Array a
reorder a (fromIntegral -> x) (fromIntegral -> y) (fromIntegral -> z) (fromIntegral -> w) =
  a `op1` (\p k -> af_tile p k x y z w)

shift
  :: Array (a :: *)
  -> Int
  -> Int
  -> Int
  -> Int
  -> Array a
shift a (fromIntegral -> x) (fromIntegral -> y) (fromIntegral -> z) (fromIntegral -> w) =
  a `op1` (\p k -> af_shift p k x y z w)

moddims
  :: forall a dims
   . Dims dims
  => Array (a :: *)
  -> Array a
moddims (Array fptr) =
  unsafePerformIO . mask_ . withForeignPtr fptr $ \ptr -> do
    newPtr <- alloca $ \aPtr -> do
      dimsPtr <- newArray dims
      throwAFError =<< af_moddims aPtr ptr n dimsPtr
      free dimsPtr
      peek aPtr
    Array <$> newForeignPtr af_release_array_finalizer newPtr
  where
    dims = toDims (Proxy @ dims)
    n = fromIntegral (length dims)

flat
  :: Array (a :: *)
  -> Array a
flat = (`op1` af_flat)

flip
  :: Array (a :: *)
  -> Int
  -> Array a
flip a (fromIntegral -> dim) =
  a `op1` (\p k -> af_flip p k dim)

lower
  :: Array (a :: *)
  -> Bool
  -> Array a
lower a b =
  a `op1` (\p k -> af_lower p k b)

upper
  :: Array (a :: *)
  -> Bool
  -> Array a
upper a b =
  a `op1` (\p k -> af_upper p k b)

select
  :: Array (a :: *)
  -> Array a
  -> Array a
  -> Array a
select a b c = op3 a b c af_select

selectScalarR
  :: Array (a :: *)
  -> Array a
  -> Double
  -> Array a
selectScalarR a b c = op2 a b (\p w x -> af_select_scalar_r p w x c)

selectScalarL
  :: Array (a :: *)
  -> Double
  -> Array a
  -> Array a
selectScalarL a n b = op2 a b (\p w x -> af_select_scalar_l p w n x)

-- af_err af_replace(af_array a, const af_array cond, const af_array b);
-- af_err af_replace_scalar(af_array a, const af_array cond, const double b);
