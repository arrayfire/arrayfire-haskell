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
--------------------------------------------------------------------------------
-- |
-- Module      : ArrayFire.Data
-- Copyright   : David Johnson (c) 2019-2020
-- License     : BSD 3
-- Maintainer  : David Johnson <djohnson.m@gmail.com>
-- Stability   : Experimental
-- Portability : GHC
--
--------------------------------------------------------------------------------
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
import System.IO.Unsafe

import ArrayFire.Exception
import ArrayFire.FFI
import ArrayFire.Types
import ArrayFire.Internal.Data

constant :: [Int] -> Double -> Array Double
constant dims val =
  unsafePerformIO . mask_ $ do
    ptr <- alloca $ \ptrPtr ->
      withArray (fromIntegral <$> dims) $ \dimArray -> do
        throwAFError =<< af_constant ptrPtr val n dimArray typ
        peek ptrPtr
    Array <$>
      newForeignPtr
        af_release_array_finalizer
          ptr
      where
        n = fromIntegral (length dims)
        typ = afType (Proxy @ Double)

constantComplex
  :: [Int]
  -> Complex Double
  -> Array (Complex Double)
constantComplex dims val = unsafePerformIO . mask_ $ do
  ptr <- alloca $ \ptrPtr -> mask_ $ do
    withArray (fromIntegral <$> dims) $ \dimArray -> do
      throwAFError =<< af_constant_complex ptrPtr (realPart val) (imagPart val) n dimArray typ
      peek ptrPtr
  Array <$>
    newForeignPtr
      af_release_array_finalizer
        ptr
      where
        n = fromIntegral (length dims)
        typ = afType (Proxy @ (Complex Double))

constantLong
  :: [Int]
  -> Int64
  -> Array Int64
constantLong dims val = unsafePerformIO . mask_ $ do
  ptr <- alloca $ \ptrPtr ->
    withArray (fromIntegral <$> dims) $ \dimArray -> do
      throwAFError =<< af_constant_long ptrPtr (fromIntegral val) n dimArray
      peek ptrPtr
  Array <$>
    newForeignPtr
      af_release_array_finalizer
        ptr
      where
        n = fromIntegral (length dims)

constantULong
  :: [Int]
  -> Word64
  -> Array Word64
constantULong dims val = unsafePerformIO . mask_ $ do
  ptr <- alloca $ \ptrPtr -> mask_ $ do
    withArray (fromIntegral <$> dims) $ \dimArray -> do
      throwAFError =<< af_constant_ulong ptrPtr (fromIntegral val) n dimArray
      peek ptrPtr
  Array <$>
    newForeignPtr
      af_release_array_finalizer
        ptr
      where
        n = fromIntegral (length dims)

range
  :: forall a
   . AFType a
  => [Int]
  -> Int
  -> IO (Array a)
range dims (fromIntegral -> k) = do
  ptr <- alloca $ \ptrPtr -> mask_ $ do
    withArray (fromIntegral <$> dims) $ \dimArray -> do
      throwAFError =<< af_range ptrPtr n dimArray k typ
      peek ptrPtr
  Array <$>
    newForeignPtr
      af_release_array_finalizer
        ptr
      where
        n = fromIntegral (length dims)
        typ = afType (Proxy @ a)

iota
  :: forall a . AFType a
  => [Int] -> [Int] -> IO (Array a)
iota dims tdims = do
  ptr <- alloca $ \ptrPtr -> mask_ $
    withArray (fromIntegral <$> dims) $ \dimArray ->
      withArray (fromIntegral <$> tdims) $ \tdimArray -> do
        throwAFError =<< af_iota ptrPtr n dimArray tn tdimArray typ
        peek ptrPtr
  Array <$>
    newForeignPtr
      af_release_array_finalizer
        ptr
      where
        n = fromIntegral (length dims)
        tn = fromIntegral (length tdims)
        typ = afType (Proxy @ a)

identity :: forall a . AFType a => [Int] -> Array a
identity dims = unsafePerformIO . mask_ $ do
  ptr <- alloca $ \ptrPtr -> mask_ $ do
    withArray (fromIntegral <$> dims) $ \dimArray -> do
      throwAFError =<< af_identity ptrPtr n dimArray typ
      peek ptrPtr
  Array <$>
    newForeignPtr
      af_release_array_finalizer
        ptr
      where
        n = fromIntegral (length dims)
        typ = afType (Proxy @ a)

diagCreate
  :: AFType (a :: *)
  => Array a
  -> Int
  -> Array a
diagCreate x (fromIntegral -> n) =
  x `op1` (\p a -> af_diag_create p a n)

diagExtract
  :: AFType (a :: *)
  => Array a
  -> Int
  -> Array a
diagExtract x (fromIntegral -> n) =
  x `op1` (\p a -> af_diag_extract p a n)

join
  :: Int
  -> Array (a :: *)
  -> Array a
  -> Array a
join (fromIntegral -> n) arr1 arr2 = op2 arr1 arr2 (\p a b -> af_join p n a b)

joinMany
  :: Int
  -> [Array a]
  -> Array a
joinMany (fromIntegral -> n) arrays = unsafePerformIO . mask_ $ do
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
  :: forall a
   . [Int]
  -> Array (a :: *)
  -> Array a
moddims dims (Array fptr) =
  unsafePerformIO . mask_ . withForeignPtr fptr $ \ptr -> do
    newPtr <- alloca $ \aPtr -> do
      withArray (fromIntegral <$> dims) $ \dimsPtr -> do
        throwAFError =<< af_moddims aPtr ptr n dimsPtr
        peek aPtr
    Array <$> newForeignPtr af_release_array_finalizer newPtr
  where
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
lower a (fromIntegral . fromEnum -> b) =
  a `op1` (\p k -> af_lower p k b)

upper
  :: Array (a :: *)
  -> Bool
  -> Array a
upper a (fromIntegral . fromEnum -> b) =
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
