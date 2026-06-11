--------------------------------------------------------------------------------
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE KindSignatures      #-}
--------------------------------------------------------------------------------
-- |
-- Module      : ArrayFire.Array
-- Copyright   : David Johnson (c) 2019-2026
-- License     : BSD 3
-- Maintainer  : David Johnson <code@dmj.io>
-- Stability   : Experimental
-- Portability : GHC
--
-- Functions for constructing and querying metadata from 'Array'
--
-- @
-- module Main where
--
-- import ArrayFire
--
-- main :: 'IO' ()
-- main = 'print' (matrix \@'Double' (2,2) [ [1..], [1..] ])
-- @
--
-- @
-- ArrayFire Array
-- [2 2 1 1]
--     1.0000     1.0000
--     2.0000     2.0000
-- @
--------------------------------------------------------------------------------
module ArrayFire.Array where

import           Control.Exception
import           Control.Monad
import           Data.Proxy
import           Data.Vector.Storable       hiding (mapM_, take, concat, concatMap)
import qualified Data.Vector.Storable       as V
import           Foreign.ForeignPtr
import           Foreign.Marshal            hiding (void)
import           Foreign.Ptr
import           Foreign.Storable

import           System.IO.Unsafe

import           ArrayFire.Exception
import           ArrayFire.FFI
import           ArrayFire.Util
import           ArrayFire.Internal.Array
import           ArrayFire.Internal.Defines
import           ArrayFire.Internal.Types

-- | Smart constructor for creating a scalar 'Array'
--
-- >>> scalar @Double 2.0
-- ArrayFire Array
-- [1 1 1 1]
--    2.0000
scalar :: AFType a => a -> Array a
scalar x = mkArray [1] [x]

-- | Smart constructor for creating a vector 'Array'
--
-- >>> vector @Double 10 [1..]
-- ArrayFire Array
-- [10 1 1 1]
--     1.0000
--     2.0000
--     3.0000
--     4.0000
--     5.0000
--     6.0000
--     7.0000
--     8.0000
--     9.0000
--    10.0000
vector :: AFType a => Int -> [a] -> Array a
vector n = mkArray [n] . take n

-- | Construct an 'Array' from a flat list with explicit dimensions.
--
-- Dimensions are in column-major order (first dim varies fastest).
-- Prefer 'fromVector' when data is already in a 'Data.Vector.Storable.Vector'
-- to avoid the intermediate list allocation.
--
-- >>> fromList [2,3] [1..6 :: Double]
-- ArrayFire Array
-- [2 3 1 1]
--     1.0000     3.0000     5.0000
--     2.0000     4.0000     6.0000
fromList :: AFType a => [Int] -> [a] -> Array a
fromList = mkArray

-- | Smart constructor for creating a matrix 'Array'
--
-- >>> A.matrix @Double (3,2) [[1,2,3],[4,5,6]]
-- ArrayFire Array
-- [3 2 1 1]
--    1.0000     4.0000
--    2.0000     5.0000
--    3.0000     6.0000
--
matrix :: AFType a => (Int,Int) -> [[a]] -> Array a
matrix (x,y)
  = mkArray [x,y]
  . concat
  . fmap (take x)
  . take y

-- | Smart constructor for creating a cubic 'Array'
--
-- >>> cube @Double (2,2,2) [[[2,2],[2,2]],[[2,2],[2,2]]]
--
-- @
-- ArrayFire Array
-- [2 2 2 1]
--    2.0000     2.0000
--    2.0000     2.0000
--
--    2.0000     2.0000
--    2.0000     2.0000
-- @
cube :: AFType a => (Int,Int,Int) -> [[[a]]] -> Array a
cube (x,y,z)
  = mkArray [x,y,z]
  . concat
  . fmap concat
  . fmap (take y)
  . (fmap . fmap . take) x
  . take z

-- | Smart constructor for creating a tensor 'Array'
--
-- >>> tensor @Double (2,2,2,2) [[[[2,2],[2,2]],[[2,2],[2,2]]], [[[2,2],[2,2]],[[2,2],[2,2]]]]
--
-- @
-- ArrayFire Array
-- [2 2 2 2]
--     2.0000     2.0000
--     2.0000     2.0000
--
--     2.0000     2.0000
--     2.0000     2.0000
--
--
--     2.0000     2.0000
--     2.0000     2.0000
--
--     2.0000     2.0000
--     2.0000     2.0000
-- @
tensor :: AFType a => (Int,Int,Int,Int) -> [[[[a]]]] -> Array a
tensor (w,x,y,z)
  = mkArray [w,x,y,z]
  . concat
  . fmap concat
  . (fmap . fmap) concat
  . (fmap . fmap . fmap . take) w
  . (fmap . fmap . take) x
  . (fmap . take) y
  . take z

-- | Internal function for 'Array' construction
--
-- >>> mkArray @Double [10] [1.0 .. 10.0]
-- ArrayFire Array
-- [10 1 1 1]
--     1.0000
--     2.0000
--     3.0000
--     4.0000
--     5.0000
--     6.0000
--     7.0000
--     8.0000
--     9.0000
--    10.0000
mkArray
  :: forall array
   . AFType array
  => [Int]
  -- ^ Dimensions
  -> [array]
  -- ^ Array elements
  -> Array array
  -- ^ Returned array
{-# NOINLINE mkArray #-}
mkArray dims xs =
  unsafePerformIO . mask_ $ do
    let ndims = fromIntegral (Prelude.length dims)
    calloca $ \arrayPtr -> do
      dimsPtr <- newArray (DimT . fromIntegral <$> dims)
      if size == 0
        then onException
               (do throwAFError =<< af_create_handle arrayPtr ndims dimsPtr dType
                   free dimsPtr)
               (free dimsPtr)
        else do
          when (Prelude.length (Prelude.take size xs) < size) $ do
            free dimsPtr
            let msg = "Invalid elements provided. "
                 <> "Expected "
                 <> show size
                 <> " elements received "
                 <> show (Prelude.length xs)
            throwIO (AFException SizeError 203 msg)
          dataPtr <- castPtr <$> newArray (Prelude.take size xs)
          onException
            (do throwAFError =<< af_create_array arrayPtr dataPtr ndims dimsPtr dType
                free dataPtr >> free dimsPtr)
            (free dataPtr >> free dimsPtr)
      arr <- peek arrayPtr
      Array <$> newForeignPtr af_release_array_finalizer arr
    where
      size  = Prelude.product dims
      dType = afType (Proxy @array)

-- | Constructs an 'Array' from a 'Storable' 'Vector', avoiding the intermediate list allocation of 'mkArray'.
--
-- The vector's contiguous buffer is handed straight to @af_create_array@, which
-- copies it into the 'Array' (and uploads to device memory on GPU backends), so
-- no intermediate Haskell list is built.
-- Throws 'AFException' if the vector length does not match the product of the given dimensions.
--
-- >>> fromVector @Double [3] (Data.Vector.Storable.fromList [1,2,3])
-- ArrayFire Array
-- [3 1 1 1]
--     1.0000
--     2.0000
--     3.0000
fromVector
  :: forall a
   . AFType a
  => [Int]
  -- ^ Dimensions
  -> Vector a
  -- ^ Source storable vector
  -> Array a
{-# NOINLINE fromVector #-}
fromVector dims vec =
  unsafePerformIO . mask_ $ do
    let size  = Prelude.product dims
        ndims = fromIntegral (Prelude.length dims)
        dType = afType (Proxy @a)
    when (V.length vec /= size) $
      throwIO $ AFException SizeError 203 $
        "fromVector: dimension product " <> show size <>
        " does not match vector length " <> show (V.length vec)
    calloca $ \arrayPtr -> do
      dimsPtr <- newArray (DimT . fromIntegral <$> dims)
      onException
        (V.unsafeWith vec $ \ptr -> do
          throwAFError =<< af_create_array arrayPtr (castPtr ptr) ndims dimsPtr dType
          free dimsPtr)
        (free dimsPtr)
      arr <- peek arrayPtr
      Array <$> newForeignPtr af_release_array_finalizer arr

-- | Copies an 'Array' to a new 'Array'
--
-- >>> copyArray (scalar @Double 10)
-- ArrayFire Array
-- [1 1 1 1]
--   10.0000
copyArray
  :: AFType a
  => Array a
  -- ^ 'Array' to be copied
  -> Array a
    -- ^ Newly copied 'Array'
copyArray = (`op1` af_copy_array)

-- | Retains an 'Array', increases reference count
--
-- >>> retainArray (scalar @Double 10)
-- ArrayFire Array
-- [1 1 1 1]
--   10.0000
retainArray
  :: AFType a
  => Array a
  -- ^ Input 'Array'
  -> Array a
retainArray =
  (`op1` af_retain_array)

-- | Retrieves 'Array' reference count
--
-- >>> initialArray = scalar @Double 10
-- >>> retainedArray = retainArray initialArray
-- >>> getDataRefCount retainedArray
-- 2
--
getDataRefCount
  :: AFType a
  => Array a
  -- ^ Input 'Array'
  -> Int
  -- ^ Reference count
getDataRefCount =
  fromIntegral . (`infoFromArray` af_get_data_ref_count)

-- | Force evaluation of a lazily-deferred 'Array', flushing any pending
-- computation in the JIT queue and returning the same array.
--
-- >>> eval (vector @Double 10 [1..])
-- ArrayFire Array
-- ...
--
eval :: AFType a => Array a -> Array a
eval arr@(Array fptr) = unsafePerformIO . mask_ $
  withForeignPtr fptr (throwAFError <=< af_eval) >> pure arr
{-# NOINLINE eval #-}

-- | Should manual evaluation occur
--
-- >>> setManualEvalFlag True
-- ()
setManualEvalFlag
  :: Bool
  -- ^ Whether or not to perform manual evaluation
  -> IO ()
setManualEvalFlag (fromIntegral . fromEnum -> b) =
  afCall (af_set_manual_eval_flag b)

-- | Retrieve manual evaluation status
--
-- >>> setManualEvalFlag False
-- >>> getManualEvalFlag
-- False
--
getManualEvalFlag
  :: IO Bool
getManualEvalFlag =
  toEnum . fromIntegral <$> afCall1 af_get_manual_eval_flag

-- | Retrieve element count
--
-- >>> getElements (vector @Double 10 [1..])
-- 10
--
getElements
  :: AFType a
  => Array a
  -- ^ Input 'Array'
  -> Int
  -- ^ Count of elements in 'Array'
getElements a =
  fromIntegral (a `infoFromArray` af_get_elements)

-- | Retrieve type of 'Array'
--
-- >>> getType (vector @Double 10 [1..])
-- F64
--
getType
  :: AFType a
  => Array a
  -> AFDType
getType a = fromAFType (a `infoFromArray` af_get_type)

-- | Retrieves dimensions of 'Array'
--
-- >>> getDims (vector @Double 10 [1..])
-- (10,1,1,1)
--
getDims
  :: AFType a
  => Array a
  -> (Int,Int,Int,Int)
getDims arr = do
  let (a,b,c,d) = arr `infoFromArray4` af_get_dims
  (fromIntegral a, fromIntegral b, fromIntegral c, fromIntegral d)

-- | Retrieves number of dimensions in 'Array'
--
-- >>> getNumDims (matrix @Double (2,2) [[1..],[1..]])
-- 2
--
getNumDims
  :: AFType a
  => Array a
  -> Int
getNumDims = fromIntegral . (`infoFromArray` af_get_numdims)

-- | Checks if an 'Array' is empty
--
-- >>> isEmpty (matrix @Double (2,2) [[1..],[1..]])
-- False
--
isEmpty
  :: AFType a
  => Array a
  -> Bool
isEmpty a = toEnum . fromIntegral $ (a `infoFromArray` af_is_empty)

-- | Checks if an 'Array' is a scalar (contains only one element)
--
-- >>> isScalar (matrix @Double (2,2) [[1..],[1..]])
-- False
-- >>> isScalar (1.0 :: Array Double)
-- True
--
isScalar
  :: AFType a
  => Array a
  -> Bool
isScalar a = toEnum . fromIntegral $ (a `infoFromArray` af_is_scalar)

-- | Checks if an 'Array' is row-oriented
--
-- >>> isRow (matrix @Double (2,2) [[1..],[1..]])
-- False
--
isRow
  :: AFType a
  => Array a
  -> Bool
isRow a = toEnum . fromIntegral $ (a `infoFromArray` af_is_row)

-- | Checks if an 'Array' is a column-oriented
--
-- >>> isColumn (vector @Double 10 [1..])
-- True
--
isColumn
  :: AFType a
  => Array a
  -> Bool
isColumn a = toEnum . fromIntegral $ (a `infoFromArray` af_is_column)

-- | Checks if an 'Array' is a vector
--
-- >>> isVector (vector @Double 10 [1..])
-- True
-- >>> isVector (1.0 :: Array Double)
-- False
--
isVector
  :: AFType a
  => Array a
  -> Bool
isVector a = toEnum . fromIntegral $ (a `infoFromArray` af_is_vector)

-- | Checks if an 'Array' is a Complex
--
-- >>> isComplex (scalar (1.0 :+ 1.0) :: Array (Complex Double))
-- True
--
isComplex
  :: AFType a
  => Array a
  -> Bool
isComplex a = toEnum . fromIntegral $ (a `infoFromArray` af_is_complex)

-- | Checks if an 'Array' is Real
--
-- >>> isReal (scalar 1.0 :: Array Double)
-- True
--
isReal
  :: AFType a
  => Array a
  -> Bool
isReal a = toEnum . fromIntegral $ (a `infoFromArray` af_is_real)

-- | Checks if an 'Array' is 'Double'
--
-- >>> isDouble (scalar 1.0 :: Array Double)
-- True
--
isDouble
  :: AFType a
  => Array a
  -> Bool
isDouble a = toEnum . fromIntegral $ (a `infoFromArray` af_is_double)

-- | Checks if an 'Array' is 'Float'
--
-- >>> isSingle (scalar 1.0 :: Array Float)
-- True
--
isSingle
  :: AFType a
  => Array a
  -> Bool
isSingle a = toEnum . fromIntegral $ (a `infoFromArray` af_is_single)

-- | Checks if an 'Array' is 'Double', 'Float', Complex 'Double', or Complex 'Float'
--
-- >>> isRealFloating (scalar 1.0 :: Array Double)
-- True
--
isRealFloating
  :: AFType a
  => Array a
  -> Bool
isRealFloating a = toEnum . fromIntegral $ (a `infoFromArray` af_is_realfloating)

-- | Checks if an 'Array' is 'Double' or 'Float'
--
-- >>> isFloating (scalar 1.0 :: Array Double)
-- True
isFloating
  :: AFType a
  => Array a
  -> Bool
isFloating a = toEnum . fromIntegral $ (a `infoFromArray` af_is_floating)

-- | Checks if an 'Array' is of type Int16, Int32, or Int64
--
-- >>> isInteger (scalar 1 :: Array Int16)
-- True
isInteger
  :: AFType a
  => Array a
  -> Bool
isInteger a = toEnum . fromIntegral $ (a `infoFromArray` af_is_integer)

-- | Checks if an 'Array' is of type CBool
--
-- >>> isBool (scalar 1 :: Array CBool)
-- True
isBool
  :: AFType a
  => Array a
  -> Bool
isBool a = toEnum . fromIntegral $ (a `infoFromArray` af_is_bool)

-- | Checks if an 'Array' is sparse
--
-- >>> isSparse (scalar 1 :: Array Double)
-- False
isSparse
  :: AFType a
  => Array a
  -> Bool
isSparse a = toEnum . fromIntegral $ (a `infoFromArray` af_is_sparse)

-- | Converts an 'Array' to a 'Storable' 'Vector'
--
-- >>> toVector (vector @Double 10 [1..])
-- [1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0]
toVector :: forall a . AFType a => Array a -> Vector a
{-# NOINLINE toVector #-}
toVector arr@(Array fptr) =
  unsafePerformIO . mask_ . withForeignPtr fptr $ \arrPtr -> do
    let len = getElements arr
        size = len * getSizeOf (Proxy @a)
    ptr <- mallocBytes size
    throwAFError =<< af_get_data_ptr (castPtr ptr) arrPtr
    newFptr <- newForeignPtr finalizerFree ptr
    pure $ unsafeFromForeignPtr0 newFptr len

-- | Converts an 'Array' to [a]
--
-- >>> toList (vector @Double 10 [1..])
-- [1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0]
toList :: forall a . AFType a => Array a -> [a]
toList = V.toList . toVector

-- | Retrieves single scalar value from an 'Array'
--
-- >>> getScalar (scalar @Double 22.0) :: Double
-- 22.0
getScalar :: forall a b . (Storable a, AFType b) => Array b -> a
{-# NOINLINE getScalar #-}
getScalar (Array fptr) =
  unsafePerformIO . mask_ . withForeignPtr fptr $ \arrPtr -> do
    alloca $ \ptr -> do
      throwAFError =<< af_get_scalar (castPtr ptr) arrPtr
      peek ptr
