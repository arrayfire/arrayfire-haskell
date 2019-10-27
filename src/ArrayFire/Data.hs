{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds           #-}
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
import Data.Int
import Data.Proxy
import Data.Word
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Marshal          hiding (void)
import Foreign.Storable
import System.IO.Unsafe
import Unsafe.Coerce

import ArrayFire.Exception
import ArrayFire.FFI
import ArrayFire.Internal.Data
import ArrayFire.Internal.Defines
import ArrayFire.Internal.Types
import ArrayFire.Arith

-- | Creates an 'Array' from a scalar value from given dimensions
--
-- >>> constant @Double [2,2] 2.0
--  ArrayFire Array
-- [2 2 1 1]
--    2.0000     2.0000
--    2.0000     2.0000
constant
  :: forall a . AFType a
  => [Int]
  -- ^ Dimensions
  -> a
  -- ^ Scalar value
  -> Array a
constant dims val =
  case dtyp of
    x | x == c64 ->
        cast $ constantComplex dims (unsafeCoerce val :: Complex Double)
      | x == c32 ->
        cast $ constantComplex dims (unsafeCoerce val :: Complex Float)
      | x == s64 ->
        cast $ constantLong dims (unsafeCoerce val :: Int)
      | x == u64 ->
        cast $ constantULong dims (unsafeCoerce val :: Word64)
      | x == s32 ->
        cast $ constant' dims (fromIntegral (unsafeCoerce val :: Int32) :: Double)
      | x == s16 ->
        cast $ constant' dims (fromIntegral (unsafeCoerce val :: Int16) :: Double)
      | x == u32 ->
        cast $ constant' dims (fromIntegral (unsafeCoerce val :: Word32) :: Double)
      | x == u8 ->
        cast $ constant' dims (fromIntegral (unsafeCoerce val :: Word8) :: Double)
      | x == u16 ->
        cast $ constant' dims (fromIntegral (unsafeCoerce val :: Word16) :: Double)
      | x == f64 ->
        cast $ constant' dims (unsafeCoerce val :: Double)
      | x == b8  ->
        cast $ constant' dims (fromIntegral (unsafeCoerce val :: CBool) :: Double)
      | x == f32 ->
        cast $ constant' dims (realToFrac (unsafeCoerce val :: Float))
      | otherwise -> error "constant: Invalid array fire type"
  where
    dtyp = afType (Proxy @ a)

    constant'
      :: [Int]
      -- ^ Dimensions
      -> Double
      -- ^ Scalar value
      -> Array Double
    constant' dims' val' =
      unsafePerformIO . mask_ $ do
        ptr <- alloca $ \ptrPtr -> do
          zeroOutArray ptrPtr
          withArray (fromIntegral <$> dims') $ \dimArray -> do
            throwAFError =<< af_constant ptrPtr val' n dimArray typ
            peek ptrPtr
        Array <$>
          newForeignPtr
            af_release_array_finalizer
              ptr
          where
            n = fromIntegral (length dims')
            typ = afType (Proxy @ Double)

    -- | Creates an 'Array (Complex Double)' from a scalar val'ue
    --
    -- @
    -- >>> constantComplex [2,2] (2.0 :+ 2.0)
    -- @
    --
    constantComplex
      :: forall arr . (Real arr, AFType (Complex arr))
      => [Int]
      -- ^ Dimensions
      -> Complex arr
      -- ^ Scalar val'ue
      -> Array (Complex arr)
    constantComplex dims' ((realToFrac -> x) :+ (realToFrac -> y)) = unsafePerformIO . mask_ $ do
      ptr <- alloca $ \ptrPtr -> do
        zeroOutArray ptrPtr
        withArray (fromIntegral <$> dims') $ \dimArray -> do
          throwAFError =<< af_constant_complex ptrPtr x y n dimArray typ
          peek ptrPtr
      Array <$>
        newForeignPtr
          af_release_array_finalizer
            ptr
          where
            n = fromIntegral (length dims')
            typ = afType (Proxy @ (Complex arr))

    -- | Creates an 'Array Int64' from a scalar val'ue
    --
    -- @
    -- >>> constantLong [2,2] 2.0
    -- @
    --
    constantLong
      :: [Int]
      -- ^ Dimensions
      -> Int
      -- ^ Scalar val'ue
      -> Array Int
    constantLong dims' val' = unsafePerformIO . mask_ $ do
      ptr <- alloca $ \ptrPtr -> do
        zeroOutArray ptrPtr
        withArray (fromIntegral <$> dims') $ \dimArray -> do
          throwAFError =<< af_constant_long ptrPtr (fromIntegral val') n dimArray
          peek ptrPtr
      Array <$>
        newForeignPtr
          af_release_array_finalizer
            ptr
          where
            n = fromIntegral (length dims')

    -- | Creates an 'Array Word64' from a scalar val'ue
    --
    -- @
    -- >>> constantULong [2,2] 2.0
    -- @
    --
    constantULong
      :: [Int]
      -> Word64
      -> Array Word64
    constantULong dims' val' = unsafePerformIO . mask_ $ do
      ptr <- alloca $ \ptrPtr -> do
        zeroOutArray ptrPtr
        withArray (fromIntegral <$> dims') $ \dimArray -> do
          throwAFError =<< af_constant_ulong ptrPtr (fromIntegral val') n dimArray
          peek ptrPtr
      Array <$>
        newForeignPtr
          af_release_array_finalizer
            ptr
          where
            n = fromIntegral (length dims')

-- | Creates a range of values in an Array
--
-- >>> range @Double [10] (-1)
-- ArrayFire Array
-- [10 1 1 1]
--    0.0000     1.0000     2.0000     3.0000     4.0000     5.0000     6.0000     7.0000     8.0000     9.0000
range
  :: forall a
   . AFType a
  => [Int]
  -> Int
  -> Array a
range dims (fromIntegral -> k) = unsafePerformIO $ do
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

-- | Create an sequence [0, dims.elements() - 1] and modify to specified dimensions dims and then tile it according to tile_dims.
--
-- <http://arrayfire.org/docs/group__data__func__iota.htm>
--
-- >>> iota @Double [5,3] []
-- ArrayFire Array
-- [5 3 1 1]
--    0.0000     1.0000     2.0000     3.0000     4.0000
--    5.0000     6.0000     7.0000     8.0000     9.0000
--   10.0000    11.0000    12.0000    13.0000    14.0000
--
-- >>> iota @Double [5,3] [1,2]
-- ArrayFire Array
-- [5 6 1 1]
--  0.0000     1.0000     2.0000     3.0000     4.0000
--  5.0000     6.0000     7.0000     8.0000     9.0000
-- 10.0000    11.0000    12.0000    13.0000    14.0000
--  0.0000     1.0000     2.0000     3.0000     4.0000
--  5.0000     6.0000     7.0000     8.0000     9.0000
-- 10.0000    11.0000    12.0000    13.0000    14.0000
iota
  :: forall a . AFType a
  => [Int]
  -- ^ is the array containing sizes of the dimension
  -> [Int]
  -- ^ is array containing the number of repetitions of the unit dimensions
  -> Array a
  -- ^ is the generated array
iota dims tdims = unsafePerformIO $ do
  let dims' = take 4 (dims ++ repeat 1)
      tdims' =  take 4 (tdims ++ repeat 1)
  ptr <- alloca $ \ptrPtr -> mask_ $ do
    zeroOutArray ptrPtr
    withArray (fromIntegral <$> dims') $ \dimArray ->
      withArray (fromIntegral <$> tdims') $ \tdimArray -> do
        throwAFError =<< af_iota ptrPtr 4 dimArray 4 tdimArray typ
        peek ptrPtr
  Array <$>
    newForeignPtr
      af_release_array_finalizer
        ptr
      where
        typ = afType (Proxy @ a)

-- | Creates the identity `Array` from given dimensions
--
-- >>> identity [2,2]
-- ArrayFire Array
-- [2 2 1 1]
--    1.0000     0.0000
--    0.0000     1.0000
identity
  :: forall a . AFType a
  => [Int]
  -- ^ Dimensions
  -> Array a
identity dims = unsafePerformIO . mask_ $ do
  let dims' = take 4 (dims ++ repeat 1)
  ptr <- alloca $ \ptrPtr -> mask_ $ do
    zeroOutArray ptrPtr
    withArray (fromIntegral <$> dims') $ \dimArray -> do
      throwAFError =<< af_identity ptrPtr n dimArray typ
      peek ptrPtr
  Array <$>
    newForeignPtr
      af_release_array_finalizer
        ptr
      where
        n = fromIntegral (length dims)
        typ = afType (Proxy @ a)

-- | Create a diagonal matrix from input array when extract is set to false
--
-- >>> diagCreate (vector @Double 2 [1..]) 0
-- ArrayFire Array
-- [2 2 1 1]
--    1.0000     0.0000
--    0.0000     2.0000
diagCreate
  :: AFType (a :: *)
  => Array a
  -- ^	is the input array which is the diagonal
  -> Int
  -- ^ is the diagonal index
  -> Array a
diagCreate x (fromIntegral -> n) =
  x `op1` (\p a -> af_diag_create p a n)

-- | Create a diagonal matrix from input array when extract is set to false
--
-- >>> diagExtract (matrix @Double (2,2) [[1,2],[3,4]]) 0
-- ArrayFire Array
-- [2 1 1 1]
--    1.0000     4.0000
diagExtract
  :: AFType (a :: *)
  => Array a
  -> Int
  -> Array a
diagExtract x (fromIntegral -> n) =
  x `op1` (\p a -> af_diag_extract p a n)

-- | Join two Arrays together along a specified dimension
--
-- >>> join 0 (matrix @Double (2,2) [[1,2],[3,4]]) (matrix @Double (2,2) [[5,6],[7,8]])
-- ArrayFire Array
-- [4 2 1 1]
--    1.0000     2.0000     5.0000     6.0000
--    3.0000     4.0000     7.0000     8.0000
--
join
  :: Int
  -> Array (a :: *)
  -> Array a
  -> Array a
join (fromIntegral -> n) arr1 arr2 = op2 arr1 arr2 (\p a b -> af_join p n a b)

-- | Join many Arrays together along a specified dimension
--
-- *FIX ME*
--
-- >>> joinMany 0 [1,2,3]
-- ArrayFire Array
-- [3 1 1 1]
--    1.0000     2.0000     3.0000
--
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
        zeroOutArray aPtr
        throwAFError =<< af_join_many aPtr n nArrays fPtrsPtr
        peek aPtr
  Array <$>
    newForeignPtr af_release_array_finalizer newPtr
  where
    nArrays = fromIntegral (length arrays)

-- | Tiles an Array according to specified dimensions
--
-- >>> tile @Double (scalar 22.0) [5,5]
-- ArrayFire Array
-- [5 5 1 1]
-- 22.0000    22.0000    22.0000    22.0000    22.0000
-- 22.0000    22.0000    22.0000    22.0000    22.0000
-- 22.0000    22.0000    22.0000    22.0000    22.0000
-- 22.0000    22.0000    22.0000    22.0000    22.0000
-- 22.0000    22.0000    22.0000    22.0000    22.0000
--
tile
  :: Array (a :: *)
  -> [Int]
  -> Array a
tile a (take 4 . (++repeat 1) -> [x,y,z,w]) =
  a `op1` (\p k -> af_tile p k (fromIntegral x) (fromIntegral y) (fromIntegral z) (fromIntegral w))
tile _ _ = error "impossible"

-- | Reorders an Array according to newly specified dimensions
--
-- *FIX ME*
--
-- >>> reorder @Double (scalar 22.0) [5,5]
-- ArrayFire Array
-- [5 5 1 1]
-- 22.0000    22.0000    22.0000    22.0000    22.0000
-- 22.0000    22.0000    22.0000    22.0000    22.0000
-- 22.0000    22.0000    22.0000    22.0000    22.0000
-- 22.0000    22.0000    22.0000    22.0000    22.0000
-- 22.0000    22.0000    22.0000    22.0000    22.0000
--
reorder
  :: Array (a :: *)
  -> [Int]
  -> Array a
reorder a (take 4 . (++ repeat 0) -> [x,y,z,w]) =
  a `op1` (\p k -> af_reorder p k (fromIntegral x) (fromIntegral y) (fromIntegral z) (fromIntegral w))
reorder _ _ = error "impossible"

-- | Shift elements in an Array along a specified dimension (elements will wrap).
--
-- >>> shift (vector @Double 4 [1..]) 2 0 0 0
-- ArrayFire Array
-- [4 1 1 1]
--    3.0000     4.0000     1.0000     2.0000
--
shift
  :: Array (a :: *)
  -> Int
  -> Int
  -> Int
  -> Int
  -> Array a
shift a (fromIntegral -> x) (fromIntegral -> y) (fromIntegral -> z) (fromIntegral -> w) =
  a `op1` (\p k -> af_shift p k x y z w)

-- | Modify dimensions of array
--
-- >>> moddims (vector @Double 3 [1..]) [1,3]
-- ArrayFire Array
-- [1 3 1 1]
--    1.0000
--    2.0000
--    3.0000
--
moddims
  :: forall a
   . Array (a :: *)
  -> [Int]
  -> Array a
moddims (Array fptr) dims =
  unsafePerformIO . mask_ . withForeignPtr fptr $ \ptr -> do
    newPtr <- alloca $ \aPtr -> do
      zeroOutArray aPtr
      withArray (fromIntegral <$> dims) $ \dimsPtr -> do
        throwAFError =<< af_moddims aPtr ptr n dimsPtr
        peek aPtr
    Array <$> newForeignPtr af_release_array_finalizer newPtr
  where
    n = fromIntegral (length dims)

-- | Flatten an Array into a single dimension
--
-- >>> flat (matrix @Double (2,2) [[1..],[1..]])
-- ArrayFire Array
-- [4 1 1 1]
--    1.0000     2.0000     1.0000     2.0000
--
-- >>> flat $ cube @Int (2,2,2) [[[1,1],[1,1]],[[1,1],[1,1]]]
-- ArrayFire Array
-- [8 1 1 1]
--     1          1          1          1          1          1          1          1
--
flat
  :: Array a
  -> Array a
flat = (`op1` af_flat)

-- | Flip the values of an Array along a specified dimension
--
-- >>> matrix @Double (2,2) [[2,2],[3,3]]
-- ArrayFire Array
-- [2 2 1 1]
--    2.0000     2.0000
--    3.0000     3.0000
--
-- >>> A.flip (matrix @Double (2,2) [[2,2],[3,3]]) 1
-- ArrayFire Array
-- [2 2 1 1]
--    3.0000     3.0000
--    2.0000     2.0000
--
flip
  :: Array a
  -> Int
  -> Array a
flip a (fromIntegral -> dim) =
  a `op1` (\p k -> af_flip p k dim)

-- | Create a lower triangular matrix from input array.
--
-- >>> lower (constant [2,2] 10 :: Array Double) True
-- ArrayFire Array
-- [2 2 1 1]
--    1.0000    10.0000
--    0.0000     1.0000
--
lower
  :: Array a
  -- ^ is the input matrix
  -> Bool
  -- ^ boolean parameter specifying if the diagonal elements should be 1
  -> Array a
lower a (fromIntegral . fromEnum -> b) =
  a `op1` (\p k -> af_lower p k b)

-- | Create an upper triangular matrix from input array.
--
-- >>> upper (constant [2,2] 10 :: Array Double) True
-- ArrayFire Array
-- [2 2 1 1]
--    1.0000     0.0000
--   10.0000     1.0000
--
upper
  :: Array a
  -> Bool
  -> Array a
upper a (fromIntegral . fromEnum -> b) =
  a `op1` (\p k -> af_upper p k b)

-- | Selects elements from two arrays based on the values of a binary conditional array.
--
-- >>> cond = vector @CBool 5 [1,0,1,0,1]
-- >>> arr1 = vector @Double 5 (repeat 1)
-- >>> arr2 = vector @Double 5 (repeat 2)
-- >>> select cond arr1 arr2
-- ArrayFire Array
-- [5 1 1 1]
--    1.0000     2.0000     1.0000     2.0000     1.0000
--
select
  :: Array CBool
  -- ^ is the conditional array
  -> Array a
  -- ^ is the array containing elements from the true part of the condition
  -> Array a
  -- ^	is the array containing elements from the false part of the condition
  -> Array a
  -- ^ is the output containing elements of a when cond is true else elements from b
select a b c = op3 a b c af_select

-- | Selects elements from two arrays based on the values of a binary conditional array.
--
-- <http://arrayfire.org/docs/group__data__func__select.htm#gab6886120d0bac4717276910e468bbe88>
--
-- >>> cond = vector @CBool 5 [1,0,1,0,1]
-- >>> arr1 = vector @Double 5 (repeat 1)
-- >>> x = 99
-- >>> selectScalarR cond x arr1
-- ArrayFire Array
-- [5 1 1 1]
--    1.0000    99.0000     1.0000    99.0000     1.0000
--
selectScalarR
  :: Array CBool
  -- ^ is the conditional array
  -> Array a
  -- ^ is the array containing elements from the true part of the condition
  -> Double
  -- ^	is a scalar assigned to out when cond is false
  -> Array a
  -- ^ the output containing elements of a when cond is true else elements from b
selectScalarR a b c = op2 a b (\p w x -> af_select_scalar_r p w x c)

-- | Selects elements from two arrays based on the values of a binary conditional array.
--
-- [ArrayFire Docs](http://arrayfire.org/docs/group__data__func__select.htm#ga0ccdc05779f88cab5095bce987c2da9d)
--
-- >>> cond = vector @CBool 5 [1,0,1,0,1]
-- >>> arr1 = vector @Double 5 (repeat 1)
-- >>> x = 99
-- >>> selectScalarL cond x arr1
-- ArrayFire Array
-- [5 1 1 1]
--   99.0000     1.0000    99.0000     1.0000    99.0000
--
selectScalarL
  :: Array CBool
  -- ^ the conditional array
  -> Double
  -- ^ a scalar assigned to out when cond is true
  -> Array a
  -- ^ the array containing elements from the false part of the condition
  -> Array a
  -- ^ is the output containing elements of a when cond is true else elements from b
selectScalarL a n b = op2 a b (\p w x -> af_select_scalar_l p w n x)

-- af_err af_replace(af_array a, const af_array cond, const af_array b);
-- af_err af_replace_scalar(af_array a, const af_array cond, const double b);
