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

-- | Creates an 'Array' 'Double' from a scalar value
--
-- @
-- >>> 'constant' \@'Double' [2,2] 2.0
-- @
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
-- >>> range @Double [10] (-1)
-- ArrayFire Array
-- [10 1 1 1]
--    0.0000     1.0000     2.0000     3.0000     4.0000     5.0000     6.0000     7.0000     8.0000     9.0000
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
  ptr <- alloca $ \ptrPtr -> mask_ $ do
    zeroOutArray ptrPtr
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

-- | Creates the identity `Array` from given dimensions
--
-- >>> identity [2,2] 2.0
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
  ptr <- alloca $ \ptrPtr -> mask_ $ do
    zeroOutArray ptrPtr
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
        zeroOutArray aPtr
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
      zeroOutArray aPtr
      withArray (fromIntegral <$> dims) $ \dimsPtr -> do
        throwAFError =<< af_moddims aPtr ptr n dimsPtr
        peek aPtr
    Array <$> newForeignPtr af_release_array_finalizer newPtr
  where
    n = fromIntegral (length dims)

flat
  :: Array a
  -> Array a
flat = (`op1` af_flat)

flip
  :: Array a
  -> Int
  -> Array a
flip a (fromIntegral -> dim) =
  a `op1` (\p k -> af_flip p k dim)

lower
  :: Array a
  -> Bool
  -> Array a
lower a (fromIntegral . fromEnum -> b) =
  a `op1` (\p k -> af_lower p k b)

upper
  :: Array a
  -> Bool
  -> Array a
upper a (fromIntegral . fromEnum -> b) =
  a `op1` (\p k -> af_upper p k b)

select
  :: Array a
  -> Array a
  -> Array a
  -> Array a
select a b c = op3 a b c af_select

selectScalarR
  :: Array a
  -> Array a
  -> Double
  -> Array a
selectScalarR a b c = op2 a b (\p w x -> af_select_scalar_r p w x c)

selectScalarL
  :: Array a
  -> Double
  -> Array a
  -> Array a
selectScalarL a n b = op2 a b (\p w x -> af_select_scalar_l p w n x)

-- af_err af_replace(af_array a, const af_array cond, const af_array b);
-- af_err af_replace_scalar(af_array a, const af_array cond, const double b);
