{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}
--------------------------------------------------------------------------------
-- |
-- Module      : ArrayFire.Algorithm
-- Copyright   : David Johnson (c) 2019-2020
-- License     : BSD 3
-- Maintainer  : David Johnson <djohnson.m@gmail.com>
-- Stability   : Experimental
-- Portability : GHC
--
-- Functions for aggregation, manipulation of 'Array'
--
-- @
-- module Main where
--
-- import ArrayFire
--
-- main :: IO ()
-- main = print =<< getAvailableBackends
-- @
--
-- @
-- [nix-shell:~\/arrayfire]$ .\/main
-- [CPU,OpenCL]
-- @
--------------------------------------------------------------------------------
module ArrayFire.Algorithm where

import ArrayFire.Array
import ArrayFire.FFI
import ArrayFire.Internal.Algorithm
import ArrayFire.Internal.Types

import Foreign.C.Types
import Data.Word

-- | Retrieves count of all elements in 'Array' along the specified dimension
--
-- @
-- >>> 'clamp' ('vector' \@'Int' 10 [1..])
-- @
sum
  :: AFType a
  => Array a
  -- ^ Array to sum
  -> Int
  -- ^ Dimension along which to perform sum
  -> a
  -- ^ Will return the sum of all values in the input array along the specified dimension
sum x (fromIntegral -> n) = getScalar (x `op1` (\p a -> af_sum p a n))

-- | Retrieves count of all elements in 'Array' along the specified dimension
--
-- @
-- >>> 'clamp' ('vector' \@'Int' 10 [1..])
-- @
sumNaN
  :: (Fractional a, AFType a)
  => Array a
  -- ^ Array to sum
  -> Int
  -- ^ Dimension along which to perform sum
  -> Double
  -- ^ Default value to use in the case of NaN
  -> a
  -- ^ Will return the sum of all values in the input array along the specified dimension, substituted with the default value
sumNaN n (fromIntegral -> i) d = getScalar (n `op1` (\p a -> af_sum_nan p a i d))

-- | Retrieves count of all elements in 'Array' along the specified dimension
--
-- @
-- >>> 'clamp' ('vector' \@'Int' 10 [1..])
-- @
product
  :: AFType a
  => Array a
  -- ^ Array to product
  -> Int
  -- ^ Dimension along which to perform product
  -> a
  -- ^ Will return the product of all values in the input array along the specified dimension
product x (fromIntegral -> n) = getScalar (x `op1` (\p a -> af_product p a n))

-- | Retrieves count of all elements in 'Array' along the specified dimension
--
-- @
-- >>> 'clamp' ('vector' \@'Int' 10 [1..])
-- @
productNaN
  :: (AFType a, Fractional a)
  => Array a
  -- ^ Array to product
  -> Int
  -- ^ Dimension along which to perform product
  -> Double
  -- ^ Default value to use in the case of NaN
  -> a
  -- ^ Will return the product of all values in the input array along the specified dimension, substituted with the default value
productNaN n (fromIntegral -> i) d = getScalar (n `op1` (\p a -> af_product_nan p a i d))

-- | Retrieves count of all elements in 'Array' along the specified dimension
--
-- @
-- >>> 'clamp' ('vector' \@'Int' 10 [1..])
-- @
min
  :: AFType a
  => Array a
  -- ^ Array input
  -> Int
  -- ^ Dimension along which to retrieve the min element
  -> a
  -- ^ Will contain the minimum of all values in the input array along dim
min x (fromIntegral -> n) = getScalar (x `op1` (\p a -> af_min p a n))

-- | Retrieves count of all elements in 'Array' along the specified dimension
--
-- @
-- >>> 'clamp' ('vector' \@'Int' 10 [1..])
-- @
max
  :: AFType a
  => Array a
  -- ^ Array input
  -> Int
  -- ^ Dimension along which to retrieve the max element
  -> a
  -- ^ Will contain the maximum of all values in the input array along dim
max x (fromIntegral -> n) = getScalar (x `op1` (\p a -> af_max p a n))

-- | Retrieves count of all elements in 'Array' along the specified dimension
--
-- @
-- >>> 'clamp' ('vector' \@'Int' 10 [1..])
-- @
allTrue
  :: forall a. AFType a
  => Array a
  -- ^ Array input
  -> Int
  -- ^ Dimension along which to see if all elements are True
  -> Bool
  -- ^ Will contain the maximum of all values in the input array along dim
allTrue x (fromIntegral -> n) =
  toEnum . fromIntegral $ getScalar @CBool @a (x `op1` (\p a -> af_all_true p a n))

-- | Retrieves count of all elements in 'Array' along the specified dimension
--
-- @
-- >>> 'clamp' ('vector' \@'Int' 10 [1..])
-- @
anyTrue
  :: forall a . AFType a
  => Array a
  -- ^ Array input
  -> Int
  -- ^ Dimension along which to see if all elements are True
  -> Bool
  -- ^ Returns if all elements are true
anyTrue x (fromIntegral -> n) =
  toEnum . fromIntegral $ getScalar @CBool @a (x `op1` (\p a -> af_any_true p a n))

-- | Retrieves count of all elements in 'Array' along the specified dimension
--
-- @
-- >>> 'clamp' ('vector' \@'Int' 10 [1..])
-- @
count
  :: forall a . AFType a
  => Array a
  -- ^ Array input
  -> Int
  -- ^ Dimension along which to count
  -> Int
  -- ^ Count of all elements along dimension
count x (fromIntegral -> n) = fromIntegral $ getScalar @Word32 @a (x `op1` (\p a -> af_count p a n))

-- | Sum all elements in 'Array'
--
-- @
-- >>> 'clamp' ('vector' \@'Int' 10 [1..])
-- @
sumAll
  :: AFType a
  => Array a
  -- ^ Input array
  -> (Double, Double)
  -- ^ imaginary and real part
sumAll = (`infoFromArray2` af_sum_all)

-- | Sum all elements in 'Array', substituting 'NaN' values with a user specified default.
--
-- @
-- >>> 'clamp' ('vector' \@'Int' 10 [1..])
-- @
sumNaNAll
  :: (AFType a, Fractional a)
  => Array a
  -- ^ Input array
  -> Double
  -- ^ NaN substitute
  -> (Double, Double)
  -- ^ imaginary and real part
sumNaNAll a d = infoFromArray2 a (\p g x -> af_sum_nan_all p g x d)

-- | Product all elements in 'Array'
--
-- @
-- >>> 'clamp' ('vector' \@'Int' 10 [1..])
-- @
productAll
  :: AFType a
  => Array a
  -- ^ Input array
  -> (Double, Double)
  -- ^ imaginary and real part
productAll = (`infoFromArray2` af_product_all)

-- | Product all elements in 'Array', substituting NaN values with a user specified default.
--
-- @
-- >>> 'clamp' ('vector' \@'Int' 10 [1..])
-- @
productNaNAll
  :: (AFType a, Fractional a)
  => Array a
  -- ^ Input array
  -> Double
  -- ^ NaN substitute
  -> (Double, Double)
  -- ^ imaginary and real part
productNaNAll a d = infoFromArray2 a (\p x y -> af_product_nan_all p x y d)

-- | Find the maximum of two 'Array's
--
-- @
-- >>> 'clamp' ('vector' \@'Int' 10 [1..])
-- @
minAll
  :: AFType a
  => Array a
  -- ^ Input array
  -> (Double, Double)
  -- ^ imaginary and real part
minAll = (`infoFromArray2` af_min_all)

-- | Find the maximum of two 'Array's
--
-- @
-- >>> 'clamp' ('vector' \@'Int' 10 [1..])
-- @
maxAll
  :: AFType a
  => Array a
  -- ^ Input array
  -> (Double, Double)
  -- ^ imaginary and real part
maxAll = (`infoFromArray2` af_max_all)

-- | Find the maximum of two 'Array's
--
-- @
-- >>> 'clamp' ('vector' \@'Int' 10 [1..])
-- @
allTrueAll
  :: AFType a
  => Array a
  -- ^ Input array
  -> (Double, Double)
  -- ^ imaginary and real part
allTrueAll = (`infoFromArray2` af_all_true_all)

-- | Find the maximum of two 'Array's
--
-- @
-- >>> 'clamp' ('vector' \@'Int' 10 [1..])
-- @
anyTrueAll
  :: AFType a
  => Array a
  -- ^ Input array
  -> (Double, Double)
  -- ^ imaginary and real part
anyTrueAll = (`infoFromArray2` af_any_true_all)

-- | Find the maximum of two 'Array's
--
-- @
-- >>> 'clamp' ('vector' \@'Int' 10 [1..])
-- @
countAll
  :: AFType a
  => Array a
  -- ^ Input array
  -> (Double, Double)
  -- ^ imaginary and real part
countAll = (`infoFromArray2` af_count_all)

-- | Find the maximum of two 'Array's
--
-- @
-- >>> 'clamp' ('vector' \@'Int' 10 [1..])
-- @
imin
  :: AFType a
  => Array a
  -- ^ Input array
  -> Int
  -- ^ The dimension along which the minimum value is extracted
  -> (Array a, Array a)
  -- ^ will contain the minimum of all values in in along dim, will also contain the location of minimum of all values in in along dim
imin a (fromIntegral -> n) = op2p a (\x y z -> af_imin x y z n)

-- | Find the maximum of two 'Array's
--
-- @
-- >>> 'clamp' ('vector' \@'Int' 10 [1..])
-- @
imax
  :: AFType a
  => Array a
  -- ^ Input array
  -> Int
  -- ^ The dimension along which the minimum value is extracted
  -> (Array a, Array a)
  -- ^ will contain the maximum of all values in in along dim, will also contain the location of maximum of all values in in along dim
imax a (fromIntegral -> n) = op2p a (\x y z -> af_imax x y z n)

-- | Find the maximum of two 'Array's
--
-- @
-- >>> 'clamp' ('vector' \@'Int' 10 [1..])
-- @
iminAll
  :: AFType a
  => Array a
  -- ^ Input array
  -> (Double, Double, Int)
  -- ^ will contain the real part of minimum value of all elements in input in, also will contain the imaginary part of minimum value of all elements in input in, will contain the location of minimum of all values in
iminAll a = do
  let (x,y,fromIntegral -> z) = a `infoFromArray3` af_imin_all
  (x,y,z)

-- | Find the maximum of two 'Array's
--
-- @
-- >>> 'clamp' ('vector' \@'Int' 10 [1..])
-- @
imaxAll
  :: AFType a
  => Array a
  -- ^ Input array
  -> (Double, Double, Int)
  -- ^ will contain the real part of maximum value of all elements in input in, also will contain the imaginary part of maximum value of all elements in input in, will contain the location of maximum of all values in
imaxAll a = do
  let (x,y,fromIntegral -> z) = a `infoFromArray3` af_imax_all
  (x,y,z)

-- | Find the maximum of two 'Array's
--
-- @
-- >>> 'clamp' ('vector' \@'Int' 10 [1..])
-- @
accum
  :: AFType a
  => Array a
  -- ^ Input array
  -> Int
  -- ^ Dimension along which to calculate the sum
  -> Array a
  -- ^ Contains inclusive sum
accum a (fromIntegral -> n) = a `op1` (\x y -> af_accum x y n)

-- | Find the maximum of two 'Array's
--
-- @
-- >>> 'clamp' ('vector' \@'Int' 10 [1..])
-- @
scan
  :: AFType a
  => Array a
  -- ^ The input array
  -> Int
  -- ^ The dimension along which the scan is performed
  -> BinaryOp
  -- ^ Binary operation to be used
  -> Bool
  -- ^ Should the scan be inclusive or not
  -> Array a
  -- ^ The scan of the input
scan a (fromIntegral -> d) op (fromIntegral . fromEnum -> batch) =
  a `op1` (\x y -> af_scan x y d (toBinaryOp op) batch)

-- | Find the maximum of two 'Array's
--
-- @
-- >>> 'clamp' ('vector' \@'Int' 10 [1..])
-- @
scanByKey
  :: AFType a
  => Array a
  -- ^ The key array
  -> Array a
  -- ^ The input array
  -> Int
  -- ^ Dimension along which scan is performed
  -> BinaryOp
  -- ^ Type of binary operation used
  -> Bool
  -- ^ Is the scan incluside or not
  -> Array a
scanByKey a b (fromIntegral -> d) op (fromIntegral . fromEnum -> batch) =
  op2 a b (\x y z -> af_scan_by_key x y z d (toBinaryOp op) batch)

-- | Find the maximum of two 'Array's
--
-- @
-- >>> 'clamp' ('vector' \@'Int' 10 [1..])
-- @
where'
  :: AFType a
  => Array a
  -- ^ Is the input array.
  -> Array a
  -- ^ will contain indices where input array is non-zero
where' = (`op1` af_where)

-- | Find the maximum of two 'Array's
--
-- @
-- >>> 'clamp' ('vector' \@'Int' 10 [1..])
-- @
diff1
  :: AFType a
  => Array a
  -- ^ Input array
  -> Int
  -- ^ Dimension along which numerical difference is performed
  -> Array a
  -- ^ Will contain first order numerical difference
diff1 a (fromIntegral -> n) = a `op1` (\p x -> af_diff1 p x n)

-- | Find the maximum of two 'Array's
--
-- @
-- >>> 'clamp' ('vector' \@'Int' 10 [1..])
-- @
diff2
  :: AFType a
  => Array a
  -- ^ Input array
  -> Int
  -- ^ Dimension along which numerical difference is performed
  -> Array a
  -- ^ Will contain second order numerical difference
diff2 a (fromIntegral -> n) = a `op1` (\p x -> af_diff2 p x n)

-- | Find the maximum of two 'Array's
--
-- @
-- >>> 'clamp' ('vector' \@'Int' 10 [1..])
-- @
sort
  :: AFType a
  => Array a
  -- ^ Input array
  -> Int
  -- ^ Dimension along `sort` is performed
  -> Bool
  -- ^ Return results in ascending order
  -> Array a
  -- ^ Will contain sorted input
sort a (fromIntegral -> n) (fromIntegral . fromEnum -> b) =
  a `op1` (\p x -> af_sort p x n b)

-- | Find the maximum of two 'Array's
--
-- @
-- >>> 'clamp' ('vector' \@'Int' 10 [1..])
-- @
sortIndex
  :: AFType a
  => Array a
  -- ^ Input array
  -> Int
  -- ^ Dimension along `sortIndex` is performed
  -> Bool
  -- ^ Return results in ascending order
  -> (Array a, Array a)
  -- ^ Contains the sorted, contains indices for original input
sortIndex a (fromIntegral -> n) (fromIntegral . fromEnum -> b) =
  a `op2p` (\p1 p2 p3 -> af_sort_index p1 p2 p3 n b)

-- | Find the maximum of two 'Array's
--
-- @
-- >>> 'clamp' ('vector' \@'Int' 10 [1..])
-- @
sortByKey
  :: AFType a
  => Array a
  -- ^ Keys input array
  -> Array a
  -- ^ Values input array
  -> Int
  -- ^ Dimension along which to perform the operation
  -> Bool
  -- ^ Return results in ascending order
  -> (Array a, Array a)
sortByKey a1 a2 (fromIntegral -> n) (fromIntegral . fromEnum -> b) =
  op2p2 a1 a2 (\w x y z -> af_sort_by_key w x y z n b)

-- | Find the maximum of two 'Array's
--
-- @
-- >>> 'clamp' ('vector' \@'Int' 10 [1..])
-- @
setUnique
  :: AFType a
  => Array a
  -- ^ input array
  -> Bool
  -- ^ if true, skips the sorting steps internally
  -> Array a
  -- ^ Will contain the unique values from in
setUnique a (fromIntegral . fromEnum -> b) =
  op1 a (\x y -> af_set_unique x y b)

-- | Find the maximum of two 'Array's
--
-- @
-- >>> 'clamp' ('vector' \@'Int' 10 [1..])
-- @
setUnion
  :: AFType a
  => Array a
  -- ^ First input array
  -> Array a
  -- ^ Second input array
  -> Bool
  -- ^ If true, skips calling unique internally
  -> Array a
setUnion a1 a2 (fromIntegral . fromEnum -> b) =
  op2 a1 a2 (\x y z -> af_set_union x y z b)

-- | Find the maximum of two 'Array's
--
-- @
-- >>> 'clamp' ('vector' \@'Int' 10 [1..])
-- @
setIntersect
  :: AFType a
  => Array a
  -- ^ First input array
  -> Array a
  -- ^ Second input array
  -> Bool
  -- ^ If true, skips calling unique internally
  -> Array a
  -- ^ Intersection of first and second array
setIntersect a1 a2 (fromIntegral . fromEnum -> b) =
  op2 a1 a2 (\x y z -> af_set_intersect x y z b)
