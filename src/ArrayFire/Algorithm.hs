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
-- import qualified ArrayFire as A
--
-- main :: IO ()
-- main = print $ A.sum (A.vector @Double 10 [1..]) 0
-- -- 55
-- @
--------------------------------------------------------------------------------
module ArrayFire.Algorithm where

import ArrayFire.Array
import ArrayFire.FFI
import ArrayFire.Internal.Algorithm
import ArrayFire.Internal.Types

import Foreign.C.Types
import Data.Word

-- | Sum all of the elements in 'Array' along the specified dimension
--
-- >>> A.sum (A.vector @Double 10 [1..]) 0
-- 55.0
--
-- >>> A.sum (A.matrix @Double (10,10) [[2..],[2..]]) 0
-- 65.0
sum
  :: AFType a
  => Array a
  -- ^ Array to sum
  -> Int
  -- ^ Dimension along which to perform sum
  -> a
  -- ^ Will return the sum of all values in the input array along the specified dimension
sum x (fromIntegral -> n) = getScalar (x `op1` (\p a -> af_sum p a n))

-- | Sum all of the elements in 'Array' along the specified dimension, using a default value for NaN
--
-- >>> A.sumNaN (A.vector @Double 10 [1..]) 0 0.0
-- 55
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

-- | Product all of the elements in 'Array' along the specified dimension
--
-- >>> A.product (A.vector @Double 10 [1..]) 0
-- 3628800.0
product
  :: AFType a
  => Array a
  -- ^ Array to product
  -> Int
  -- ^ Dimension along which to perform product
  -> a
  -- ^ Will return the product of all values in the input array along the specified dimension
product x (fromIntegral -> n) = getScalar (x `op1` (\p a -> af_product p a n))

-- | Product all of the elements in 'Array' along the specified dimension, using a default value for NaN
--
-- >>> A.productNaN (A.vector @Double 10 [1..]) 0 0.0
-- 3628800.0
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

-- | Take the minimum of an 'Array' along a specific dimension
--
-- >>> A.min (A.vector @Double 10 [1..]) 0
-- 1.0
min
  :: AFType a
  => Array a
  -- ^ Array input
  -> Int
  -- ^ Dimension along which to retrieve the min element
  -> a
  -- ^ Will contain the minimum of all values in the input array along dim
min x (fromIntegral -> n) = getScalar (x `op1` (\p a -> af_min p a n))

-- | Take the maximum of an 'Array' along a specific dimension
--
-- >>> A.max (A.vector @Double 10 [1..]) 0
-- 10
max
  :: AFType a
  => Array a
  -- ^ Array input
  -> Int
  -- ^ Dimension along which to retrieve the max element
  -> a
  -- ^ Will contain the maximum of all values in the input array along dim
max x (fromIntegral -> n) = getScalar (x `op1` (\p a -> af_max p a n))

-- | Find if all elements in an 'Array' are 'True' along a dimension
--
-- >>> A.allTrue (A.vector @CBool 10 (repeat 0)) 0
-- False
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

-- | Find if any elements in an 'Array' are 'True' along a dimension
--
-- >>> A.anyTrue (A.vector @CBool 10 (repeat 0)) 0
-- False
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

-- | Count elements in an 'Array' along a dimension
--
-- >>> A.count (A.vector @Double 10 [1..]) 0
-- 10
count
  :: forall a . AFType a
  => Array a
  -- ^ Array input
  -> Int
  -- ^ Dimension along which to count
  -> Int
  -- ^ Count of all elements along dimension
count x (fromIntegral -> n) = fromIntegral $ getScalar @Word32 @a (x `op1` (\p a -> af_count p a n))

-- | Sum all elements in an 'Array' along all dimensions
--
-- >>> A.sumAll (A.vector @Double 10 [1..])
-- (55.0,0.0)
sumAll
  :: AFType a
  => Array a
  -- ^ Input array
  -> (Double, Double)
  -- ^ imaginary and real part
sumAll = (`infoFromArray2` af_sum_all)

-- | Sum all elements in an 'Array' along all dimensions, using a default value for NaN
--
-- >>> A.sumNaNAll (A.vector @Double 10 [1..]) 0.0
-- (55.0,0.0)
sumNaNAll
  :: (AFType a, Fractional a)
  => Array a
  -- ^ Input array
  -> Double
  -- ^ NaN substitute
  -> (Double, Double)
  -- ^ imaginary and real part
sumNaNAll a d = infoFromArray2 a (\p g x -> af_sum_nan_all p g x d)

-- | Product all elements in an 'Array' along all dimensions, using a default value for NaN
--
-- >>> A.productAll (A.vector @Double 10 [1..])
-- (3628800.0,0.0)
productAll
  :: AFType a
  => Array a
  -- ^ Input array
  -> (Double, Double)
  -- ^ imaginary and real part
productAll = (`infoFromArray2` af_product_all)

-- | Product all elements in an 'Array' along all dimensions, using a default value for NaN
--
-- >>> A.productNaNAll (A.vector @Double 10 [1..]) 1.0
-- (3628800.0,0.0)
productNaNAll
  :: (AFType a, Fractional a)
  => Array a
  -- ^ Input array
  -> Double
  -- ^ NaN substitute
  -> (Double, Double)
  -- ^ imaginary and real part
productNaNAll a d = infoFromArray2 a (\p x y -> af_product_nan_all p x y d)

-- | Take the minimum across all elements along all dimensions in 'Array'
--
-- >>> A.minAll (A.vector @Double 10 [1..])
-- (1.0,0.0)
minAll
  :: AFType a
  => Array a
  -- ^ Input array
  -> (Double, Double)
  -- ^ imaginary and real part
minAll = (`infoFromArray2` af_min_all)

-- | Take the maximum across all elements along all dimensions in 'Array'
--
-- >>> A.maxAll (A.vector @Double 10 [1..])
-- (10.0,0.0)
maxAll
  :: AFType a
  => Array a
  -- ^ Input array
  -> (Double, Double)
  -- ^ imaginary and real part
maxAll = (`infoFromArray2` af_max_all)

-- | Decide if all elements along all dimensions in 'Array' are True
--
-- >>> A.allTrueAll (A.vector @CBool 10 (repeat 1))
-- (1.0, 0.0)
allTrueAll
  :: AFType a
  => Array a
  -- ^ Input array
  -> (Double, Double)
  -- ^ imaginary and real part
allTrueAll = (`infoFromArray2` af_all_true_all)

-- | Decide if any elements along all dimensions in 'Array' are True
--
-- >>> A.anyTrueAll $ A.vector @CBool 10 (repeat 0)
-- (0.0,0.0)
anyTrueAll
  :: AFType a
  => Array a
  -- ^ Input array
  -> (Double, Double)
  -- ^ imaginary and real part
anyTrueAll = (`infoFromArray2` af_any_true_all)

-- | Count all elements along all dimensions in 'Array'
--
-- >>> A.countAll (A.matrix @Double (100,100) (replicate 100 [1..]))
-- (10000.0,0.0)
countAll
  :: AFType a
  => Array a
  -- ^ Input array
  -> (Double, Double)
  -- ^ imaginary and real part
countAll = (`infoFromArray2` af_count_all)

-- | Find the minimum element along a specified dimension in 'Array'
--
-- >>> A.imin (A.vector @Double 10 [1..]) 0
-- (ArrayFire Array
-- [1 1 1 1]
--    1.0000
-- ,ArrayFire Array
-- [1 1 1 1]
--         0
-- )
imin
  :: AFType a
  => Array a
  -- ^ Input array
  -> Int
  -- ^ The dimension along which the minimum value is extracted
  -> (Array a, Array a)
  -- ^ will contain the minimum of all values along dim, will also contain the location of minimum of all values in in along dim
imin a (fromIntegral -> n) = op2p a (\x y z -> af_imin x y z n)

-- | Find the maximum element along a specified dimension in 'Array'
--
-- >>> A.imax (A.vector @Double 10 [1..]) 0
-- (ArrayFire Array
-- [1 1 1 1]
--   10.0000
-- ,ArrayFire Array
-- [1 1 1 1]
--         9
-- )
imax
  :: AFType a
  => Array a
  -- ^ Input array
  -> Int
  -- ^ The dimension along which the minimum value is extracted
  -> (Array a, Array a)
  -- ^ will contain the maximum of all values in in along dim, will also contain the location of maximum of all values in in along dim
imax a (fromIntegral -> n) = op2p a (\x y z -> af_imax x y z n)

-- | Find the minimum element along all dimensions in 'Array'
--
-- >>> A.iminAll (A.vector @Double 10 [1..])
-- (1.0,0.0,0)
iminAll
  :: AFType a
  => Array a
  -- ^ Input array
  -> (Double, Double, Int)
  -- ^ will contain the real part of minimum value of all elements in input in, also will contain the imaginary part of minimum value of all elements in input in, will contain the location of minimum of all values in
iminAll a = do
  let (x,y,fromIntegral -> z) = a `infoFromArray3` af_imin_all
  (x,y,z)

-- | Find the maximum element along all dimensions in 'Array'
--
-- >>> A.imaxAll (A.vector @Double 10 [1..])
-- (10.0,0.0,9)
imaxAll
  :: AFType a
  => Array a
  -- ^ Input array
  -> (Double, Double, Int)
  -- ^ will contain the real part of maximum value of all elements in input in, also will contain the imaginary part of maximum value of all elements in input in, will contain the location of maximum of all values in
imaxAll a = do
  let (x,y,fromIntegral -> z) = a `infoFromArray3` af_imax_all
  (x,y,z)

-- | Calculate sum of 'Array' across specified dimension
--
-- >>> A.accum (A.vector @Double 10 [1..]) 0
-- ArrayFire Array
-- [10 1 1 1]
--    1.0000     3.0000     6.0000    10.0000    15.0000    21.0000    28.0000    36.0000    45.0000    55.0000
accum
  :: AFType a
  => Array a
  -- ^ Input array
  -> Int
  -- ^ Dimension along which to calculate the sum
  -> Array a
  -- ^ Contains inclusive sum
accum a (fromIntegral -> n) = a `op1` (\x y -> af_accum x y n)

-- | Scan elements of an 'Array' across a dimension, using a 'BinaryOp', specifying inclusivity.
--
-- >>> A.scan (A.vector @Double 10 [1..]) 0 Add True
-- ArrayFire Array
-- [10 1 1 1]
--    1.0000     3.0000     6.0000    10.0000    15.0000    21.0000    28.0000    36.0000    45.0000    55.0000
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
scan a (fromIntegral -> d) op (fromIntegral . fromEnum -> inclusive) =
  a `op1` (\x y -> af_scan x y d (toBinaryOp op) inclusive)

-- | Scan elements of an 'Array' across a dimension, by key, using a 'BinaryOp', specifying inclusivity.
--
-- >>> A.scanByKey (A.vector @Int 7 [2..]) (A.vector @Int 10 [1..]) 1 Add True
-- ArrayFire Array
-- [7 1 1 1]
--         2          3          4          5          6          7          8
scanByKey
  :: (AFType a, AFType k)
  => Array k
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
scanByKey a b (fromIntegral -> d) op (fromIntegral . fromEnum -> inclusive) =
  op2 a b (\x y z -> af_scan_by_key x y z d (toBinaryOp op) inclusive)

-- | Find indices where input Array is non zero
--
-- >>> A.where' (A.vector @Double 10 (repeat 0))
-- ArrayFire Array
-- [0 1 1 1]
-- <empty>
where'
  :: AFType a
  => Array a
  -- ^ Is the input array.
  -> Array a
  -- ^ will contain indices where input array is non-zero
where' = (`op1` af_where)

-- | First order numerical difference along specified dimension.
--
-- >>> A.diff1 (A.vector @Double 4 [10,35,65,95]) 0
-- ArrayFire Array
-- [3 1 1 1]
--   25.0000    30.0000    30.0000
diff1
  :: AFType a
  => Array a
  -- ^ Input array
  -> Int
  -- ^ Dimension along which numerical difference is performed
  -> Array a
  -- ^ Will contain first order numerical difference
diff1 a (fromIntegral -> n) = a `op1` (\p x -> af_diff1 p x n)

-- | Second order numerical difference along specified dimension.
--
-- >>> A.diff2 (A.vector @Double 5 [1.0,20,55,89,44]) 0
-- ArrayFire Array
-- [3 1 1 1]
--   16.0000    -1.0000   -79.0000
diff2
  :: AFType a
  => Array a
  -- ^ Input array
  -> Int
  -- ^ Dimension along which numerical difference is performed
  -> Array a
  -- ^ Will contain second order numerical difference
diff2 a (fromIntegral -> n) = a `op1` (\p x -> af_diff2 p x n)

-- | Sort an Array along a specified dimension, specifying ordering of results (ascending / descending)
--
-- >>> A.sort (A.vector @Double 4 [ 2,4,3,1 ]) 0 True
-- ArrayFire Array
-- [4 1 1 1]
--    1.0000     2.0000     3.0000     4.0000
--
-- >>> A.sort (A.vector @Double 4 [ 2,4,3,1 ]) 0 False
-- ArrayFire Array
-- [4 1 1 1]
--    4.0000     3.0000     2.0000     1.0000
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

-- | Sort an 'Array' along a specified dimension, specifying ordering of results (ascending / descending), returns indices of sorted results
--
--
-- >>> A.sortIndex (A.vector @Double 4 [3,2,1,4]) 0 True
-- (ArrayFire Array
-- [4 1 1 1]
--     1.0000     2.0000     3.0000     4.0000
-- ,ArrayFire Array
-- [4 1 1 1]
--          2          1          0          3
-- )
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

-- | Sort an 'Array' along a specified dimension by keys, specifying ordering of results (ascending / descending)
--
-- >>> A.sortByKey (A.vector @Double 4 [2,1,4,3]) (A.vector @Double 4 [10,9,8,7]) 0 True
-- (ArrayFire Array
-- [4 1 1 1]
--     1.0000     2.0000     3.0000     4.0000
-- ,ArrayFire Array
-- [4 1 1 1]
--     9.0000    10.0000     7.0000     8.0000
-- )
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

-- | Finds the unique values in an 'Array', specifying if sorting should occur.
--
-- >>> A.setUnique (A.vector @Double 2 [1.0,1.0]) True
-- ArrayFire Array
-- [1 1 1 1]
--    1.0000
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

-- | Takes the union of two 'Array's, specifying if `setUnique` should be called first.
--
-- >>> A.setUnion (A.vector @Double 3 [3,4,5]) (A.vector @Double 3 [1,2,3]) True
-- ArrayFire Array
-- [5 1 1 1]
--    1.0000     2.0000     3.0000     4.0000     5.0000
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

-- | Takes the intersection of two 'Array's, specifying if `setUnique` should be called first.
--
-- >>> A.setIntersect (A.vector @Double 3 [3,4,5]) (A.vector @Double 3 [1,2,3]) True
-- ArrayFire Array
-- [1 1 1 1]
--    3.0000
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
