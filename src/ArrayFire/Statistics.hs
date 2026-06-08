{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
--------------------------------------------------------------------------------
-- |
-- Module      : ArrayFire.Statistics
-- Copyright   : David Johnson (c) 2019-2020
-- License     : BSD3
-- Maintainer  : David Johnson <code@dmj.io>
-- Stability   : Experimental
-- Portability : GHC
--
-- Statistics API.
-- Example of finding the top k elements along with their indices from an 'Array'
--
-- @
-- >>> let (vals,indexes) = 'topk' ( 'vector' \@'Double' 10 [1..] ) 3 'TopKDefault'
-- >>> vals
--
-- ArrayFire Array
-- [3 1 1 1]
--    10.0000
--     9.0000
--     8.0000
--
-- >>> indexes
--
-- ArrayFire Array
-- [3 1 1 1]
--          9
--          8
--          7
-- @
--------------------------------------------------------------------------------
module ArrayFire.Statistics where

import ArrayFire.Array
import ArrayFire.FFI
import ArrayFire.Internal.Statistics
import ArrayFire.Internal.Types

-- | Calculates 'mean' of 'Array' along user-specified dimension.
--
-- >>> mean ( vector @Int 10 [1..] ) 0
-- ArrayFire Array
--   [1 1 1 1]
--      5.5000
mean
  :: AFType a
  => Array a
  -- ^ Input 'Array'
  -> Int
  -- ^ The dimension along which the mean is extracted
  -> Array a
  -- ^ Will contain the mean of the input 'Array' along dimension dim
mean a n =
  a `op1` (\x y ->
    af_mean x y (fromIntegral n))

-- | Calculates 'meanWeighted' of 'Array' along user-specified dimension.
--
-- >>> meanWeighted (vector @Double 10 [1..10]) (vector @Double 10 [1..10]) 0
-- ArrayFire Array
--   [1 1 1 1]
--      7.0000
meanWeighted
  :: AFType a
  => Array a
  -- ^ Input 'Array'
  -> Array a
  -- ^ Weights 'Array'
  -> Int
  -- ^ The dimension along which the mean is extracted
  -> Array a
  -- ^ Will contain the mean of the input 'Array' along dimension dim
meanWeighted x y (fromIntegral -> n) =
  op2 x y $ \a b c ->
    af_mean_weighted a b c n

-- | Calculates /variance/ of 'Array' along user-specified dimension.
--
-- >>> var (vector @Double 8 [1..8]) False 0
-- ArrayFire Array
--   [1 1 1 1]
--      6.0000
var
  :: AFType a
  => Array a
  -- ^ Input 'Array'
  -> Bool
  -- ^ boolean denoting Population variance (false) or Sample Variance (true)
  -> Int
  -- ^ The dimension along which the variance is extracted
  -> Array a
  -- ^ will contain the variance of the input array along dimension dim
var arr (fromIntegral . fromEnum -> b) d =
  arr `op1` (\p x ->
    af_var p x b (fromIntegral d))

-- | Calculates 'varWeighted' of 'Array' along user-specified dimension.
--
-- >>> varWeighted ( vector @Double 10 [1..] ) ( vector @Double 10 [1..] ) 0
-- ArrayFire Array
--   [1 1 1 1]
--      6.0000
varWeighted
  :: AFType a
  => Array a
  -- ^ Input 'Array'
  -> Array a
  -- ^ Weights 'Array' used to scale input in before getting variance
  -> Int
  -- ^ The dimension along which the variance is extracted
  -> Array a
  -- ^ Contains the variance of the input array along dimension dim
varWeighted x y (fromIntegral -> n) =
  op2 x y $ \a b c ->
    af_var_weighted a b c n

-- | Calculates 'stdev' of 'Array' along user-specified dimension.
--
-- >>> stdev (vector @Double 10 (cycle [1,-1])) 0
-- ArrayFire Array
--   [1 1 1 1]
--      1.0000
stdev
  :: AFType a
  => Array a
  -- ^ Input 'Array'
  -> Int
  -- ^ The dimension along which the standard deviation is extracted
  -> Array a
  -- ^ Contains the standard deviation of the input array along dimension dim
stdev a n =
  a `op1` (\x y ->
    af_stdev x y (fromIntegral n))

-- | Calculates /covariance/ of two 'Array's with a bias specifier.
--
-- >>> cov (vector @Double 10 (repeat 1)) (vector @Double 10 (repeat 1)) False
-- ArrayFire Array
--   [1 1 1 1]
--      0.0000
cov
  :: AFType a
  => Array a
  -- ^ First input 'Array'
  -> Array a
  -- ^ Second input 'Array'
  -> Bool
  -- ^ A boolean specifying if biased estimate should be taken (default: 'False')
  -> Array a
  -- ^ Contains will the covariance of the input 'Array's
cov x y (fromIntegral . fromEnum -> n) =
  op2 x y $ \a b c ->
    af_cov a b c n

-- | Calculates 'median' of 'Array' along user-specified dimension.
--
-- >>> median ( vector @Double 10 [1..] ) 0
-- ArrayFire Array
--   [1 1 1 1]
--      5.5000
median
  :: AFType a
  => Array a
  -- ^ Input 'Array'
  -> Int
  -- ^ Dimension along which to calculate 'median'
  -> Array a
  -- ^ Array containing 'median'
median a n =
  a `op1` (\x y ->
    af_median x y (fromIntegral n))

-- | Calculates 'mean' of all elements in an 'Array'
--
-- >>> meanAll $ matrix @Double (2,2) [[1,2],[4,5]]
-- (3.0,2.232709401e-314)
meanAll
  :: AFType a
  => Array a
  -- ^ Input 'Array'
  -> (Double, Double)
  -- ^ Mean result (real and imaginary part)
meanAll = (`infoFromArray2` af_mean_all)

-- | Calculates weighted mean of all elements in an 'Array'
--
-- >>> meanAllWeighted (matrix @Double (2,2) [[1,2],[3,4]]) (matrix @Double (2,2) [[1,2],[3,4]])
-- (3.0,1.400743288453e-312)
meanAllWeighted
  :: AFType a
  => Array a
  -- ^ Input 'Array'
  -> Array a
  -- ^ 'Array' of weights
  -> (Double, Double)
  -- ^ Weighted mean (real and imaginary part)
meanAllWeighted a b =
  infoFromArray22 a b af_mean_all_weighted

-- | Calculates variance of all elements in an 'Array'
--
-- >>> varAll (vector @Double 10 (repeat 10)) False
-- (0.0,1.4013073623e-312)
varAll
  :: AFType a
  => Array a
  -- ^ Input 'Array'
  -> Bool
  -- ^ Input 'Array'
  -> (Double, Double)
  -- ^ Variance (real and imaginary part)
varAll a (fromIntegral . fromEnum -> b) =
  infoFromArray2 a $ \x y z ->
    af_var_all x y z b

-- | Calculates weighted variance of all elements in an 'Array'
--
-- >>> varAllWeighted ( vector @Double 10 [1..] ) ( vector @Double 10 [1..] )
-- (6.0,2.1941097984e-314)
varAllWeighted
  :: AFType a
  => Array a
  -- ^ Input 'Array'
  -> Array a
  -- ^ 'Array' of weights
  -> (Double, Double)
  -- ^ Variance weighted result, (real and imaginary part)
varAllWeighted a b =
  infoFromArray22 a b af_var_all_weighted

-- | Calculates standard deviation of all elements in an 'Array'
--
-- >>> stdevAll (vector @Double 10 (repeat 10))
-- (0.0,2.190573324e-314)
stdevAll
  :: AFType a
  => Array a
  -- ^ Input 'Array'
  -> (Double, Double)
  -- ^ Standard deviation result, (real and imaginary part)
stdevAll = (`infoFromArray2` af_stdev_all)

-- | Calculates median of all elements in an 'Array'
--
-- >>> medianAll (vector @Double 10 (repeat 10))
-- (10.0,2.1961564713e-314)
medianAll
  :: (AFType a, Fractional a)
  => Array a
  -- ^ Input 'Array'
  -> (Double, Double)
  -- ^ Median result, real and imaginary part
medianAll = (`infoFromArray2` af_median_all)

-- | This algorithm returns Pearson product-moment correlation coefficient.
-- <https://en.wikipedia.org/wiki/Pearson_correlation_coefficient>
--
-- >>> corrCoef ( vector @Int 10 [1..] ) ( vector @Int 10 [10,9..] )
-- (-1.0,2.1904819737e-314)
corrCoef
  :: AFType a
  => Array a
  -- ^ First input 'Array'
  -> Array a
  -- ^ Second input 'Array'
  -> (Double, Double)
  -- ^ Correlation coefficient result, real and imaginary part
corrCoef a b =
  infoFromArray22 a b af_corrcoef

-- | This function returns the top k values along a given dimension of the input array.
--
-- @
-- >>> let (vals,indexes) = 'topk' ( 'vector' \@'Double' 10 [1..] ) 3 'TopKDefault'
-- >>> indexes
--
-- ArrayFire Array
-- [3 1 1 1]
--          9
--          8
--          7
--
-- >>> vals
-- ArrayFire Array
-- [3 1 1 1]
--    10.0000
--     9.0000
--     8.0000
-- @
--
-- The indices along with their values are returned. If the input is a multi-dimensional array, the indices will be the index of the value in that dimension. Order of duplicate values are not preserved. This function is optimized for small values of k.
-- This function performs the operation across all dimensions of the input array.
-- This function is optimized for small values of k.
-- The order of the returned keys may not be in the same order as the appear in the input array
--
topk
  :: AFType a
  => Array a
  -- ^ First input 'Array', with at least /k/ elements along /dim/
  -> Int
  -- ^ The number of elements to be retrieved along the dim dimension
  -> TopK
  -- ^  If descending, the highest values are returned. Otherwise, the lowest values are returned
  -> (Array a, Array a)
  -- ^ Returns The values of the top k elements along the dim dimension
  -- along with the indices of the top k elements along the dim dimension
topk a (fromIntegral -> x) (fromTopK -> f)
  = a `op2p` (\b c d -> af_topk b c d x 0 f)
