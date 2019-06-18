{-# LANGUAGE ViewPatterns #-}
--------------------------------------------------------------------------------
-- |
-- Module      : ArrayFire.Statistics
-- Copyright   : David Johnson (c) 2019-2020
-- License     : BSD 3
-- Maintainer  : David Johnson <djohnson.m@gmail.com>
-- Stability   : Experimental
-- Portability : GHC
--
--------------------------------------------------------------------------------
module ArrayFire.Statistics where

import ArrayFire.Internal.Statistics
import ArrayFire.Internal.Defines
import ArrayFire.Types
import ArrayFire.FFI

mean
  :: AFType a
  => Array a
  -> Int
  -> Array a
mean a n =
  a `op1` (\x y ->
    af_mean x y (fromIntegral n))

meanWeighted
  :: AFType a
  => Array a
  -> Array a
  -> Int
  -> Array a
meanWeighted x y (fromIntegral -> n) =
  op2 x y $ \a b c ->
    af_mean_weighted a b c n

var
  :: AFType a
  => Array a
  -> Bool
  -> Int
  -> Array a
var arr b d =
  arr `op1` (\p x ->
    af_var p x b (fromIntegral d))

varWeighted
  :: AFType a
  => Array a
  -> Array a
  -> Int
  -> Array a
varWeighted x y (fromIntegral -> n) =
  op2 x y $ \a b c ->
    af_var_weighted a b c n

stdev
  :: AFType a
  => Array a
  -> Int
  -> Array a
stdev a n =
  a `op1` (\x y ->
    af_stdev x y (fromIntegral n))

cov
  :: AFType a
  => Array a
  -> Array a
  -> Bool
  -> Array a
cov x y n =
  op2 x y $ \a b c ->
    af_cov a b c n

median
  :: AFType a
  => Array a
  -> Int
  -> Array a
median a n =
  a `op1` (\x y ->
    af_median x y (fromIntegral n))

meanAll
  :: AFType a
  => Array a
  -> (Double, Double)
meanAll = (`infoFromArray2` af_mean_all)

meanAllWeighted
  :: AFType a
  => Array a
  -> Array a
  -> (Double, Double)
meanAllWeighted a b =
  infoFromArray22 a b af_mean_all_weighted

varAll
  :: AFType a
  => Array a
  -> Bool
  -> (Double, Double)
varAll a b =
  infoFromArray2 a $ \x y z ->
    af_var_all x y z b

varAllWeighted
  :: AFType a
  => Array a
  -> Array a
  -> (Double, Double)
varAllWeighted a b =
  infoFromArray22 a b af_var_all_weighted

stdevAll
  :: AFType a
  => Array a
  -> (Double, Double)
stdevAll = (`infoFromArray2` af_stdev_all)

medianAll
  :: AFType a
  => Array a
  -> (Double, Double)
medianAll = (`infoFromArray2` af_stdev_all)

corrCoef
  :: AFType a
  => Array a
  -> Array a
  -> (Double, Double)
corrCoef a b =
  infoFromArray22 a b af_corrcoef

topk
  :: AFType a
  => Array a
  -> Int
  -> Int
  -> AFTopkFunction
  -> (Array a, Array a)
topk a x y f
  = a `op2p` (\b c d -> af_topk b c d x y f)
