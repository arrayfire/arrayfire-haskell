{-# LANGUAGE ViewPatterns #-}
module ArrayFire.Statistics where

import Foreign.Marshal
import Foreign.Storable
import Foreign.C.String

import ArrayFire.Internal.Statistics
import ArrayFire.Internal.Defines

import ArrayFire.Exception
import ArrayFire.Types
import ArrayFire.FFI

mean
  :: AFType a
  => Array a
  -> Int -- * dimension
  -> Array a
mean x n =
  x `op1` (\x y ->
    af_mean x y (fromIntegral n))

meanWeighted
  :: AFType a
  => Array a
  -> Array a
  -> Int -- * dimension
  -> Array a
meanWeighted x y (fromIntegral -> n) =
  op2 x y $ \a b c ->
    af_mean_weighted a b c n

var
  :: AFType a
  => Array a
  -> Bool
  -> Int  -- * dimension
  -> Array a
var arr b d =
  arr `op1` (\p x ->
    af_var p x b (fromIntegral d))