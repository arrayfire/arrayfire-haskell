{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
--------------------------------------------------------------------------------
-- |
-- Module      : ArrayFire.Orphans
-- Copyright   : David Johnson (c) 2019-2020
-- License     : BSD 3
-- Maintainer  : David Johnson <djohnson.m@gmail.com>
-- Stability   : Experimental
-- Portability : GHC
--
--------------------------------------------------------------------------------
module ArrayFire.Orphans where

import           Prelude

import qualified ArrayFire.Arith     as A
import qualified ArrayFire.Array     as A
import qualified ArrayFire.Algorithm as A
import qualified ArrayFire.Data      as A
import           ArrayFire.Types
import           ArrayFire.Util

instance (AFType a, Eq a) => Eq (Array a) where
  x == y = A.allTrueAll (A.eqBatched x y False) == (1.0,0.0)
  x /= y = A.allTrueAll (A.neqBatched x y False) == (0.0,0.0)

instance (Num a, AFType a) => Num (Array a) where
  x + y       = A.add x y
  x * y       = A.mul x y
  abs         = A.abs
  signum      = A.sign
  negate arr  = do
    let (w,x,y,z) = A.getDims arr
    A.cast (A.constant @a [w,x,y,z] 0) `A.sub` arr
  x - y       = A.sub x y
  fromInteger = A.scalar . fromIntegral

instance Show (Array a) where
  show = arrayString

instance forall a . (Fractional a, AFType a) => Fractional (Array a) where
  x / y  = A.div x y
  fromRational n = A.scalar @a (fromRational n)

instance forall a . (Ord a, AFType a, Fractional a) => Floating (Array a) where
  pi   = A.scalar @a 3.14159
  exp  = A.exp @a
  log  = A.log @a
  sqrt = A.sqrt @a
  (**) = A.pow @a
  sin  = A.sin @a
  cos  = A.cos @a
  tan = A.tan @a
  tanh = A.tanh @a
  asin = A.asin @a
  acos = A.acos @a
  atan = A.atan @a
  sinh = A.sinh @a
  cosh = A.cosh @a
  acosh = A.acosh @a
  atanh = A.atanh @a
  asinh = A.asinh @a
