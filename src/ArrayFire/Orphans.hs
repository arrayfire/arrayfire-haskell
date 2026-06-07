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
-- Maintainer  : David Johnson <code@dmj.io>
-- Stability   : Experimental
-- Portability : GHC
--
--------------------------------------------------------------------------------
module ArrayFire.Orphans where

import           Prelude hiding (pi)
import qualified Prelude

import           Control.DeepSeq (NFData(..))

import qualified ArrayFire.Arith     as A
import qualified ArrayFire.Array     as A
import qualified ArrayFire.Algorithm as A
import           ArrayFire.Types
import           ArrayFire.Util

instance NFData (Array a) where
  rnf x = x `seq` ()

instance (AFType a, Eq a) => Eq (Array a) where
  x == y = A.getDims x == A.getDims y
        && A.allTrueAll (A.eqBatched x y False) == (1.0,0.0)
  x /= y = A.getDims x /= A.getDims y
        || A.anyTrueAll (A.neqBatched x y False) /= (0.0,0.0)

instance (Num a, AFType a) => Num (Array a) where
  x + y       = A.add x y
  x * y       = A.mul x y
  abs         = A.abs
  signum x    = A.cast (A.gt x 0) - A.cast (A.lt x 0)
  negate arr  = A.scalar @a (fromInteger (-1)) `A.mul` arr
  x - y       = A.sub x y
  fromInteger = A.scalar . fromIntegral

instance Show (Array a) where
  show = arrayString

instance forall a . (Fractional a, AFType a) => Fractional (Array a) where
  x / y  = A.div x y
  fromRational n = A.scalar @a (fromRational n)

instance forall a . (Ord a, AFType a, Fractional a) => Floating (Array a) where
  pi   = A.scalar @a (realToFrac (Prelude.pi :: Double))
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
