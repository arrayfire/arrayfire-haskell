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

import qualified ArrayFire.Arith as A
import qualified ArrayFire.Array as A
import qualified ArrayFire.Data  as A
import           ArrayFire.Types
import           ArrayFire.Util
import           Foreign.C

instance (AFType a, Eq a) => Eq (Array a) where
  x == y = toEnum . fromIntegral $ A.getScalar @CBool @a $! A.eq x y
  x /= y = toEnum . fromIntegral $ A.getScalar @CBool @a $! A.neq x y

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

instance (Ord a, AFType a) => Ord (Array a) where
  x < y  = toEnum . fromIntegral $ A.getScalar @CBool @a (A.lt x y)
  x > y  = toEnum . fromIntegral $ A.getScalar @CBool @a (A.gt x y)
  x <= y = toEnum . fromIntegral $ A.getScalar @CBool @a (A.le x y)
  x >= y = toEnum . fromIntegral $ A.getScalar @CBool @a (A.ge x y)

instance AFType a => Semigroup (Array a) where
  x <> y = A.mul x y

instance Show (Array a) where
  show = arrayString

instance forall a . (Fractional a, AFType a) => Fractional (Array a) where
  x / y  = A.div x y
  fromRational n = A.scalar @a (fromRational n)

instance forall a . (Ord a, AFType a, Fractional a) => Floating (Array a) where
  pi   = A.scalar @a 3.14159
  exp  = A.exp @a
  log  = A.log @a
  sin  = A.sin @a
  cos  = A.cos @a
  asin = A.asin @a
  acos = A.acos @a
  atan = A.atan @a
  sinh = A.sinh @a
  cosh = A.cosh @a
  acosh x =
    A.log (x + (x+1.0) * A.sqrt ((x-1.0)/(x+1.0)))
  atanh x =
    0.5 * A.log ((1.0+x) / (1.0-x))
  asinh x
    | x > huge  = A.log 2 + A.log x
    | x < 0     = -asinh (-x)
    | otherwise = A.log (x + A.sqrt (1 + x*x))
       where
         huge = 1e20


