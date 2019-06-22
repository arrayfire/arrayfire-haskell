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
-- Various instances for 'Array'
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
  x == y = toEnum . fromIntegral $ A.getScalar @CBool @a $! A.eq x y False
  x /= y = toEnum . fromIntegral $ A.getScalar @CBool @a $! A.neq x y False

instance (Num a, AFType a) => Num (Array a) where
  x + y       = A.add x y False
  x * y       = A.mul x y False
  abs         = A.abs
  signum      = A.sign
  negate arr  = do
    let (w,x,y,z) = A.getDims arr
    A.cast (A.constant [w,x,y,z] 0.0) `A.sub` arr $ False
  x - y       = A.sub x y False
  fromInteger = A.scalar . fromIntegral

instance (Ord a, AFType a) => Ord (Array a) where
  x < y  = toEnum . fromIntegral $ A.getScalar @CBool @a (A.lt x y False)
  x > y  = toEnum . fromIntegral $ A.getScalar @CBool @a (A.gt x y False)
  x <= y = toEnum . fromIntegral $ A.getScalar @CBool @a (A.le x y False)
  x >= y = toEnum . fromIntegral $ A.getScalar @CBool @a (A.ge x y False)

instance AFType a => Semigroup (Array a) where
  x <> y = A.mul x y False

instance Show (Array a) where
  show = arrayString

instance forall a . (Fractional a, AFType a) => Fractional (Array a) where
  x / y  = A.div x y False
  fromRational n = A.scalar @a (fromRational n)
