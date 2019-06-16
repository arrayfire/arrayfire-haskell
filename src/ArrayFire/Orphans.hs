{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module ArrayFire.Orphans where

import           Prelude

import qualified ArrayFire.Arith as A
import qualified ArrayFire.Array as A
import qualified ArrayFire.BLAS  as A
import           ArrayFire.Types
import           ArrayFire.Util

instance (AFType a, Eq a) => Eq (Array a) where
  x == y = A.getScalarBool (A.eq x y False)
  x /= y = A.getScalarBool (A.neq x y False)

instance (Num a, AFType a) => Num (Array a) where
  x + y       = A.add x y False
  x * y       = A.mul x y False
  abs         = A.abs
  signum      = A.sign
  negate      = error "TODO: negate"
  x - y       = A.sub x y False
  fromInteger = A.scalar . fromIntegral

instance (Ord a, AFType a) => Ord (Array a) where
  x < y  = A.getScalarBool (A.lt x y False)
  x > y  = A.getScalarBool (A.gt x y False)
  x <= y = A.getScalarBool (A.le x y False)
  x >= y = A.getScalarBool (A.ge x y False)

instance AFType a => Semigroup (Array a) where
  x <> y = A.matmul x y None None

instance Show (Array a) where
  show = arrayString

instance forall a . (Fractional a, AFType a) => Fractional (Array a) where
  x / y  = A.div x y False
  fromRational n = A.scalar @a (fromRational n)

-- instance Bits (Array Bool) where
--   x .&. y = A.bitAnd x y False
--   x .|. y = A.bitOr x y False
--   x `xor` y = A.bitXor x y False
--   complement = error "TODO: implement complement"
--   shiftL x _ = A.bitShiftL x undefined False
--   shiftR x _ = A.bitShiftR x undefined False
--   rotateL x _ = undefined
--   rotateR x _ = undefined

