{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
module Main where

import           Data.Proxy
import           Spec                    (spec)
import           Test.Hspec              (hspec)
import           Test.QuickCheck
import           Test.QuickCheck.Classes

import qualified ArrayFire               as A
import           ArrayFire               (Array)

import           Foreign.C.Types          (CBool (..))

-- Multi-dimensional arrays: used for eqLaws, so the Eq instance is exercised
-- on matrices and tensors, not just scalars.
instance (A.AFType a, Arbitrary a) => Arbitrary (Array a) where
  arbitrary = do
    ndim  <- choose (1, 4)
    dims  <- vectorOf ndim (choose (1, 4))
    elems <- vectorOf (product dims) arbitrary
    pure (A.mkArray dims elems)
  shrink arr =
    [ A.mkArray dims' (take (product dims') (A.toList arr))
    | dims' <- shrunkDims
    , product dims' > 0
    ]
   where
    (d0, d1, d2, d3) = A.getDims arr
    ndim             = A.getNumDims arr
    currentDims      = take ndim [d0, d1, d2, d3]
    shrunkDims =
      [ [if i == j then d - 1 else d | (j, d) <- zip [0..] currentDims]
      | i <- [0 .. ndim - 1]
      , currentDims !! i > 1
      ]
      ++ [take (ndim - 1) currentDims | ndim > 1]

-- Scalar wrapper for numLaws.
-- Num laws require: (a) binary ops succeed for any two generated values, and
-- (b) `fromInteger 0` compares equal to `0 * x`.  Both hold only when all
-- arrays are the same shape.  Scalars ([1 1 1 1]) are the minimal fixed shape
-- that makes every Num law well-typed and exact for integer element types.
newtype Scalar a = Scalar (Array a)
  deriving (Show, Eq, Num)

instance Arbitrary CBool where
  arbitrary = CBool <$> arbitrary

instance (A.AFType a, Arbitrary a) => Arbitrary (Scalar a) where
  arbitrary = Scalar . A.scalar <$> arbitrary
  shrink (Scalar arr) = Scalar . A.scalar <$> case A.toList arr of
    x : _ -> shrink x
    []    -> []

main :: IO ()
main = do
  hspec spec
  -- IEEE 754 is not an exact ring; only Eq laws for floating-point arrays.
  lawsCheck (eqLaws (Proxy :: Proxy (Array Double)))
  lawsCheck (eqLaws (Proxy :: Proxy (Array Float)))
  lawsCheck (showLaws (Proxy :: Proxy (Array Float)))
  lawsCheck (showLaws (Proxy :: Proxy (Array Double)))
  -- Complex: Eq only (IEEE 754 + gt/lt undefined for complex numbers).
  lawsCheck (eqLaws (Proxy :: Proxy (Array (A.Complex Double))))
  lawsCheck (eqLaws (Proxy :: Proxy (Array (A.Complex Float))))
  lawsCheck (showLaws (Proxy :: Proxy (Array (A.Complex Double))))
  lawsCheck (showLaws (Proxy :: Proxy (Array (A.Complex Float))))
  -- Integral types: exact ring laws via Scalar, Eq laws via multi-dim Array.
  intChecks (Proxy :: Proxy Int)
  intChecks (Proxy :: Proxy A.Int16)
  intChecks (Proxy :: Proxy A.Int32)
  intChecks (Proxy :: Proxy A.Int64)
  intChecks (Proxy :: Proxy A.Word8)
  intChecks (Proxy :: Proxy A.Word16)
  intChecks (Proxy :: Proxy A.Word32)
  intChecks (Proxy :: Proxy A.Word64)
  intChecks (Proxy :: Proxy Word)
  intChecks (Proxy :: Proxy A.CBool)

intChecks :: forall a. (A.AFType a, Arbitrary a, Num a, Eq a) => Proxy a -> IO ()
intChecks _ = do
  lawsCheck (showLaws (Proxy :: Proxy (Array a)))
  lawsCheck (numLaws (Proxy :: Proxy (Scalar a)))
  lawsCheck (eqLaws  (Proxy :: Proxy (Array  a)))
