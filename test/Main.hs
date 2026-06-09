{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
module Main where

import           Prelude                 hiding (negate)
import           Control.Monad           (forM_, unless)
import           Data.IORef              (IORef, newIORef, readIORef, writeIORef)
import           Data.Proxy
import           Data.Semiring           (Semiring (..), Ring (..))
import           Spec                    (spec)
import           System.Exit             (exitFailure)
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

-- Semiring/Ring instances so we can exercise semiringLaws/ringLaws, which
-- check associativity, distributivity and annihilation explicitly (stronger
-- than numLaws).  Defined in terms of the derived Num instance; exact for the
-- integral element types these are instantiated at.
instance (A.AFType a, Num a) => Semiring (Scalar a) where
  zero          = 0
  one           = 1
  plus          = (+)
  times         = (*)
  fromNatural n = fromInteger (toInteger n)

instance (A.AFType a, Num a) => Ring (Scalar a) where
  negate x = 0 - x

instance Arbitrary CBool where
  arbitrary = CBool <$> arbitrary

instance (A.AFType a, Arbitrary a) => Arbitrary (Scalar a) where
  arbitrary = Scalar . A.scalar <$> arbitrary
  shrink (Scalar arr) = Scalar . A.scalar <$> case A.toList arr of
    x : _ -> shrink x
    []    -> []

-- Run a Laws check, print results in the same format as lawsCheck, and mark
-- the IORef False on any failure so we can call exitFailure at the end.
checkLaws :: IORef Bool -> Laws -> IO ()
checkLaws ref laws = do
  let cls = lawsTypeclass laws
  forM_ (lawsProperties laws) $ \(name, prop) -> do
    putStr $ cls ++ ": " ++ name ++ " "
    r <- quickCheckWithResult stdArgs { chatty = False } prop
    putStr (output r)
    unless (isSuccess r) (writeIORef ref False)

main :: IO ()
main = do
  ref <- newIORef True
  let check = checkLaws ref
  -- IEEE 754 is not an exact ring; only Eq laws for floating-point arrays.
  check (eqLaws (Proxy :: Proxy (Array Double)))
  check (eqLaws (Proxy :: Proxy (Array Float)))
  -- Complex: Eq only (IEEE 754 + gt/lt undefined for complex numbers).
  check (eqLaws (Proxy :: Proxy (Array (A.Complex Double))))
  check (eqLaws (Proxy :: Proxy (Array (A.Complex Float))))
  -- Integral types: exact ring laws via Scalar, Eq laws via multi-dim Array.
  intChecks ref (Proxy :: Proxy Int)
  intChecks ref (Proxy :: Proxy A.Int16)
  intChecks ref (Proxy :: Proxy A.Int32)
  intChecks ref (Proxy :: Proxy A.Int64)
  intChecks ref (Proxy :: Proxy A.Word8)
  intChecks ref (Proxy :: Proxy A.Word16)
  intChecks ref (Proxy :: Proxy A.Word32)
  intChecks ref (Proxy :: Proxy A.Word64)
  intChecks ref (Proxy :: Proxy Word)
  intChecks ref (Proxy :: Proxy A.CBool)
  hspec spec
  ok <- readIORef ref
  unless ok exitFailure

intChecks :: forall a. (A.AFType a, Arbitrary a, Num a, Eq a) => IORef Bool -> Proxy a -> IO ()
intChecks ref _ = do
  checkLaws ref (numLaws      (Proxy :: Proxy (Scalar a)))
  checkLaws ref (semiringLaws (Proxy :: Proxy (Scalar a)))
  checkLaws ref (ringLaws     (Proxy :: Proxy (Scalar a)))
  checkLaws ref (eqLaws       (Proxy :: Proxy (Array  a)))
