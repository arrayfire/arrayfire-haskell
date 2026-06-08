{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module ArrayFire.ArithSpec where

import ArrayFire (AFType, Array, cast, clamp, getType, isInf, isZero, matrix, maxOf, minOf, mkArray, scalar, vector)
import qualified ArrayFire
import Control.Exception (throwIO)
import Control.Monad (unless, when)
import Foreign.C
import GHC.Exts (IsList (..))
import GHC.Stack
import Test.HUnit.Lang (FailureReason (..), HUnitFailure (..))
import Test.Hspec
import Test.Hspec.QuickCheck
import Prelude hiding (div)

compareWith :: (HasCallStack, Show a) => (a -> a -> Bool) -> a -> a -> Expectation
compareWith comparator result expected =
  unless (comparator result expected) $ do
    throwIO (HUnitFailure location $ ExpectedButGot Nothing expectedMsg actualMsg)
 where
  expectedMsg = show expected
  actualMsg = show result
  location = case reverse (toList callStack) of
    (_, loc) : _ -> Just loc
    [] -> Nothing

class (Num a) => HasEpsilon a where
  eps :: a

instance HasEpsilon Float where
  eps = 1.1920929e-7

instance HasEpsilon Double where
  eps = 2.220446049250313e-16

approxWith :: (Ord a, Num a) => a -> a -> a -> a -> Bool
approxWith rtol atol a b = abs (a - b) <= Prelude.max atol (rtol * Prelude.max (abs a) (abs b))

approx :: (Ord a, HasEpsilon a) => a -> a -> Bool
approx a b = approxWith (2 * eps * Prelude.max (abs a) (abs b)) (4 * eps) a b

shouldBeApprox :: (Ord a, HasEpsilon a, Show a) => a -> a -> Expectation
shouldBeApprox = compareWith approx

evalf :: (AFType a) => Array a -> a
evalf = ArrayFire.getScalar

shouldMatchBuiltin ::
  (AFType a, Ord a, RealFloat a, HasEpsilon a, Show a) =>
  (Array a -> Array a) ->
  (a -> a) ->
  a ->
  Expectation
shouldMatchBuiltin f f' x
  | isInfinite y && isInfinite y' = pure ()
  | Prelude.isNaN y && Prelude.isNaN y' = pure ()
  | otherwise = y `shouldBeApprox` y'
 where
  y = evalf (f (scalar x))
  y' = f' x

shouldMatchBuiltin2 ::
  (AFType a, Ord a, RealFloat a, HasEpsilon a, Show a) =>
  (Array a -> Array a -> Array a) ->
  (a -> a -> a) ->
  a ->
  a ->
  Expectation
shouldMatchBuiltin2 f f' a = shouldMatchBuiltin (f (scalar a)) (f' a)

spec :: Spec
spec =
  describe "Arith tests" $ do
    it "Should negate scalar value" $ do
      negate (scalar @Int 1) `shouldBe` (-1)
    it "Should negate a vector" $ do
      negate (vector @Int 3 [2, 2, 2]) `shouldBe` vector @Int 3 [-2, -2, -2]
    it "Should add two scalar arrays" $ do
      scalar @Int 1 + 2 `shouldBe` 3
    it "Should add two scalar bool arrays" $ do
      scalar @CBool 1 + 0 `shouldBe` 1
    it "Should subtract two scalar arrays" $ do
      scalar @Int 4 - 2 `shouldBe` 2
    it "Should multiply two scalar arrays" $ do
      scalar @Double 4 `ArrayFire.mul` 2 `shouldBe` 8
    it "Should divide two scalar arrays" $ do
      ArrayFire.div @Double 8 2 `shouldBe` 4
    it "Should add two matrices" $ do
      matrix @Int (2, 2) [[1, 1], [1, 1]] + matrix @Int (2, 2) [[1, 1], [1, 1]]
        `shouldBe` matrix @Int (2, 2) [[2, 2], [2, 2]]
    prop "Should take cubed root" $ \(x :: Double) ->
      evalf (ArrayFire.cbrt (scalar (x * x * x))) `shouldBeApprox` x

    it "Should lte Array" $ do
      2 `ArrayFire.le` (3 :: Array Double) `shouldBe` 1
    it "Should gte Array" $ do
      2 `ArrayFire.ge` (3 :: Array Double) `shouldBe` 0
    it "Should gt Array" $ do
      2 `ArrayFire.gt` (3 :: Array Double) `shouldBe` 0
    it "Should lt Array" $ do
      2 `ArrayFire.le` (3 :: Array Double) `shouldBe` 1
    it "Should eq Array" $ do
      3 == (3 :: Array Double) `shouldBe` True
    it "Should and Array" $ do
      (mkArray @CBool [1] [0] `ArrayFire.and` mkArray [1] [1])
        `shouldBe` mkArray [1] [0]
    it "Should and Array" $ do
      (mkArray @CBool [2] [0, 0] `ArrayFire.and` mkArray [2] [1, 0])
        `shouldBe` mkArray [2] [0, 0]
    it "Should or Array" $ do
      (mkArray @CBool [2] [0, 0] `ArrayFire.or` mkArray [2] [1, 0])
        `shouldBe` mkArray [2] [1, 0]
    it "Should not Array" $ do
      ArrayFire.not (mkArray @CBool [2] [1, 0]) `shouldBe` mkArray [2] [0, 1]
    it "Should bitwise and array" $ do
      ArrayFire.bitAnd (scalar @Int 1) (scalar @Int 0)
        `shouldBe` 0
    it "Should bitwise or array" $ do
      ArrayFire.bitOr (scalar @Int 1) (scalar @Int 0)
        `shouldBe` 1
    it "Should bitwise xor array" $ do
      ArrayFire.bitXor (scalar @Int 1) (scalar @Int 1)
        `shouldBe` 0
    it "Should bitwise shift left an array" $ do
      ArrayFire.bitShiftL (scalar @Int 1) (scalar @Int 3)
        `shouldBe` 8
    it "Should cast an array" $ do
      getType (cast (scalar @Int 1) :: Array Double)
        `shouldBe` ArrayFire.F64
    it "Should find the minimum of two arrays" $ do
      minOf (scalar @Int 1) (scalar @Int 0)
        `shouldBe` 0
    it "Should find the max of two arrays" $ do
      maxOf (scalar @Int 1) (scalar @Int 0)
        `shouldBe` 1
    it "Should take the clamp of 3 arrays" $ do
      clamp (scalar @Int 2) (scalar @Int 1) (scalar @Int 3)
        `shouldBe` 2
    it "Should check if an array has positive or negative infinities" $ do
      isInf (scalar @Double (1 / 0)) `shouldBe` scalar @Double 1
      isInf (scalar @Double 10) `shouldBe` scalar @Double 0
    it "Should check if an array has any NaN values" $ do
      ArrayFire.isNaN (scalar @Double (acos 2)) `shouldBe` scalar @Double 1
      ArrayFire.isNaN (scalar @Double 10) `shouldBe` scalar @Double 0
    it "Should check if an array has any Zero values" $ do
      isZero (scalar @Double (acos 2)) `shouldBe` scalar @Double 0
      isZero (scalar @Double 0) `shouldBe` scalar @Double 1
      isZero (scalar @Double 1) `shouldBe` scalar @Double 0

    prop "Floating @Float (exp)" $ \(x :: Float) -> exp `shouldMatchBuiltin` exp $ x
    prop "Floating @Float (log)" $ \(x :: Float) -> log `shouldMatchBuiltin` log $ x
    prop "Floating @Float (sqrt)" $ \(x :: Float) -> sqrt `shouldMatchBuiltin` sqrt $ x
    prop "Floating @Float (**)" $ \(x :: Float) (y :: Float) -> ((**) `shouldMatchBuiltin2` (**)) x y
    prop "Floating @Float (sin)" $ \(x :: Float) -> sin `shouldMatchBuiltin` sin $ x
    prop "Floating @Float (cos)" $ \(x :: Float) -> cos `shouldMatchBuiltin` cos $ x
    prop "Floating @Float (tan)" $ \(x :: Float) -> tan `shouldMatchBuiltin` tan $ x
    prop "Floating @Float (asin)" $ \(x :: Float) -> asin `shouldMatchBuiltin` asin $ x
    prop "Floating @Float (acos)" $ \(x :: Float) -> acos `shouldMatchBuiltin` acos $ x
    prop "Floating @Float (atan)" $ \(x :: Float) -> atan `shouldMatchBuiltin` atan $ x
    prop "Floating @Float (sinh)" $ \(x :: Float) -> sinh `shouldMatchBuiltin` sinh $ x
    prop "Floating @Float (cosh)" $ \(x :: Float) -> cosh `shouldMatchBuiltin` cosh $ x
    prop "Floating @Float (tanh)" $ \(x :: Float) -> tanh `shouldMatchBuiltin` tanh $ x
    prop "Floating @Float (asinh)" $ \(x :: Float) -> asinh `shouldMatchBuiltin` asinh $ x
    prop "Floating @Float (acosh)" $ \(x :: Float) -> acosh `shouldMatchBuiltin` acosh $ x
    prop "Floating @Float (atanh)" $ \(x :: Float) -> atanh `shouldMatchBuiltin` atanh $ x
