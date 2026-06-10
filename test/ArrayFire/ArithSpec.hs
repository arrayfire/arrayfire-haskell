{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module ArrayFire.ArithSpec where

import ArrayFire (AFType, Array, cast, clamp, cplx, cplx2, getType, imag, isInf, isZero, matrix, maxOf, minOf, mkArray, real, scalar, vector)
import qualified ArrayFire
import Data.Complex (Complex (..))
import Control.Exception (throwIO)
import Control.Monad (unless, when)
import Foreign.C
import GHC.Exts (IsList (..))
import GHC.Stack
import Test.HUnit.Lang (FailureReason (..), HUnitFailure (..))
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck ((==>))
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

-- | Relative + absolute tolerance check at machine-epsilon scale.
-- Tolerance = max(4*eps, 2*eps * max(|a|,|b|)).
approx :: (Ord a, HasEpsilon a) => a -> a -> Bool
approx a b = approxWith (2 * eps) (4 * eps) a b

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
      let x3 = x * x * x
      in not (isNaN x3 || isInfinite x3) ==>
         evalf (ArrayFire.cbrt (scalar x3)) `shouldBeApprox` x

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
      isInf (scalar @Double (1 / 0)) `shouldBe` scalar @CBool 1
      isInf (scalar @Double 10) `shouldBe` scalar @CBool 0
    it "Should check if an array has any NaN values" $ do
      ArrayFire.isNaN (scalar @Double (acos 2)) `shouldBe` scalar @CBool 1
      ArrayFire.isNaN (scalar @Double 10) `shouldBe` scalar @CBool 0
    it "Should check if an array has any Zero values" $ do
      isZero (scalar @Double (acos 2)) `shouldBe` scalar @CBool 0
      isZero (scalar @Double 0) `shouldBe` scalar @CBool 1
      isZero (scalar @Double 1) `shouldBe` scalar @CBool 0

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

    describe "erf" $ do
      it "erf 0 = 0" $
        evalf (ArrayFire.erf (scalar @Double 0)) `shouldBeApprox` 0
      it "erf 1 ≈ 0.8427" $
        evalf (ArrayFire.erf (scalar @Double 1)) `shouldBeApprox` 0.8427007929497149
      it "erf is odd: erf(-x) = -erf(x)" $
        evalf (ArrayFire.erf (scalar @Double (-1))) `shouldBeApprox`
          negate (evalf (ArrayFire.erf (scalar @Double 1)))

    describe "erfc" $ do
      it "erfc 0 = 1" $
        evalf (ArrayFire.erfc (scalar @Double 0)) `shouldBeApprox` 1
      it "erf(x) + erfc(x) = 1" $ do
        let x = scalar @Double 1.5
        (evalf (ArrayFire.erf x) + evalf (ArrayFire.erfc x)) `shouldBeApprox` 1

    describe "sigmoid" $ do
      it "sigmoid 0 = 0.5" $
        evalf (ArrayFire.sigmoid (scalar @Double 0)) `shouldBeApprox` 0.5
      it "sigmoid(-x) = 1 - sigmoid(x)" $ do
        let x = scalar @Double 2.0
        evalf (ArrayFire.sigmoid (negate x))
          `shouldBeApprox`
          (1 - evalf (ArrayFire.sigmoid x))

    describe "expm1" $ do
      it "expm1 0 = 0" $
        evalf (ArrayFire.expm1 (scalar @Double 0)) `shouldBeApprox` 0
      it "expm1 1 = e - 1" $
        evalf (ArrayFire.expm1 (scalar @Double 1)) `shouldBeApprox` (exp 1 - 1)

    describe "clamp (vector)" $ do
      it "clamps each element to [lo, hi]" $
        clamp (vector @Int 5 [0,1,5,9,10])
              (scalar @Int 2)
              (scalar @Int 8)
          `shouldBe` vector @Int 5 [2,2,5,8,8]

    describe "signum" $ do
      it "positive Int → 1" $
        signum (scalar @Int 5) `shouldBe` scalar @Int 1
      it "negative Int → -1" $
        signum (scalar @Int (-3)) `shouldBe` scalar @Int (-1)
      it "zero Int → 0" $
        signum (scalar @Int 0) `shouldBe` scalar @Int 0
      -- unsigned: old sign(-x) - sign(x) wrapped, making signum always 0
      it "positive Word32 → 1 (unsigned negate wraps)" $
        signum (scalar @ArrayFire.Word32 7) `shouldBe` scalar @ArrayFire.Word32 1
      it "zero Word32 → 0" $
        signum (scalar @ArrayFire.Word32 0) `shouldBe` scalar @ArrayFire.Word32 0
      -- IEEE 754: af_sign checks the sign bit, so sign(-0.0) = 1 → old signum(0.0) = 1
      it "negative zero Double → 0 (IEEE 754 -0.0)" $
        evalf (signum (scalar @Double (-0.0))) `shouldBeApprox` 0
      it "positive Double → 1" $
        evalf (signum (scalar @Double 2.5)) `shouldBeApprox` 1
      it "negative Double → -1" $
        evalf (signum (scalar @Double (-2.5))) `shouldBeApprox` (-1)
      it "signum vector" $
        signum (vector @Int 3 [-4, 0, 7]) `shouldBe` vector @Int 3 [-1, 0, 1]

    describe "cplx" $ do
      it "lifts a real scalar to complex with zero imaginary part" $
        cplx (scalar @Double 5.0) `shouldBe` scalar @(Complex Double) (5.0 :+ 0.0)
      it "real . cplx == id on a vector" $ do
        let v = vector @Double 4 [1, 2, 3, 4]
        (real (cplx v) :: Array Double) `shouldBe` v
      it "imag . cplx == 0 on a vector" $ do
        let v = vector @Double 4 [1, 2, 3, 4]
        ArrayFire.toList (imag (cplx v) :: Array Double) `shouldBe` [0, 0, 0, 0]

    describe "cplx2" $ do
      it "combines real and imaginary parts into a complex scalar" $
        cplx2 (scalar @Double 3.0) (scalar @Double 4.0)
          `shouldBe` scalar @(Complex Double) (3.0 :+ 4.0)
      it "real . cplx2 r i == r" $ do
        let r = vector @Double 3 [1, 2, 3]
            i = vector @Double 3 [4, 5, 6]
        (real (cplx2 r i) :: Array Double) `shouldBe` r
      it "imag . cplx2 r i == i" $ do
        let r = vector @Double 3 [1, 2, 3]
            i = vector @Double 3 [4, 5, 6]
        (imag (cplx2 r i) :: Array Double) `shouldBe` i

    describe "real / imag" $ do
      it "real extracts the real part of a complex scalar" $
        (real (scalar @(Complex Double) (7.0 :+ 3.0)) :: Array Double)
          `shouldBe` scalar @Double 7.0
      it "imag extracts the imaginary part of a complex scalar" $
        (imag (scalar @(Complex Double) (7.0 :+ 3.0)) :: Array Double)
          `shouldBe` scalar @Double 3.0
      it "real and imag round-trip via cplx2" $ do
        let c = vector @(Complex Double) 3 [1:+2, 3:+4, 5:+6]
        cplx2 (real c :: Array Double) (imag c :: Array Double) `shouldBe` c

    describe "factorial" $ do
      it "factorial 0 = 1" $
        evalf (ArrayFire.factorial (scalar @Double 0)) `shouldBeApprox` 1
      it "factorial 5 = 120" $
        evalf (ArrayFire.factorial (scalar @Double 5)) `shouldBeApprox` 120
      it "factorial 10 = 3628800" $
        evalf (ArrayFire.factorial (scalar @Double 10)) `shouldBeApprox` 3628800

    describe "floor" $ do
      it "floor of 1.7 is 1" $
        evalf (ArrayFire.floor (scalar @Double 1.7)) `shouldBeApprox` 1
      it "floor of -1.2 is -2" $
        evalf (ArrayFire.floor (scalar @Double (-1.2))) `shouldBeApprox` (-2)
      it "floor of exact integer is unchanged" $
        evalf (ArrayFire.floor (scalar @Double 3.0)) `shouldBeApprox` 3

    describe "ceil" $ do
      it "ceil of 1.2 is 2" $
        evalf (ArrayFire.ceil (scalar @Double 1.2)) `shouldBeApprox` 2
      it "ceil of -1.7 is -1" $
        evalf (ArrayFire.ceil (scalar @Double (-1.7))) `shouldBeApprox` (-1)
      it "ceil of exact integer is unchanged" $
        evalf (ArrayFire.ceil (scalar @Double 4.0)) `shouldBeApprox` 4

    describe "trunc" $ do
      it "trunc of 1.9 is 1" $
        evalf (ArrayFire.trunc (scalar @Double 1.9)) `shouldBeApprox` 1
      it "trunc of -1.9 is -1" $
        evalf (ArrayFire.trunc (scalar @Double (-1.9))) `shouldBeApprox` (-1)
      it "trunc of exact integer is unchanged" $
        evalf (ArrayFire.trunc (scalar @Double 5.0)) `shouldBeApprox` 5

    describe "log10" $ do
      it "log10 of 100 is 2" $
        evalf (ArrayFire.log10 (scalar @Double 100)) `shouldBeApprox` 2
      it "log10 of 1 is 0" $
        evalf (ArrayFire.log10 (scalar @Double 1)) `shouldBeApprox` 0

    describe "log2" $ do
      it "log2 of 8 is 3" $
        evalf (ArrayFire.log2 (scalar @Double 8)) `shouldBeApprox` 3
      it "log2 of 1 is 0" $
        evalf (ArrayFire.log2 (scalar @Double 1)) `shouldBeApprox` 0

    describe "log1p" $ do
      it "log1p 0 = 0" $
        evalf (ArrayFire.log1p (scalar @Double 0)) `shouldBeApprox` 0
      it "log1p (e-1) = 1" $
        evalf (ArrayFire.log1p (scalar @Double (exp 1 - 1))) `shouldBeApprox` 1

    describe "pow" $ do
      it "2^10 = 1024" $
        ArrayFire.pow (scalar @Int 2) (scalar @Int 10) `shouldBe` scalar @Int 1024
      it "3^3 = 27" $
        ArrayFire.pow (scalar @Int 3) (scalar @Int 3) `shouldBe` scalar @Int 27

    describe "pow2" $ do
      it "pow2 1 = 2" $
        ArrayFire.pow2 (scalar @Int 1) `shouldBe` scalar @Int 2
      it "pow2 4 = 16" $
        ArrayFire.pow2 (scalar @Int 4) `shouldBe` scalar @Int 16
      it "pow2 0 = 1" $
        ArrayFire.pow2 (scalar @Int 0) `shouldBe` scalar @Int 1

    describe "root" $ do
      it "cube root of 8 is 2" $
        evalf (ArrayFire.root (scalar @Double 8) (scalar @Double 3)) `shouldBeApprox` 2
      it "square root of 9 is 3" $
        evalf (ArrayFire.root (scalar @Double 9) (scalar @Double 2)) `shouldBeApprox` 3

    describe "arg" $ do
      it "arg of a positive real scalar is 0" $
        evalf (ArrayFire.arg (scalar @Double 5)) `shouldBeApprox` 0
      it "arg of 0 is 0" $
        evalf (ArrayFire.arg (scalar @Double 0)) `shouldBeApprox` 0

    describe "atan2" $ do
      it "atan2(1,1) = pi/4" $
        evalf (ArrayFire.atan2 (scalar @Double 1) (scalar @Double 1))
          `shouldBeApprox` (pi / 4)
      it "atan2(0,1) = 0" $
        evalf (ArrayFire.atan2 (scalar @Double 0) (scalar @Double 1))
          `shouldBeApprox` 0

    describe "lgamma" $ do
      it "lgamma 1 = 0" $
        evalf (ArrayFire.lgamma (scalar @Double 1)) `shouldBeApprox` 0
      it "lgamma 0.5 = log(sqrt(pi))" $
        evalf (ArrayFire.lgamma (scalar @Double 0.5)) `shouldBeApprox` log (sqrt pi)

    describe "tgamma" $ do
      it "tgamma 1 = 1" $
        evalf (ArrayFire.tgamma (scalar @Double 1)) `shouldBeApprox` 1
      it "tgamma 5 = 24 (= 4!)" $
        evalf (ArrayFire.tgamma (scalar @Double 5)) `shouldBeApprox` 24
      it "tgamma 0.5 = sqrt(pi)" $
        evalf (ArrayFire.tgamma (scalar @Double 0.5)) `shouldBeApprox` (sqrt pi)

    describe "addBatched" $ do
      it "adds two scalars (batch=True)" $
        (scalar @Int 3 `ArrayFire.addBatched` scalar @Int 4) True `shouldBe` scalar @Int 7
      it "adds two scalars (batch=False)" $
        (scalar @Int 10 `ArrayFire.addBatched` scalar @Int 5) False `shouldBe` scalar @Int 15

    describe "subBatched" $ do
      it "subtracts two scalars (batch=True)" $
        (scalar @Int 9 `ArrayFire.subBatched` scalar @Int 4) True `shouldBe` scalar @Int 5
      it "subtracts two scalars (batch=False)" $
        (scalar @Int 10 `ArrayFire.subBatched` scalar @Int 3) False `shouldBe` scalar @Int 7

    describe "mulBatched" $ do
      it "multiplies two scalars (batch=True)" $
        (scalar @Int 3 `ArrayFire.mulBatched` scalar @Int 5) True `shouldBe` scalar @Int 15
      it "multiplies two scalars (batch=False)" $
        (scalar @Int 6 `ArrayFire.mulBatched` scalar @Int 7) False `shouldBe` scalar @Int 42

    describe "divBatched" $ do
      it "divides two scalars (batch=True)" $
        (scalar @Int 12 `ArrayFire.divBatched` scalar @Int 4) True `shouldBe` scalar @Int 3
      it "divides two scalars (batch=False)" $
        (scalar @Int 20 `ArrayFire.divBatched` scalar @Int 5) False `shouldBe` scalar @Int 4

    describe "eqBatched" $ do
      it "equal scalars return 1 (batch=False)" $
        (scalar @Int 5 `ArrayFire.eqBatched` scalar @Int 5) False `shouldBe` scalar @CBool 1
      it "unequal scalars return 0 (batch=False)" $
        (scalar @Int 5 `ArrayFire.eqBatched` scalar @Int 6) False `shouldBe` scalar @CBool 0

    describe "neqBatched" $ do
      it "unequal scalars return 1 (batch=False)" $
        (scalar @Int 5 `ArrayFire.neqBatched` scalar @Int 6) False `shouldBe` scalar @CBool 1
      it "equal scalars return 0 (batch=False)" $
        (scalar @Int 5 `ArrayFire.neqBatched` scalar @Int 5) False `shouldBe` scalar @CBool 0

    describe "ltBatched" $ do
      it "1 < 2 returns 1 (batch=False)" $
        (scalar @Int 1 `ArrayFire.ltBatched` scalar @Int 2) False `shouldBe` scalar @CBool 1
      it "2 < 1 returns 0 (batch=False)" $
        (scalar @Int 2 `ArrayFire.ltBatched` scalar @Int 1) False `shouldBe` scalar @CBool 0

    describe "leBatched" $ do
      it "1 <= 1 returns 1 (batch=False)" $
        (scalar @Int 1 `ArrayFire.leBatched` scalar @Int 1) False `shouldBe` scalar @CBool 1
      it "2 <= 1 returns 0 (batch=False)" $
        (scalar @Int 2 `ArrayFire.leBatched` scalar @Int 1) False `shouldBe` scalar @CBool 0

    describe "gtBatched" $ do
      it "2 > 1 returns 1 (batch=False)" $
        (scalar @Int 2 `ArrayFire.gtBatched` scalar @Int 1) False `shouldBe` scalar @CBool 1
      it "1 > 2 returns 0 (batch=False)" $
        (scalar @Int 1 `ArrayFire.gtBatched` scalar @Int 2) False `shouldBe` scalar @CBool 0

    describe "geBatched" $ do
      it "1 >= 1 returns 1 (batch=False)" $
        (scalar @Int 1 `ArrayFire.geBatched` scalar @Int 1) False `shouldBe` scalar @CBool 1
      it "1 >= 2 returns 0 (batch=False)" $
        (scalar @Int 1 `ArrayFire.geBatched` scalar @Int 2) False `shouldBe` scalar @CBool 0

    describe "bitAndBatched" $ do
      it "bitAndBatched 1 1 = 1 (batch=False)" $
        ArrayFire.bitAndBatched (scalar @Int 1) (scalar @Int 1) False `shouldBe` scalar @Int 1
      it "bitAndBatched 1 0 = 0 (batch=False)" $
        ArrayFire.bitAndBatched (scalar @Int 1) (scalar @Int 0) False `shouldBe` scalar @Int 0

    describe "bitOrBatched" $ do
      it "bitOrBatched 1 0 = 1 (batch=False)" $
        ArrayFire.bitOrBatched (scalar @Int 1) (scalar @Int 0) False `shouldBe` scalar @Int 1
      it "bitOrBatched 0 0 = 0 (batch=False)" $
        ArrayFire.bitOrBatched (scalar @Int 0) (scalar @Int 0) False `shouldBe` scalar @Int 0

    describe "bitXorBatched" $ do
      it "bitXorBatched 1 1 = 0 (batch=False)" $
        ArrayFire.bitXorBatched (scalar @Int 1) (scalar @Int 1) False `shouldBe` scalar @Int 0
      it "bitXorBatched 1 0 = 1 (batch=False)" $
        ArrayFire.bitXorBatched (scalar @Int 1) (scalar @Int 0) False `shouldBe` scalar @Int 1

    describe "bitShiftL" $ do
      it "1 << 3 = 8" $
        ArrayFire.bitShiftL (scalar @Int 1) (scalar @Int 3) `shouldBe` scalar @Int 8
      it "1 << 0 = 1" $
        ArrayFire.bitShiftL (scalar @Int 1) (scalar @Int 0) `shouldBe` scalar @Int 1
      it "3 << 2 = 12" $
        ArrayFire.bitShiftL (scalar @Int 3) (scalar @Int 2) `shouldBe` scalar @Int 12

    describe "bitShiftR" $ do
      it "8 >> 3 = 1" $
        ArrayFire.bitShiftR (scalar @Int 8) (scalar @Int 3) `shouldBe` scalar @Int 1
      it "12 >> 2 = 3" $
        ArrayFire.bitShiftR (scalar @Int 12) (scalar @Int 2) `shouldBe` scalar @Int 3
      it "1 >> 0 = 1" $
        ArrayFire.bitShiftR (scalar @Int 1) (scalar @Int 0) `shouldBe` scalar @Int 1

    describe "andBatched" $ do
      it "1 AND 1 = 1 (batch=False)" $
        ArrayFire.andBatched (scalar @Int 1) (scalar @Int 1) False `shouldBe` scalar @CBool 1
      it "1 AND 0 = 0 (batch=False)" $
        ArrayFire.andBatched (scalar @Int 1) (scalar @Int 0) False `shouldBe` scalar @CBool 0

    describe "orBatched" $ do
      it "1 OR 0 = 1 (batch=False)" $
        ArrayFire.orBatched (scalar @Int 1) (scalar @Int 0) False `shouldBe` scalar @CBool 1
      it "0 OR 0 = 0 (batch=False)" $
        ArrayFire.orBatched (scalar @Int 0) (scalar @Int 0) False `shouldBe` scalar @CBool 0

    describe "bitShiftLBatched" $ do
      it "1 << 3 = 8 (batch=False)" $
        ArrayFire.bitShiftLBatched (scalar @Int 1) (scalar @Int 3) False `shouldBe` scalar @Int 8
      it "3 << 2 = 12 (batch=False)" $
        ArrayFire.bitShiftLBatched (scalar @Int 3) (scalar @Int 2) False `shouldBe` scalar @Int 12

    describe "bitShiftRBatched" $ do
      it "8 >> 3 = 1 (batch=False)" $
        ArrayFire.bitShiftRBatched (scalar @Int 8) (scalar @Int 3) False `shouldBe` scalar @Int 1
      it "12 >> 2 = 3 (batch=False)" $
        ArrayFire.bitShiftRBatched (scalar @Int 12) (scalar @Int 2) False `shouldBe` scalar @Int 3

    describe "clampBatched" $ do
      it "clamp 2 to [1,3] = 2 (batch=False)" $
        ArrayFire.clampBatched (scalar @Int 2) (scalar @Int 1) (scalar @Int 3) False `shouldBe` scalar @Int 2
      it "clamp 0 to [1,3] = 1 (batch=False)" $
        ArrayFire.clampBatched (scalar @Int 0) (scalar @Int 1) (scalar @Int 3) False `shouldBe` scalar @Int 1
      it "clamp 5 to [1,3] = 3 (batch=False)" $
        ArrayFire.clampBatched (scalar @Int 5) (scalar @Int 1) (scalar @Int 3) False `shouldBe` scalar @Int 3

    describe "remBatched" $ do
      it "7 rem 3 = 1 (batch=False)" $
        ArrayFire.remBatched (scalar @Int 7) (scalar @Int 3) False `shouldBe` scalar @Int 1
      it "10 rem 5 = 0 (batch=False)" $
        ArrayFire.remBatched (scalar @Int 10) (scalar @Int 5) False `shouldBe` scalar @Int 0

    describe "modBatched" $ do
      it "7 mod 3 = 1 (batch=False)" $
        ArrayFire.modBatched (scalar @Int 7) (scalar @Int 3) False `shouldBe` scalar @Int 1
      it "9 mod 3 = 0 (batch=False)" $
        ArrayFire.modBatched (scalar @Int 9) (scalar @Int 3) False `shouldBe` scalar @Int 0

    describe "minOfBatched" $ do
      it "min 2 3 = 2 (batch=False)" $
        ArrayFire.minOfBatched (scalar @Int 2) (scalar @Int 3) False `shouldBe` scalar @Int 2
      it "min 5 1 = 1 (batch=False)" $
        ArrayFire.minOfBatched (scalar @Int 5) (scalar @Int 1) False `shouldBe` scalar @Int 1

    describe "maxOfBatched" $ do
      it "max 2 3 = 3 (batch=False)" $
        ArrayFire.maxOfBatched (scalar @Int 2) (scalar @Int 3) False `shouldBe` scalar @Int 3
      it "max 5 1 = 5 (batch=False)" $
        ArrayFire.maxOfBatched (scalar @Int 5) (scalar @Int 1) False `shouldBe` scalar @Int 5

    describe "rootBatched" $ do
      it "cube root of 8 = 2 (batch=False)" $
        evalf (ArrayFire.rootBatched (scalar @Double 8) (scalar @Double 3) False) `shouldBeApprox` 2
      it "square root of 9 = 3 (batch=False)" $
        evalf (ArrayFire.rootBatched (scalar @Double 9) (scalar @Double 2) False) `shouldBeApprox` 3

    describe "powBatched" $ do
      it "2^3 = 8 (batch=False)" $
        ArrayFire.powBatched (scalar @Int 2) (scalar @Int 3) False `shouldBe` scalar @Int 8
      it "5^2 = 25 (batch=False)" $
        ArrayFire.powBatched (scalar @Int 5) (scalar @Int 2) False `shouldBe` scalar @Int 25
