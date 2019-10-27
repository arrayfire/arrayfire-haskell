{-# LANGUAGE TypeApplications #-}
module ArrayFire.ArithSpec where

import ArrayFire  hiding (acos)
import Prelude    hiding (sqrt, div, and, or, not, isNaN)
import Test.Hspec
import Foreign.C

spec :: Spec
spec =
  describe "Arith tests" $ do
    it "Should negate scalar value" $ do
      negate (scalar @Int 1) `shouldBe` (-1)
    it "Should negate a vector" $ do
      negate (vector @Int 3 [2,2,2]) `shouldBe` vector @Int 3 [-2,-2,-2]
    it "Should add two scalar arrays" $ do
      scalar @Int 1 + 2 `shouldBe` 3
    it "Should add two scalar bool arrays" $ do
      scalar @CBool 1 + 0 `shouldBe` 1
    it "Should subtract two scalar arrays" $ do
      scalar @Int 4 - 2 `shouldBe` 2
    it "Should multiply two scalar arrays" $ do
      scalar @Double 4 `mul` 2 `shouldBe` 8
    it "Should divide two scalar arrays" $ do
      div @Double 8 2 `shouldBe` 4
    it "Should add two matrices" $ do
      matrix @Int (2,2) [[1,1],[1,1]] + matrix @Int (2,2) [[1,1],[1,1]]
        `shouldBe`
           matrix @Int (2,2) [[2,2],[2,2]]
    it "Should take cubed root" $ do
      3 `shouldBe` cbrt @Double 27
    it "Should take square root" $ do
      2 `shouldBe` sqrt @Double 4
    it "Should lt Array" $ do
      2 < (3 :: Array Double) `shouldBe` True
    it "Should lte Array" $ do
      2 <= (3 :: Array Double) `shouldBe` True
    it "Should gte Array" $ do
      2 >= (3 :: Array Double) `shouldBe` False
    it "Should gt Array" $ do
      2 > (3 :: Array Double) `shouldBe` False
    it "Should eq Array" $ do
      3 == (3 :: Array Double) `shouldBe` True
    it "Should and Array" $ do
      (mkArray @CBool [1] [0] `and` mkArray [1] [1])
         `shouldBe` mkArray [1] [0]
    it "Should and Array" $ do
      (mkArray @CBool [2] [0,0] `and` mkArray [2] [1,0])
         `shouldBe` mkArray [2] [0, 0]
    it "Should or Array" $ do
      (mkArray @CBool [2] [0,0] `or` mkArray [2] [1,0])
         `shouldBe` mkArray [2] [1, 0]
    it "Should not Array" $ do
      not (mkArray @CBool [2] [1,0]) `shouldBe` mkArray [2] [0,1]
    it "Should bitwise and array" $ do
      bitAnd (scalar @Int 1) (scalar @Int 0)
        `shouldBe`
           0
    it "Should bitwise or array" $ do
      bitOr (scalar @Int 1) (scalar @Int 0)
        `shouldBe`
           1
    it "Should bitwise xor array" $ do
      bitXor (scalar @Int 1) (scalar @Int 1)
        `shouldBe`
           0
    it "Should bitwise shift left an array" $ do
       bitShiftL (scalar @Int 1) (scalar @Int 3)
        `shouldBe`
           8
    it "Should cast an array" $ do
      getType (cast (scalar @Int 1) :: Array Double)
        `shouldBe`
           F64
    it "Should find the minimum of two arrays" $ do
      minOf (scalar @Int 1) (scalar @Int 0)
        `shouldBe`
           0
    it "Should find the max of two arrays" $ do
      maxOf (scalar @Int 1) (scalar @Int 0)
        `shouldBe`
           1
    it "Should take the clamp of 3 arrays" $ do
      clamp (scalar @Int 2) (scalar @Int 1) (scalar @Int 3)
       `shouldBe`
          2
    it "Should check if an array has positive or negative infinities" $ do
      isInf (scalar @Double (1 / 0)) `shouldBe` scalar @Double 1
      isInf (scalar @Double 10) `shouldBe` scalar @Double 0
    it "Should check if an array has any NaN values" $ do
      isNaN (scalar @Double (acos 2)) `shouldBe` scalar @Double 1
      isNaN (scalar @Double 10) `shouldBe` scalar @Double 0
    it "Should check if an array has any Zero values" $ do
      isZero (scalar @Double (acos 2)) `shouldBe` scalar @Double 0
      isZero (scalar @Double 0) `shouldBe` scalar @Double 1
      isZero (scalar @Double 1) `shouldBe` scalar @Double 0
