{-# LANGUAGE TypeApplications #-}
module ArrayFire.ArithSpec where

import ArrayFire
import Prelude    hiding (sqrt, div, and, or, not)
import Test.Hspec
import Foreign.C

spec :: Spec
spec =
  describe "Arith tests" $ do
    it "Should add two scalar arrays" $ do
      scalar @Int 1 + 2 `shouldBe` 3
    it "Should subtract two scalar arrays" $ do
      scalar @Int 4 - 2 `shouldBe` 2
    it "Should multiply two scalar arrays" $ do
      scalar @Double 4 <> 2 `shouldBe` 8
    it "Should divide two scalar arrays" $ do
      div @Double 8 2 False `shouldBe` 4
    it "Should add two matrices" $ do
      matrix @Int (2,2) [1,1,1,1] + matrix @Int (2,2) [1,1,1,1]
        `shouldBe`
           matrix @Int (2,2) (repeat 2)
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
      ((mkArray @CBool [1] [0] `and` (mkArray [1] [1])) False)
         `shouldBe` mkArray [1] [0]
    it "Should and Array" $ do
      ((mkArray @CBool [2] [0,0] `and` (mkArray [2] [1,0])) False)
         `shouldBe` mkArray [2] [0, 0]
    it "Should or Array" $ do
      ((mkArray @CBool [2] [0,0] `or` (mkArray [2] [1,0])) False)
         `shouldBe` mkArray [2] [1, 0]
    it "Should not Array" $ do
      not (mkArray @CBool [2] [1,1]) `shouldBe` mkArray [2] [0,0]