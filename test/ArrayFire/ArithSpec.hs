{-# LANGUAGE TypeApplications #-}
module ArrayFire.ArithSpec where

import ArrayFire
import Prelude    hiding (sqrt)
import Test.Hspec

spec :: Spec
spec =
  describe "Arith tests" $ do
    it "Should add two scalar arrays" $ do
      scalar @Int 1 + scalar @Int 2
        `shouldBe`
           scalar @Int 3
    it "Should add two matrices" $ do
      matrix @Int (2,2) [1,1,1,1] + matrix @Int (2,2) [1,1,1,1]
        `shouldBe`
           matrix @Int (2,2) (repeat 2)
    it "Should take cubed root" $ do
      scalar @Int 3 `shouldBe` cbrt (scalar @Int 27)
    it "Should take square root" $ do
      scalar @Int 2 `shouldBe` sqrt (scalar @Int 4)
