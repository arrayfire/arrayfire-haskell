{-# LANGUAGE TypeApplications #-}
module ArrayFire.FeaturesSpec where

import ArrayFire hiding (acos)
import Prelude
import Test.Hspec

spec :: Spec
spec =
  describe "Feautures tests" $ do
    it "Should get features number an array" $ do
      let feats = createFeatures 10
      getFeaturesNum feats `shouldBe` 10
      -- let vec = vector @Double 10 $ repeat (acos 2)
      -- getFeaturesSize feats `shouldBe` vec
      -- getFeaturesOrientation feats `shouldBe` vec
      -- getFeaturesXPos feats `shouldBe` vec
      -- getFeaturesYPos feats `shouldBe` vec
