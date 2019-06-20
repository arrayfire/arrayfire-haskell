{-# LANGUAGE TypeApplications #-}
module ArrayFire.FeaturesSpec where

import ArrayFire
import Test.Hspec

spec :: Spec
spec =
  describe "Feautures tests" $ do
    it "Should get features number an array" $ do
      let feats = createFeatures 10
      getFeaturesNum feats `shouldBe` 10
      -- print (getFeaturesSize feats)
      -- print (getFeaturesOrientation feats)
      -- print (getFeaturesXPos feats)
      -- print (getFeaturesYPos feats)

