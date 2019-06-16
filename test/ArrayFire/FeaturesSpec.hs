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
--  it "Should retain features" $ do
--    let feats = createFeatures 1
--    print (getFeaturesSize feats)
--    print (getFeaturesOrientation feats)
--    print (getFeaturesXPos feats)
--    print (getFeaturesXPos feats)
--    scalar @Int 10 `shouldBe` scalar @Int 10

