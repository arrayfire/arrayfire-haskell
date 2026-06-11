{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module ArrayFire.FeaturesSpec where

import qualified ArrayFire as A
import           Test.Hspec

-- | All five per-feature accessor arrays for a 'Features' handle.
accessors :: A.Features -> [A.Array Float]
accessors f =
  [ A.getFeaturesXPos        f
  , A.getFeaturesYPos        f
  , A.getFeaturesScore       f
  , A.getFeaturesOrientation f
  , A.getFeaturesSize        f
  ]

spec :: Spec
spec = describe "Features spec" $ do

  describe "createFeatures / getFeaturesNum" $ do
    it "reports the requested number of features" $
      A.getFeaturesNum (A.createFeatures 10) `shouldBe` 10

    it "supports an empty feature set" $
      A.getFeaturesNum (A.createFeatures 0) `shouldBe` 0

    it "supports a large feature set" $
      A.getFeaturesNum (A.createFeatures 1024) `shouldBe` 1024

  describe "accessor arrays" $ do
    it "every accessor array has getFeaturesNum elements" $ do
      let feats = A.createFeatures 10
      map A.getElements (accessors feats) `shouldBe` replicate 5 10

    it "every accessor array is a column vector of length n" $ do
      let feats = A.createFeatures 7
      map A.getDims (accessors feats) `shouldBe` replicate 5 (7,1,1,1)

    -- NB: 'createFeatures 0' is a degenerate case — ArrayFire does not
    -- allocate the per-feature accessor arrays for an empty set, so reading
    -- them back yields uninitialized handles (garbage element counts / dims).
    -- We therefore do not assert anything about accessors of an empty set.

  describe "retainFeatures" $ do
    it "preserves the feature count" $ do
      let feats = A.createFeatures 10
      A.getFeaturesNum (A.retainFeatures feats) `shouldBe` A.getFeaturesNum feats

    it "preserves accessor-array dimensions" $ do
      let feats = A.retainFeatures (A.createFeatures 5)
      map A.getDims (accessors feats) `shouldBe` replicate 5 (5,1,1,1)
