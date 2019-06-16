{-# LANGUAGE TypeApplications #-}
module ArrayFire.StatisticsSpec where

import ArrayFire    hiding (not)

import Data.Complex
import Test.Hspec

spec :: Spec
spec =
  describe "Statistics spec" $ do
    it "Should find the mean" $ do
      mean (vector @Double 10 [1..]) 0
        `shouldBe`
           5.5
    it "Should find the weighted-mean" $ do
      meanWeighted (vector @Double 10 [1..10]) (vector @Double 10 [1..10]) 0
        `shouldBe`
           7.0

