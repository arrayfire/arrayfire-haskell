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
      meanWeighted (vector @Double 10 [1..]) (vector @Double 10 [1..]) 0
        `shouldBe`
           7.0
    it "Should find the variance" $ do
      var (vector @Double 8 [1..8]) False 0
        `shouldBe`
           5.25
    it "Should find the weighted variance" $ do
      varWeighted (vector @Double 8 [1..]) (vector @Double 8 (repeat 1)) 0
        `shouldBe`
           5.25
    it "Should find the standard deviation" $ do
      stdev (vector @Double 10 (cycle [1,-1])) 0
        `shouldBe`
           1.0
    it "Should find the covariance" $ do
      cov (vector @Double 10 (repeat 1)) (vector @Double 10 (repeat 1)) False
        `shouldBe`
           0.0
    it "Should find the median" $ do
      median (vector @Double 10 [1..]) 0
        `shouldBe`
           5.5
    it "Should find the mean of all elements across all dimensions" $ do
      fst (meanAll (matrix @Double (2,2) [[10,10],[10,10]]))
        `shouldBe`
           10
    it "Should find the weighted mean of all elements across all dimensions" $ do
      fst (meanAllWeighted (matrix @Double (2,2) [[10,10],[10,10]]) (matrix @Double (2,2) [[10,10],[10,10]]))
        `shouldBe`
           10
    it "Should find the variance of all elements across all dimensions" $ do
      fst (varAll (vector @Double 10 (repeat 10)) False)
        `shouldBe`
           0
    it "Should find the weighted variance of all elements across all dimensions" $ do
      fst (varAllWeighted (vector @Double 10 (repeat 10)) (vector @Double 10 (repeat 10)))
        `shouldBe`
           0
    it "Should find the stdev of all elements across all dimensions" $ do
      fst (stdevAll (vector @Double 10 (repeat 10)))
        `shouldBe`
           0
    it "Should find the median of all elements across all dimensions" $ do
      fst (medianAll (vector @Double 10 [1..]))
        `shouldBe`
           5.5
    it "Should find the correlation coefficient" $ do
      fst (corrCoef (vector @Int 10 [1..] ) ( vector @Int 10 [10,9..] ))
        `shouldBe`
           (-1.0)
    it "Should find the top k elements" $ do
      let (vals,indexes) = topk ( vector @Double 10 [1..] ) 3 TopKDefault
      vals `shouldBe` vector @Double 3 [10,9,8]
      indexes `shouldBe` vector @Double 3 [9,8,7]
