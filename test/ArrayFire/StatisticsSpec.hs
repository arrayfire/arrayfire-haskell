{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module ArrayFire.StatisticsSpec where

import Data.Word (Word32)
import ArrayFire hiding (not, abs, isNaN)

import Data.Maybe
import Data.Complex
import Test.Hspec
import Test.Hspec.ApproxExpect
import Test.Hspec.QuickCheck  (prop)
import Test.QuickCheck        (NonEmptyList (..), (==>))

spec :: Spec
spec =
  describe "Statistics spec" $ do
    it "Should find the mean" $ do
      mean (vector @Double 10 [1..]) 0
        `shouldBe`
           5.5
    it "Should find the weighted-mean" $ do
      listToMaybe (toList (meanWeighted (vector @Double 10 [1..]) (vector @Double 10 [1..]) 0))
        `shouldBe`
           (Just 7.0)
    it "Should find the variance" $ do
      var (vector @Double 8 [1..8]) Population 0
        `shouldBe`
           5.25
    it "Should find the weighted variance (equal weights)" $ do
      varWeighted (vector @Double 8 [1..]) (vector @Double 8 (repeat 1)) 0
        `shouldBe`
           5.25
    it "Should find the weighted variance (increasing weights)" $ do
      head (toList (varWeighted (vector @Double 10 [1..]) (vector @Double 10 [1..]) 0))
        `shouldBeApprox` (21/11 :: Double)
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
      meanAll (matrix @Double (2,2) [[10,10],[10,10]])
        `shouldBe` 10
    it "Should find the weighted mean of all elements across all dimensions" $ do
      meanAllWeighted (matrix @Double (2,2) [[10,10],[10,10]]) (matrix @Double (2,2) [[10,10],[10,10]])
        `shouldBe` 10
    it "Should find the variance of all elements across all dimensions" $ do
      varAll (vector @Double 10 (repeat 10)) Population
        `shouldBe` 0
    it "Should find the weighted variance of all elements across all dimensions" $ do
      varAllWeighted (vector @Double 10 (repeat 10)) (vector @Double 10 (repeat 10))
        `shouldBe` 0
    it "Should find the stdev of all elements across all dimensions" $ do
      stdevAll (vector @Double 10 (repeat 10))
        `shouldBe` 0
    it "Should find the median of all elements across all dimensions" $ do
      medianAll (vector @Double 10 [1..])
        `shouldBe` 5.5
    it "Should find the correlation coefficient" $ do
      corrCoef (vector @Int 10 [1..]) (vector @Int 10 [10,9..])
        `shouldBe` (-1.0)
    it "Should find the top k elements" $ do
      let (vals,indexes) = topk ( vector @Double 10 [1..] ) 3 TopKDefault
      vals `shouldBe` vector @Double 3 [10,9,8]
      indexes `shouldBe` vector @Word32 3 [9,8,7]
    it "Should compute mean and variance together (population)" $ do
      let (m, v) = meanVar (vector @Double 4 [1,2,3,4]) VariancePopulation 0
      m `shouldBe` scalar @Double 2.5
      v `shouldBe` scalar @Double 1.25
    it "Should compute mean and variance together (sample)" $ do
      let (m, v) = meanVar (vector @Double 4 [1,2,3,4]) VarianceSample 0
      m `shouldBe` scalar @Double 2.5
      -- sample variance of [1,2,3,4] = 5/3 ≈ 1.6667
      case listToMaybe (toList v) of
        Just k -> k `shouldBeApprox` (5.0/3.0)
        _ -> error "failure"
    it "Should compute weighted mean and variance together" $ do
      let uniform = vector @Double 4 (repeat 1.0)
          (m, v)  = meanVarWeighted (vector @Double 4 [1,2,3,4]) uniform VariancePopulation 0
      m `shouldBe` scalar @Double 2.5
      v `shouldBe` scalar @Double 1.25

    describe "statistical properties" $ do
      -- mean(x + c) = mean(x) + c  (translation equivariance)
      prop "mean is translation-equivariant" $ \(NonEmpty xs) (c :: Double) ->
        let n   = length xs
            arr = vector @Double n xs
            lhs = meanAll (arr + scalar c)
            rhs = meanAll arr + c
        in abs (lhs - rhs) < 1e-9

      -- var(x + c) = var(x)  (translation invariance)
      prop "variance is translation-invariant" $ \(NonEmpty xs) (c :: Double) ->
        let n   = length xs
            arr = vector @Double n xs
            lhs = varAll arr Population
            rhs = varAll (arr + scalar c) Population
        in abs (lhs - rhs) < 1e-6 * (1 + abs lhs)

      -- stdev(x)^2 = var(x, Population)  (consistency)
      prop "stdev^2 equals population variance" $ \(NonEmpty xs) ->
        let n   = length xs
            arr = vector @Double n xs
            sd  = stdevAll arr
            v   = varAll arr Population
        in abs (sd * sd - v) < 1e-9 + 1e-6 * abs v

      -- mean(c * x) = c * mean(x)  (scale equivariance)
      prop "mean scales linearly" $ \(NonEmpty xs) (c :: Double) ->
        let n   = length xs
            arr = vector @Double n xs
            lhs = meanAll (scalar c * arr)
            rhs = c * meanAll arr
        in abs (lhs - rhs) < 1e-9 + 1e-9 * abs rhs

      -- corrCoef(x, y) is in [-1, 1]  (Cauchy-Schwarz)
      prop "corrCoef is in [-1, 1]" $ \(NonEmpty xs) (ys :: [Double]) ->
        let n    = length xs
            arr1 = vector @Double n xs
            arr2 = vector @Double n (take n (ys ++ repeat 0))
            r    = corrCoef arr1 arr2
        in not (isNaN r) && not (isInfinite r) ==> r >= -1.0 - 1e-9 && r <= 1.0 + 1e-9

      -- sumAll = n * meanAll  (for any non-empty list)
      prop "sumAll = n * meanAll" $ \(NonEmpty xs) ->
        let n   = length xs
            arr = vector @Double n xs
            s   = sumAll arr
            m   = meanAll arr
        in abs (s - fromIntegral n * m) < 1e-9 + 1e-6 * abs s
