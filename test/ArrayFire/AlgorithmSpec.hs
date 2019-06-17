{-# LANGUAGE TypeApplications #-}
module ArrayFire.AlgorithmSpec where

import qualified ArrayFire       as A
import           Foreign.C.Types
import           Prelude         hiding (sqrt, div, and, or, not, sum)
import           Test.Hspec

spec :: Spec
spec =
  describe "Algorithm tests" $ do
    it "Should sum a vector" $ do
      A.sum (A.vector @Int 10 [1..]) 0 `shouldBe` 55
      A.sum (A.vector @Double 10 [1..]) 0 `shouldBe` 55.0
      A.sum (A.vector @CBool 10 (repeat 1)) 0 `shouldBe` 10
    it "Should sum a default value to replace NaN" $ do
      A.sumNaN (A.vector @Float 10 [1..]) 0 1.0 `shouldBe` 55
      A.sumNaN (A.vector @Double 2 [acos 2, acos 2]) 0 50 `shouldBe` 100
    it "Should product a vector" $ do
      A.product (A.vector @Int 5 [1..]) 0 `shouldBe` 120
      A.product (A.vector @Double 5 [1..]) 0 `shouldBe` 120
      A.product (A.vector @CBool 10 (repeat 0)) 0 `shouldBe` 0
      -- A.product (A.vector @CBool 10 (repeat 1)) 0 `shouldBe` A.scalar 1
    it "Should product a default value to replace NaN" $ do
      A.productNaN (A.vector @Float 5 [1..]) 0 1.0 `shouldBe` 120
      A.productNaN (A.vector @Double 2 [acos 2, acos 2]) 0 50 `shouldBe` 2500
    it "Should take the minimum element of a vector" $ do
      A.min (A.vector @Double 5 [1..]) 0 `shouldBe` 1.0
    it "Should find if all elements are true" $ do
      A.allTrue (A.vector @Double 5 (repeat 12.0)) 0 `shouldBe` True
      A.allTrue (A.vector @CBool 5 (repeat 1)) 0 `shouldBe` True
      A.allTrue (A.vector @CBool 5 (repeat 0)) 0 `shouldBe` False
      A.allTrue (A.vector @CBool 5 (repeat 0)) 0 `shouldBe` False
    it "Should find if any elements are true" $ do
      A.anyTrue (A.vector @CBool 5 (repeat 1)) 0 `shouldBe` True
      A.anyTrue (A.vector @Int 5 (repeat 23)) 0 `shouldBe` True
      A.anyTrue (A.vector @CBool 5 (repeat 0)) 0 `shouldBe` False
    it "Should get count of all elements" $ do
      A.count (A.vector @Int 5 (repeat 1)) 0 `shouldBe` 5
      A.count (A.vector @CBool 5 (repeat 1)) 0 `shouldBe` 5
      A.count (A.vector @Double 5 (repeat 1)) 0 `shouldBe` 5
      A.count (A.vector @Float 5 (repeat 1)) 0 `shouldBe` 5
