{-# LANGUAGE TypeApplications #-}
module ArrayFire.AlgorithmSpec where

import qualified ArrayFire       as A

import           Test.Hspec

spec :: Spec
spec =
  describe "Algorithm tests" $ do
    it "Should sum a scalar" $ do
      A.sum (A.scalar @Int 10) 0 `shouldBe` 10
      A.sum (A.scalar @A.Int64 10) 0 `shouldBe` 10
      A.sum (A.scalar @A.Int32 10) 0 `shouldBe` 10
      A.sum (A.scalar @A.Int16 10) 0 `shouldBe` 10
      A.sum (A.scalar @Float 10) 0 `shouldBe` 10
      A.sum (A.scalar @A.Word32 10) 0 `shouldBe` 10
      A.sum (A.scalar @A.Word64 10) 0 `shouldBe` 10
      A.sum (A.scalar @Double 10) 0 `shouldBe` 10.0
      A.sum (A.scalar @(A.Complex Double) (1 A.:+ 1)) 0 `shouldBe` A.scalar (1 A.:+ 1)
      A.sum (A.scalar @(A.Complex Float) (1 A.:+ 1)) 0 `shouldBe` A.scalar (1 A.:+ 1)
      A.sum (A.scalar @A.CBool 1) 0 `shouldBe` 1
      A.sum (A.scalar @A.CBool 0) 0 `shouldBe` 0
    it "Should sum a vector" $ do
      A.sum (A.vector @Int 10 [1..]) 0 `shouldBe` 55
      A.sum (A.vector @A.Int64 10 [1..]) 0 `shouldBe` 55
      A.sum (A.vector @A.Int32 10 [1..]) 0 `shouldBe` 55
      A.sum (A.vector @A.Int16 10 [1..]) 0 `shouldBe` 55
      A.sum (A.vector @Float 10 [1..]) 0 `shouldBe` 55
      A.sum (A.vector @A.Word32 10 [1..]) 0 `shouldBe` 55
      A.sum (A.vector @A.Word64 10 [1..]) 0 `shouldBe` 55
      A.sum (A.vector @Double 10 [1..]) 0 `shouldBe` 55.0
      A.sum (A.vector @(A.Complex Double) 10 (repeat (1 A.:+ 1))) 0 `shouldBe` A.scalar (10.0 A.:+ 10.0)
      A.sum (A.vector @(A.Complex Float) 10 (repeat (1 A.:+ 1))) 0 `shouldBe` A.scalar (10.0 A.:+ 10.0)
      A.sum (A.vector @A.CBool 10 (repeat 1)) 0 `shouldBe` 10
      A.sum (A.vector @A.CBool 10 (repeat 0)) 0 `shouldBe` 0
    it "Should sum a default value to replace NaN" $ do
      A.sumNaN (A.vector @Float 10 [1..]) 0 1.0 `shouldBe` 55
      A.sumNaN (A.vector @Double 2 [acos 2, acos 2]) 0 50 `shouldBe` 100
      A.sumNaN (A.vector @(A.Complex Float) 10 (repeat (1 A.:+ 1))) 0 1.0 `shouldBe` A.scalar (10.0 A.:+ 10.0)
      A.sumNaN (A.vector @(A.Complex Double) 10 (repeat (1 A.:+ 1))) 0 1.0 `shouldBe` A.scalar (10.0 A.:+ 10.0)
    it "Should product a scalar" $ do
      A.product (A.scalar @Int 10) 0 `shouldBe` 10
      A.product (A.scalar @A.Int64 10) 0 `shouldBe` 10
      A.product (A.scalar @A.Int32 10) 0 `shouldBe` 10
      A.product (A.scalar @A.Int16 10) 0 `shouldBe` 10
      A.product (A.scalar @Float 10) 0 `shouldBe` 10
      A.product (A.scalar @A.Word32 10) 0 `shouldBe` 10
      A.product (A.scalar @A.Word64 10) 0 `shouldBe` 10
      A.product (A.scalar @Double 10) 0 `shouldBe` 10.0
      A.product (A.scalar @(A.Complex Double) (1 A.:+ 1)) 0 `shouldBe` A.scalar (1 A.:+ 1)
      A.product (A.scalar @(A.Complex Float) (1 A.:+ 1)) 0 `shouldBe` A.scalar (1 A.:+ 1)
      A.product (A.scalar @A.CBool 1) 0 `shouldBe` 1
      A.product (A.scalar @A.CBool 0) 0 `shouldBe` 0
    it "Should product a vector" $ do
      A.product (A.vector @Int 10 [1..]) 0 `shouldBe` 3628800
      A.product (A.vector @A.Int64 10 [1..]) 0 `shouldBe` 3628800
      A.product (A.vector @A.Int32 10 [1..]) 0 `shouldBe` 3628800
      A.product (A.vector @A.Int16 5 [1..]) 0 `shouldBe` 120
      A.product (A.vector @Float 10 [1..]) 0 `shouldBe` 3628800
      A.product (A.vector @A.Word32 10 [1..]) 0 `shouldBe` 3628800
      A.product (A.vector @A.Word64 10 [1..]) 0 `shouldBe` 3628800
      A.product (A.vector @Double 10 [1..]) 0 `shouldBe` 3628800.0
      A.product (A.vector @(A.Complex Double) 10 (repeat (1 A.:+ 1))) 0 `shouldBe` A.scalar (0.0 A.:+ 32.0)
      A.product (A.vector @(A.Complex Float) 10 (repeat (1 A.:+ 1))) 0 `shouldBe` A.scalar (0.0 A.:+ 32.0)
      A.product (A.vector @A.CBool 10 (repeat 1)) 0 `shouldBe` 1 -- FIXED in 3.8.2, vector product along 0-axis is 1 for vector size 10 of all 1's.
      A.product (A.vector @A.CBool 10 (repeat 0)) 0 `shouldBe` 0
    it "Should product a default value to replace NaN" $ do
      A.productNaN (A.vector @Float 10 [1..]) 0 1.0 `shouldBe` 3628800.0
      A.productNaN (A.vector @Double 2 [acos 2, acos 2]) 0 50 `shouldBe` 2500
      A.productNaN (A.vector @(A.Complex Float) 10 (repeat (1 A.:+ 1))) 0 1.0 `shouldBe` A.scalar (0.0 A.:+ 32)
      A.productNaN (A.vector @(A.Complex Double) 10 (repeat (1 A.:+ 1))) 0 1.0 `shouldBe` A.scalar (0 A.:+ 32)
    it "Should take the minimum element of a vector" $ do
      A.min (A.vector @Int 10 [1..]) 0 `shouldBe` 1
      A.min (A.vector @A.Int64 10 [1..]) 0 `shouldBe` 1
      A.min (A.vector @A.Int32 10 [1..]) 0 `shouldBe` 1
      A.min (A.vector @A.Int16 10 [1..]) 0 `shouldBe` 1
      A.min (A.vector @Float 10 [1..]) 0 `shouldBe` 1
      A.min (A.vector @A.Word32 10 [1..]) 0 `shouldBe` 1
      A.min (A.vector @A.Word64 10 [1..]) 0 `shouldBe` 1
      A.min (A.vector @Double 10 [1..]) 0 `shouldBe` 1
      A.min (A.vector @(A.Complex Double) 10 (repeat (1 A.:+ 1))) 0 `shouldBe` A.scalar (1 A.:+ 1)
      A.min (A.vector @(A.Complex Float) 10 (repeat (1 A.:+ 1))) 0 `shouldBe` A.scalar (1 A.:+ 1)
      A.min (A.vector @A.CBool 10 [1..]) 0 `shouldBe` 1
      A.min (A.vector @A.CBool 10 [1..]) 0 `shouldBe` 1
    it "Should find if all elements are true along dimension" $ do
      A.allTrue (A.vector @Double 5 (repeat 12.0)) 0 `shouldBe` 1
      A.allTrue (A.vector @A.CBool 5 (repeat 1)) 0 `shouldBe` 1
      A.allTrue (A.vector @A.CBool 5 (repeat 0)) 0 `shouldBe` 0
      A.allTrue (A.vector @A.CBool 5 (repeat 0)) 0 `shouldBe` 0
    it "Should find if any elements are true along dimension" $ do
      A.anyTrue (A.vector @A.CBool 5 (repeat 1)) 0 `shouldBe` 1
      A.anyTrue (A.vector @Int 5 (repeat 23)) 0 `shouldBe` 1
      A.anyTrue (A.vector @A.CBool 5 (repeat 0)) 0 `shouldBe` 0
    it "Should get count of all elements" $ do
      A.count (A.vector @Int 5 (repeat 1)) 0 `shouldBe` 5
      A.count (A.vector @A.CBool 5 (repeat 1)) 0 `shouldBe` 5
      A.count (A.vector @Double 5 (repeat 1)) 0 `shouldBe` 5
      A.count (A.vector @Float 5 (repeat 1)) 0 `shouldBe` 5
    it "Should get sum all elements" $ do
      A.sumAll (A.vector @Int 5 (repeat 2)) `shouldBe` (10,0)
      A.sumAll (A.vector @Double 5 (repeat 2)) `shouldBe` (10.0,0)
      A.sumAll (A.vector @A.CBool 3800 (repeat 1)) `shouldBe` (3800,0)
      A.sumAll (A.vector @(A.Complex Double) 5 (repeat (2 A.:+ 0))) `shouldBe` (10.0,0)
    it "Should get sum all elements" $ do
      A.sumNaNAll (A.vector @Double 2 [10, acos 2]) 1 `shouldBe` (11.0,0)
    it "Should product all elements in an Array" $ do
      A.productAll (A.vector @Int 5 (repeat 2)) `shouldBe` (32,0)
    it "Should product all elements in an Array" $ do
      A.productNaNAll (A.vector @Double 2 [10,acos 2]) 10 `shouldBe` (100,0)
    it "Should find minimum value of an Array" $ do
      A.minAll (A.vector @Int 5 [0..]) `shouldBe` (0,0)
    it "Should find maximum value of an Array" $ do
      A.maxAll (A.vector @Int 5 [0..]) `shouldBe` (4,0)
--    it "Should find if all elements are true" $ do
--      A.allTrue (A.vector @A.CBool 5 (repeat 0)) `shouldBe` False

