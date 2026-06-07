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
      A.min (A.vector @(A.Complex Double) 3 [3 A.:+ 4, 1 A.:+ 0, 2 A.:+ 2]) 0 `shouldBe` A.scalar (1 A.:+ 0)
      A.min (A.vector @(A.Complex Float) 3 [3 A.:+ 4, 1 A.:+ 0, 2 A.:+ 2]) 0 `shouldBe` A.scalar (1 A.:+ 0)
      A.min (A.vector @A.CBool 10 [1..]) 0 `shouldBe` 1
    it "Should take the maximum element of a vector" $ do
      A.max (A.vector @Int 10 [1..]) 0 `shouldBe` 10
      A.max (A.vector @A.Int64 10 [1..]) 0 `shouldBe` 10
      A.max (A.vector @A.Int32 10 [1..]) 0 `shouldBe` 10
      A.max (A.vector @A.Int16 10 [1..]) 0 `shouldBe` 10
      A.max (A.vector @Float 10 [1..]) 0 `shouldBe` 10
      A.max (A.vector @A.Word32 10 [1..]) 0 `shouldBe` 10
      A.max (A.vector @A.Word64 10 [1..]) 0 `shouldBe` 10
      A.max (A.vector @Double 10 [1..]) 0 `shouldBe` 10
      A.max (A.vector @(A.Complex Double) 3 [3 A.:+ 4, 1 A.:+ 0, 2 A.:+ 2]) 0 `shouldBe` A.scalar (3 A.:+ 4)
      A.max (A.vector @(A.Complex Float) 3 [3 A.:+ 4, 1 A.:+ 0, 2 A.:+ 2]) 0 `shouldBe` A.scalar (3 A.:+ 4)
      A.max (A.vector @A.CBool 5 [0,1,1,0,1]) 0 `shouldBe` 1
    it "Should find if all elements are true along dimension" $ do
      A.allTrue (A.vector @Double 5 (repeat 12.0)) 0 `shouldBe` A.scalar @A.CBool 1
      A.allTrue (A.vector @A.CBool 5 (repeat 1)) 0 `shouldBe` A.scalar @A.CBool 1
      A.allTrue (A.vector @A.CBool 5 (repeat 0)) 0 `shouldBe` A.scalar @A.CBool 0
    it "Should find if any elements are true along dimension" $ do
      A.anyTrue (A.vector @A.CBool 5 (repeat 1)) 0 `shouldBe` A.scalar @A.CBool 1
      A.anyTrue (A.vector @Int 5 (repeat 23)) 0 `shouldBe` A.scalar @A.CBool 1
      A.anyTrue (A.vector @A.CBool 5 (repeat 0)) 0 `shouldBe` A.scalar @A.CBool 0
    it "Should get count of all elements" $ do
      A.count (A.vector @Int 5 (repeat 1)) 0 `shouldBe` 5
      A.count (A.vector @A.CBool 5 (repeat 1)) 0 `shouldBe` 5
      A.count (A.vector @Double 5 (repeat 1)) 0 `shouldBe` 5
      A.count (A.vector @Float 5 (repeat 1)) 0 `shouldBe` 5
    it "Should get sum all elements" $ do
      A.sumAll (A.vector @Int 5 (repeat 2)) `shouldBe` (10,0)
      A.sumAll (A.vector @Double 5 (repeat 2)) `shouldBe` (10.0,0)
      A.sumAll (A.vector @A.CBool 3800 (repeat 1)) `shouldBe` (3800,0)
      A.sumAll (A.vector @(A.Complex Double) 3 [1 A.:+ 2, 3 A.:+ 4, 5 A.:+ 6]) `shouldBe` (9.0, 12.0)
    it "Should sum all elements ignoring NaN" $ do
      A.sumNaNAll (A.vector @Double 2 [10, acos 2]) 1 `shouldBe` (11.0,0)
    it "Should product all elements in an Array" $ do
      A.productAll (A.vector @Int 5 (repeat 2)) `shouldBe` (32,0)
    it "Should product all elements ignoring NaN" $ do
      A.productNaNAll (A.vector @Double 2 [10,acos 2]) 10 `shouldBe` (100,0)
    it "Should find minimum value of an Array" $ do
      A.minAll (A.vector @Int 5 [0..]) `shouldBe` (0,0)
    it "Should find maximum value of an Array" $ do
      A.maxAll (A.vector @Int 5 [0..]) `shouldBe` (4,0)
--    it "Should find if all elements are true" $ do
--      A.allTrue (A.vector @A.CBool 5 (repeat 0)) `shouldBe` False
    it "Should sum values grouped by key" $ do
      let keys = A.vector @Int 5 [1,1,2,2,2]
          vals = A.vector @Double 5 [10,20,1,2,3]
          (ko, vo) = A.sumByKey keys vals 0
      ko `shouldBe` A.vector @Int 2 [1,2]
      vo `shouldBe` A.vector @Double 2 [30,6]
    it "Should take the product of values grouped by key" $ do
      let keys = A.vector @Int 4 [1,1,2,2]
          vals = A.vector @Double 4 [2,3,4,5]
          (ko, vo) = A.productByKey keys vals 0
      ko `shouldBe` A.vector @Int 2 [1,2]
      vo `shouldBe` A.vector @Double 2 [6,20]
    it "Should find the minimum value per key group" $ do
      let keys = A.vector @Int 4 [1,1,2,2]
          vals = A.vector @Double 4 [3,1,5,2]
          (ko, vo) = A.minByKey keys vals 0
      ko `shouldBe` A.vector @Int 2 [1,2]
      vo `shouldBe` A.vector @Double 2 [1,2]
    it "Should find the maximum value per key group" $ do
      let keys = A.vector @Int 4 [1,1,2,2]
          vals = A.vector @Double 4 [3,1,5,2]
          (ko, vo) = A.maxByKey keys vals 0
      ko `shouldBe` A.vector @Int 2 [1,2]
      vo `shouldBe` A.vector @Double 2 [3,5]
    it "Should count non-zero values per key group" $ do
      let keys = A.vector @Int 4 [1,1,2,2]
          vals = A.vector @Double 4 [1,0,1,1]
          (ko, vo) = A.countByKey keys vals 0
      ko `shouldBe` A.vector @Int 2 [1,2]
      vo `shouldBe` A.vector @Double 2 [1,2]
    it "Should check allTrue per key group" $ do
      let keys = A.vector @Int 4 [1,1,2,2]
          vals = A.vector @A.CBool 4 [1,1,1,0]
          (ko, vo) = A.allTrueByKey keys vals 0
      ko `shouldBe` A.vector @Int 2 [1,2]
      vo `shouldBe` A.vector @A.CBool 2 [1,0]
    it "Should check anyTrue per key group" $ do
      let keys = A.vector @Int 4 [1,1,2,2]
          vals = A.vector @A.CBool 4 [0,0,0,1]
          (ko, vo) = A.anyTrueByKey keys vals 0
      ko `shouldBe` A.vector @Int 2 [1,2]
      vo `shouldBe` A.vector @A.CBool 2 [0,1]
    it "Should sum values grouped by key, substituting NaN with 0" $ do
      let keys = A.vector @Int 4 [1,1,2,2]
          vals = A.vector @Double 4 [10, (acos 2), 3, 4]
          (ko, vo) = A.sumByKeyNaN keys vals 0 0
      ko `shouldBe` A.vector @Int 2 [1,2]
      vo `shouldBe` A.vector @Double 2 [10, 7]
    it "Should take the product of values grouped by key, substituting NaN with 1" $ do
      let keys = A.vector @Int 4 [1,1,2,2]
          vals = A.vector @Double 4 [2, (acos 2), 4, 5]
          (ko, vo) = A.productByKeyNaN keys vals 0 1
      ko `shouldBe` A.vector @Int 2 [1,2]
      vo `shouldBe` A.vector @Double 2 [2, 20]

    describe "accum" $ do
      it "computes inclusive cumulative sum along dim 0" $ do
        A.accum (A.vector @Double 5 [1,2,3,4,5]) 0
          `shouldBe` A.vector @Double 5 [1,3,6,10,15]
      it "computes cumulative sum along dim 1 of a matrix" $ do
        A.accum (A.mkArray @Double [2,3] [1,2,3,4,5,6]) 1
          `shouldBe` A.mkArray @Double [2,3] [1,2,4,6,9,12]

    describe "diff1" $ do
      it "computes first differences along dim 0" $ do
        A.diff1 (A.vector @Double 5 [1,2,4,7,11]) 0
          `shouldBe` A.vector @Double 4 [1,2,3,4]
      it "first differences of a constant vector are zero" $ do
        A.diff1 (A.vector @Double 4 (repeat 5)) 0
          `shouldBe` A.vector @Double 3 [0,0,0]

    describe "diff2" $ do
      it "computes second differences of a quadratic sequence" $ do
        A.diff2 (A.vector @Double 5 [0,1,4,9,16]) 0
          `shouldBe` A.vector @Double 3 [2,2,2]
      it "second differences of a linear sequence are zero" $ do
        A.diff2 (A.vector @Double 5 [1,2,3,4,5]) 0
          `shouldBe` A.vector @Double 3 [0,0,0]

    describe "where'" $ do
      it "returns indices of nonzero elements" $ do
        A.where' (A.vector @Double 5 [0,1,0,2,0])
          `shouldBe` A.vector @A.Word32 2 [1,3]
      it "returns empty array when all elements are zero" $ do
        A.getDims (A.where' (A.vector @Double 3 [0,0,0]))
          `shouldBe` (0,1,1,1)

    describe "scan" $ do
      it "inclusive scan with Add equals accum" $ do
        A.scan (A.vector @Double 5 [1..5]) 0 A.Add True
          `shouldBe` A.vector @Double 5 [1,3,6,10,15]
      it "exclusive scan with Add shifts the prefix sums by one" $ do
        A.scan (A.vector @Double 5 [1..5]) 0 A.Add False
          `shouldBe` A.vector @Double 5 [0,1,3,6,10]
      it "inclusive scan with Mul gives running product" $ do
        A.scan (A.vector @Double 4 [1..4]) 0 A.Mul True
          `shouldBe` A.vector @Double 4 [1,2,6,24]

    describe "scanByKey" $ do
      it "resets prefix sum at each key boundary" $ do
        let keys = A.vector @Int 4 [1,1,2,2]
            vals = A.vector @Double 4 [1,2,3,4]
        A.scanByKey keys vals 0 A.Add True
          `shouldBe` A.vector @Double 4 [1,3,3,7]

    describe "sort" $ do
      it "sorts ascending" $ do
        A.sort (A.vector @Double 5 [3,1,4,1,5]) 0 True
          `shouldBe` A.vector @Double 5 [1,1,3,4,5]
      it "sorts descending" $ do
        A.sort (A.vector @Double 5 [3,1,4,1,5]) 0 False
          `shouldBe` A.vector @Double 5 [5,4,3,1,1]

    describe "sortIndex" $ do
      it "returns sorted values and original indices" $ do
        let (vals, idxs) = A.sortIndex (A.vector @Double 4 [3,2,1,4]) 0 True
        vals  `shouldBe` A.vector @Double  4 [1,2,3,4]
        idxs  `shouldBe` A.vector @A.Word32 4 [2,1,0,3]

    describe "sortByKey" $ do
      it "sorts values by key order" $ do
        let (ks, vs) = A.sortByKey
              (A.vector @Double 4 [2,1,4,3])
              (A.vector @Double 4 [10,9,8,7])
              0 True
        ks `shouldBe` A.vector @Double 4 [1,2,3,4]
        vs `shouldBe` A.vector @Double 4 [9,10,7,8]

    describe "setUnique" $ do
      it "removes duplicate elements" $ do
        A.setUnique (A.vector @Double 4 [1,1,2,2]) True
          `shouldBe` A.vector @Double 2 [1,2]
      it "returns a single-element array from an all-same vector" $ do
        A.setUnique (A.vector @Double 3 [5,5,5]) True
          `shouldBe` A.vector @Double 1 [5]

    describe "setUnion" $ do
      it "produces the union of two sorted sets" $ do
        A.setUnion (A.vector @Double 3 [3,4,5]) (A.vector @Double 3 [1,2,3]) True
          `shouldBe` A.vector @Double 5 [1,2,3,4,5]

    describe "setIntersect" $ do
      it "produces the intersection of two sorted sets" $ do
        A.setIntersect (A.vector @Double 3 [3,4,5]) (A.vector @Double 3 [1,2,3]) True
          `shouldBe` A.vector @Double 1 [3]
      it "returns empty array for disjoint sets" $ do
        A.getDims (A.setIntersect (A.vector @Double 2 [1,2]) (A.vector @Double 2 [3,4]) True)
          `shouldBe` (0,1,1,1)

    -- Regression: infoFromArray3 was missing mask_, risking finalizer interference.
    -- iminAll and imaxAll are the primary users.
    it "iminAll returns correct value and index" $ do
      let arr = A.vector @Double 5 [3, 1, 4, 2, 5]
      A.iminAll arr `shouldBe` (1.0, 0.0, 1)
    it "imaxAll returns correct value and index" $ do
      let arr = A.vector @Double 5 [3, 1, 4, 1, 5]
      A.imaxAll arr `shouldBe` (5.0, 0.0, 4)

