{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module ArrayFire.ArraySpec where

import Control.Exception
import Data.Complex
import qualified Data.Vector.Storable as V
import Data.Word
import Foreign.C.Types
import GHC.Int
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck       ((==>))

import ArrayFire hiding (not)

spec :: Spec
spec =
  describe "Array tests" $ do
    it "Should add two scalar arrays" $ do
      (scalar @Int 1 + scalar @Int 1) `shouldBe` scalar @Int 2
    it "Should create a 0 dimension array" $ do
      getElements (mkArray @Int [3,0,1,1] []) `shouldBe` 0
    it "Should create a 0 length array" $ do
      getElements (mkArray @Int [0,0,0,1] []) `shouldBe` 0
    it "Should create a 0 length array w/ 0 dimensions" $ do
      getElements (mkArray @Int [0,0,0,0] []) `shouldBe` 0
    it "Should create a column vector" $ do
      let arr = mkArray @Int [9,1,1,1] (repeat 9)
      isColumn arr `shouldBe` True
    it "Should create a row vector" $ do
      let arr = mkArray @Int [1,9,1,1] (repeat 9)
      isRow arr `shouldBe` True
    it "Should create a vector" $ do
      let arr = mkArray @Int [9,1,1,1] (repeat 9)
      isVector arr `shouldBe` True
    it "Should create a vector" $ do
      let arr = mkArray @Int [1,9,1,1] (repeat 9)
      isVector arr `shouldBe` True
    it "Should copy an array" $ do
      let arr = mkArray @Int [9,9,1,1] (repeat 9)
      let newArray = copyArray arr
      newArray `shouldBe` arr
    it "Should modify manual eval flag" $ do
      setManualEvalFlag False
      (`shouldBe` False) =<< getManualEvalFlag
    it "Should return the number of elements" $ do
      let arr = mkArray @Int [9,9,1,1] [1..]
      getElements arr `shouldBe` 81
    it "Should give an empty array" $ do
      let arr = mkArray @Int [0,1,1,1] []
      getElements arr `shouldBe` 0
      isEmpty arr `shouldBe` True
    it "Should create a scalar array" $ do
      let arr = mkArray @Int [1] [1]
      isScalar arr `shouldBe` True
    it "Should get number of dims specified" $ do
      let arr = mkArray @Int [1,1,1,1] [1]
      getNumDims arr `shouldBe` 1
      let arr = mkArray @Int [2,3,4,5] [1..]
      getNumDims arr `shouldBe` 4
      let arr = mkArray @Int [2,3,4] [1..]
      getNumDims arr `shouldBe` 3
    it "Should get value of dims specified" $ do
      let arr = mkArray @Int [2,3,4,5] (repeat 1)
      getDims arr `shouldBe` (2,3,4,5)

    it "Should test Sparsity" $ do
      let arr = mkArray @Double [2,2,1,1] (repeat 1)
      isSparse arr `shouldBe` False

    it "Should make a Bit array" $ do
      let arr = mkArray @CBool [2,2] [1,1,1,1]
      isBool arr `shouldBe` True

    it "Should make an integer array" $ do
      let arr = mkArray @Int [2,2] (repeat 1)
      isInteger arr `shouldBe` True

    it "Should make a Floating array" $ do
      let arr = mkArray @Double [2,2] (repeat 1)
      isFloating arr `shouldBe` True
      let arr = mkArray @CBool [2,2] (repeat 1)
      isFloating arr `shouldBe` False

    it "Should make a Complex array" $ do
      let arr = mkArray @(Complex Double) [2,2] (repeat 1)
      isComplex arr `shouldBe` True
      isReal arr `shouldBe` False

    it "Should make a Real array" $ do
      let arr = mkArray @Double [2,2] (repeat 1)
      isReal arr `shouldBe` True
      isComplex arr `shouldBe` False

    it "Should make a Double precision array" $ do
      let arr = mkArray @Double [2,2] (repeat 1)
      isDouble arr `shouldBe` True
      isSingle arr `shouldBe` False

    it "Should make a Single precision array" $ do
      let arr = mkArray @Float [2,2] (repeat 1)
      isDouble arr `shouldBe` False
      isSingle arr `shouldBe` True

    it "Should make a Real floating array" $ do
      let arr = mkArray @Float [2,2] (repeat 1)
      isRealFloating arr `shouldBe` True
      let arr = mkArray @Double [2,2] (repeat 1)
      isRealFloating arr `shouldBe` True

    it "Should get reference count" $ do
      let arr1 = mkArray @Float [2,2] (repeat 1)
          arr2 = retainArray arr1
          arr3 = retainArray arr2
      getDataRefCount arr3 `shouldBe` 3

    it "Should convert an array to a list" $ do
      let arr = mkArray @Double [30,30] (repeat 1)
      toList arr `shouldBe` Prelude.replicate (30 * 30) 1

      let arr = mkArray @Float [10,10] (repeat (5.5))
      toList arr `shouldBe` Prelude.replicate 100 5.5

      let arr = mkArray @CBool [4] [1,1,0,1]
      toList arr `shouldBe` [1,1,0,1]

      let arr = mkArray @Int16 [10] [1..]
      toList arr `shouldBe` [1..10]

      let arr = mkArray @Int32 [100] [1..100]
      toList arr `shouldBe` [1..100]

      let arr = mkArray @Int64 [100] [1..100]
      toList arr `shouldBe` [1..100]

      let arr = mkArray @Int [100] [1..100]
      toList arr `shouldBe` [1..100]

      let arr = mkArray @(Complex Float) [1] [1 :+ 1]
      toList arr `shouldBe` [1 :+ 1]

      let arr = mkArray @(Complex Double) [1] [1 :+ 1]
      toList arr `shouldBe` [1 :+ 1]

      let arr = mkArray @Word16 [10] [1..10]
      toList arr `shouldBe` [1..10]

      let arr = mkArray @Word32 [10] [1..10]
      toList arr `shouldBe` [1..10]

      let arr = mkArray @Word64 [10] [1..10]
      toList arr `shouldBe` [1..10]

      let arr = mkArray @Word [10] [1..10]
      toList arr `shouldBe` [1..10]

    -- Regression: toVector previously allocated len*size bytes instead of size,
    -- causing quadratic memory use. These round-trips verify correct element count
    -- and values at sizes where the bug was most wasteful.
    describe "toVector round-trip" $ do
      it "preserves all elements for a 1000-element Double array" $ do
        let xs  = [1..1000] :: [Double]
            arr = mkArray @Double [1000] xs
        V.toList (toVector arr) `shouldBe` xs
      it "preserves all elements for a 500-element Int array" $ do
        let xs  = [1..500] :: [Int]
            arr = mkArray @Int [500] xs
        V.toList (toVector arr) `shouldBe` xs
      it "length of toVector matches getElements" $ do
        let arr = mkArray @Double [7, 13] (repeat 0)
        V.length (toVector arr) `shouldBe` getElements arr

    describe "fromVector" $ do
      it "round-trips a Double vector" $ do
        let xs  = V.fromList [1..10 :: Double]
            arr = fromVector @Double [10] xs
        toVector arr `shouldBe` xs
      it "round-trips an Int vector" $ do
        let xs  = V.fromList [1..100 :: Int]
            arr = fromVector @Int [100] xs
        toVector arr `shouldBe` xs
      it "round-trips a Complex Double vector" $ do
        let xs  = V.fromList [1 :+ 2, 3 :+ 4 :: Complex Double]
            arr = fromVector @(Complex Double) [2] xs
        toVector arr `shouldBe` xs
      it "produces the same result as mkArray" $ do
        let xs  = [1..25 :: Double]
            arr1 = mkArray @Double [5,5] xs
            arr2 = fromVector @Double [5,5] (V.fromList xs)
        arr2 `shouldBe` arr1
      it "throws on dimension mismatch" $ do
        let xs = V.fromList [1,2,3 :: Double]
        evaluate (fromVector @Double [4] xs) `shouldThrow` anyException
      -- Round-trip is data-preserving (no arithmetic), so equality is exact.
      -- This also guards the toVector allocation fix against host over-reads.
      prop "toVector . fromVector == id (Double)" $ \(xs :: [Double]) ->
        not (null xs) ==>
          let v = V.fromList xs
          in V.toList (toVector (fromVector @Double [length xs] v)) == xs
      prop "toVector . fromVector == id (Int)" $ \(xs :: [Int]) ->
        not (null xs) ==>
          let v = V.fromList xs
          in V.toList (toVector (fromVector @Int [length xs] v)) == xs

    describe "cube" $ do
      it "creates a 2x2x2 cube with correct dims" $ do
        let c = cube @Double (2,2,2)
                  [ [[1,2],[3,4]], [[5,6],[7,8]] ]
        getDims c `shouldBe` (2,2,2,1)
      it "creates a 2x2x2 cube with correct element count" $ do
        let c = cube @Double (2,2,2)
                  [ [[1,2],[3,4]], [[5,6],[7,8]] ]
        getElements c `shouldBe` 8
      it "all-constant cube equals constant array" $ do
        let c = cube @Double (2,2,2)
                  [ [[3,3],[3,3]], [[3,3],[3,3]] ]
        c `shouldBe` mkArray @Double [2,2,2] (replicate 8 3)

    describe "tensor" $ do
      it "creates a 2x2x2x2 tensor with correct dims" $ do
        let t = tensor @Double (2,2,2,2)
                  [ [ [[1,2],[3,4]], [[5,6],[7,8]] ]
                  , [ [[1,2],[3,4]], [[5,6],[7,8]] ]
                  ]
        getDims t `shouldBe` (2,2,2,2)
      it "creates a 2x2x2x2 tensor with correct element count" $ do
        let t = tensor @Double (2,2,2,2)
                  [ [ [[1,2],[3,4]], [[5,6],[7,8]] ]
                  , [ [[1,2],[3,4]], [[5,6],[7,8]] ]
                  ]
        getElements t `shouldBe` 16
      it "all-constant tensor equals constant array" $ do
        let t = tensor @Double (2,2,2,2)
                  [ [ [[5,5],[5,5]], [[5,5],[5,5]] ]
                  , [ [[5,5],[5,5]], [[5,5],[5,5]] ]
                  ]
        t `shouldBe` mkArray @Double [2,2,2,2] (replicate 16 5)
