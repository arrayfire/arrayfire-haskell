{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module ArrayFire.ArraySpec where

import Control.Exception
import Data.Complex
import Data.Vector.Storable
import Data.Word
import Foreign.C.Types
import GHC.Int
import Test.Hspec

import ArrayFire

spec :: Spec
spec =
  describe "Array tests" $ do
    it "Should perform Array tests" $ do
      (1 + 1) `shouldBe` 2
    it "Should fail to create 0 dimension arrays" $ do
      let arr = mkArray @Int [0,0,0,0] [1..]
      evaluate arr `shouldThrow` anyException
    it "Should fail to create 0 length arrays" $ do
      let arr = mkArray @Int [0,0,0,1] []
      evaluate arr `shouldThrow` anyException
    it "Should fail to create 0 length arrays w/ 0 dimensions" $ do
      let arr = mkArray @Int [0,0,0,0] []
      evaluate arr `shouldThrow` anyException
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
      let arr = mkArray @Int [9,9,1,1] []
      getElements arr `shouldBe` 81
--    it "Should give an empty array" $ do
--      let arr = mkArray @Int [0,1,1,1] []
--      getElements arr `shouldBe` 0 -- uhhh this is wrong
--      isEmpty arr `shouldBe` True -- this is wrong too ...
    it "Should create a scalar array" $ do
      let arr = mkArray @Int [1] [1]
      isScalar arr `shouldBe` True
    it "Should get number of dims specified" $ do
      let arr = mkArray @Int [1,1,1,1] [1]
      getNumDims arr `shouldBe` 1
      let arr = mkArray @Int [2,3,4,5] [1]
      getNumDims arr `shouldBe` 4
      let arr = mkArray @Int [2,3,4] [1]
      getNumDims arr `shouldBe` 3
    it "Should get value of dims specified" $ do
      let arr = mkArray @Int [2,3,4,5] (repeat 1)
      getDims arr `shouldBe` (2,3,4,5)

    it "Should test Sparsity" $ do
      let arr = mkArray @Double [2,2,1,1] (repeat 1)
      isSparse arr `shouldBe` False

    it "Should make a Bit array" $ do
      let arr = mkArray @Bool [2,2] (repeat True)
      isBool arr `shouldBe` True

    it "Should make an integer array" $ do
      let arr = mkArray @Int [2,2] (repeat 1)
      isInteger arr `shouldBe` True

    it "Should make a Floating array" $ do
      let arr = mkArray @Double [2,2] (repeat 1)
      isFloating arr `shouldBe` True
      let arr = mkArray @Bool [2,2] (repeat True)
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
      toList (toVector arr) `shouldBe` (Prelude.replicate (30 * 30) 1)

      let arr = mkArray @CBool [4] [1,1,0,1]
      toList (toVector arr) `shouldBe` [1,1,0,1]

      let arr = mkArray @Int [100] [1..100]
      toList (toVector arr) `shouldBe` [1..100]

      let arr = mkArray @Int32 [100] [1..100]
      toList (toVector arr) `shouldBe` [1..100]

      let arr = mkArray @Int64 [100] [1..100]
      toList (toVector arr) `shouldBe` [1..100]

      let arr = mkArray @(Complex Float) [1] [1 :+ 1]
      toList (toVector arr) `shouldBe` [1 :+ 1]

      let arr = mkArray @(Complex Double) [1] [1 :+ 1]
      toList (toVector arr) `shouldBe` [1 :+ 1]

      let arr = mkArray @Int16 [10] [1..]
      toList (toVector arr) `shouldBe` [1..10]

      let arr = mkArray @Word16 [10] [1..10]
      toList (toVector arr) `shouldBe` [1..10]

      let arr = mkArray @Word32 [10] [1..10]
      toList (toVector arr) `shouldBe` [1..10]

      let arr = mkArray @Word64 [10] [1..10]
      toList (toVector arr) `shouldBe` [1..10]
