{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module ArrayFire.DataSpec where

import           Control.Exception
import           Data.Bits             (complement)
import           Data.Complex
import           Data.Word
import           Foreign.C.Types
import           GHC.Int
import           Prelude hiding (flip)
import           Test.Hspec
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck       ((==>))

import           ArrayFire hiding (not)

spec :: Spec
spec =
  describe "Data tests" $ do
    it "Should create constant Array" $ do
      constant @Float [1] 1 `shouldBe` 1
      constant @Double [1] 1 `shouldBe` 1
      constant @Int16 [1] 1 `shouldBe` 1
      constant @Int32 [1] 1 `shouldBe` 1
      constant @Int64 [1] 1 `shouldBe` 1
      constant @Int [1] 1 `shouldBe` 1
      constant @Word16 [1] 1 `shouldBe` 1
      constant @Word32 [1] 1 `shouldBe` 1
      constant @Word64 [1] 1 `shouldBe` 1
      constant @Word [1] 1 `shouldBe` 1
      constant @CBool [1] 1 `shouldBe` 1
      constant @(Complex Double) [1] (1.0 :+ 1.0)
        `shouldBe`
          constant @(Complex Double) [1] (1.0 :+ 1.0)
      constant @(Complex Float) [1] (1.0 :+ 1.0)
        `shouldBe`
          constant @(Complex Float) [1] (1.0 :+ 1.0)

    describe "arange" $ do
      it "generates a sequence along dim 0 for a 1D array" $ do
        arange @Double [5] (-1) `shouldBe` vector @Double 5 [0,1,2,3,4]
      it "generates a sequence along dim 1 for a 2D array" $ do
        arange @Double [3,2] 1 `shouldBe` mkArray @Double [3,2] [0,0,0,1,1,1]

    describe "iota" $ do
      it "generates a flat sequence without tiling" $ do
        iota @Double [5] [] `shouldBe` vector @Double 5 [0,1,2,3,4]
      it "tiles the sequence along dim 0" $ do
        iota @Double [3] [2] `shouldBe` vector @Double 6 [0,1,2,0,1,2]

    describe "identity" $ do
      it "creates a 2x2 identity matrix" $ do
        identity @Double [2,2]
          `shouldBe` mkArray @Double [2,2] [1,0,0,1]
      it "creates a 3x3 identity matrix" $ do
        identity @Double [3,3]
          `shouldBe` mkArray @Double [3,3] [1,0,0,0,1,0,0,0,1]

    describe "diagCreate" $ do
      it "creates a diagonal matrix from a vector (diag 0)" $ do
        diagCreate (vector @Double 3 [1,2,3]) 0
          `shouldBe` mkArray @Double [3,3] [1,0,0,0,2,0,0,0,3]
      it "creates a superdiagonal matrix (diag 1)" $ do
        diagCreate (vector @Double 2 [5,6]) 1
          `shouldBe` mkArray @Double [3,3] [0,0,0,5,0,0,0,6,0]

    describe "diagExtract" $ do
      it "extracts the main diagonal of a square matrix" $ do
        diagExtract (mkArray @Double [3,3] [1,0,0,0,2,0,0,0,3]) 0
          `shouldBe` vector @Double 3 [1,2,3]
      it "is the inverse of diagCreate on the main diagonal" $ do
        let v = vector @Double 4 [1,2,3,4]
        diagExtract (diagCreate v 0) 0 `shouldBe` v

    describe "lower" $ do
      it "extracts the lower triangular part (unit diagonal)" $ do
        let m = mkArray @Double [3,3] [1,2,3,4,5,6,7,8,9]
        lower m True
          `shouldBe` mkArray @Double [3,3] [1,2,3,0,1,6,0,0,1]
      it "extracts the lower triangular part (non-unit diagonal)" $ do
        let m = mkArray @Double [3,3] [1,2,3,4,5,6,7,8,9]
        lower m False
          `shouldBe` mkArray @Double [3,3] [1,2,3,0,5,6,0,0,9]

    describe "upper" $ do
      it "extracts the upper triangular part (unit diagonal)" $ do
        let m = mkArray @Double [3,3] [1,2,3,4,5,6,7,8,9]
        upper m True
          `shouldBe` mkArray @Double [3,3] [1,0,0,4,1,0,7,8,1]
      it "extracts the upper triangular part (non-unit diagonal)" $ do
        let m = mkArray @Double [3,3] [1,2,3,4,5,6,7,8,9]
        upper m False
          `shouldBe` mkArray @Double [3,3] [1,0,0,4,5,0,7,8,9]

    describe "tile" $ do
      it "tiles a scalar into a 3x3 array" $ do
        tile (scalar @Int 7) [3,3]
          `shouldBe` constant @Int [3,3] 7
      it "tiles a row vector along dim 0" $ do
        tile (mkArray @Int [1,3] [1,2,3]) [2,1]
          `shouldBe` mkArray @Int [2,3] [1,1,2,2,3,3]

    describe "moddims" $ do
      it "reshapes a vector into a matrix" $ do
        moddims (vector @Int 6 [1..6]) [2,3]
          `shouldBe` mkArray @Int [2,3] [1,2,3,4,5,6]
      it "reshapes a matrix back to a vector" $ do
        let v = vector @Int 6 [1..6]
        moddims (moddims v [2,3]) [6] `shouldBe` v

    describe "flat" $ do
      it "flattens a 2x3 matrix to a 6-element vector" $ do
        flat (mkArray @Int [2,3] [1,2,3,4,5,6])
          `shouldBe` vector @Int 6 [1,2,3,4,5,6]

    describe "flip" $ do
      it "reverses a vector (dim 0)" $ do
        flip (vector @Int 4 [1,2,3,4]) 0
          `shouldBe` vector @Int 4 [4,3,2,1]
      it "reverses columns of a matrix (dim 1)" $ do
        flip (mkArray @Int [2,2] [1,2,3,4]) 1
          `shouldBe` mkArray @Int [2,2] [3,4,1,2]

    describe "shift" $ do
      it "shifts a vector by 2 elements (wrapping)" $ do
        shift (vector @Double 4 [1,2,3,4]) 2 0 0 0
          `shouldBe` vector @Double 4 [3,4,1,2]

    describe "select" $ do
      it "selects elements from two arrays based on a boolean mask" $ do
        let cond = vector @CBool 4 [1,0,1,0]
            a    = vector @Double 4 [10,20,30,40]
            b    = vector @Double 4 [1,2,3,4]
        select cond a b `shouldBe` vector @Double 4 [10,2,30,4]

    describe "selectScalarR" $ do
      it "uses scalar for false positions" $ do
        let cond = vector @CBool 4 [1,0,1,0]
            a    = vector @Double 4 [10,20,30,40]
        selectScalarR cond a 99 `shouldBe` vector @Double 4 [10,99,30,99]

    describe "selectScalarL" $ do
      it "uses scalar for true positions" $ do
        let cond = vector @CBool 4 [1,0,1,0]
            b    = vector @Double 4 [1,2,3,4]
        selectScalarL cond 99 b `shouldBe` vector @Double 4 [99,2,99,4]

    it "Should join Arrays along the specified dimension" $ do
      join 0 (constant @Int [1, 3] 1) (constant @Int [1, 3] 2) `shouldBe` mkArray @Int [2, 3] [1, 2, 1, 2, 1, 2]
      join 1 (constant @Int [1, 2] 1) (constant @Int [1, 2] 2) `shouldBe` mkArray @Int [1, 4] [1, 1, 2, 2]
      joinMany 0 [constant @Int [1, 3] 1, constant @Int [1, 3] 2] `shouldBe` mkArray @Int [2, 3] [1, 2, 1, 2, 1, 2]
      joinMany 1 [constant @Int [1, 2] 1, constant @Int [1, 1] 2, constant @Int [1, 3] 3] `shouldBe` mkArray @Int [1, 6] [1, 1, 2, 3, 3, 3]

    describe "bitNot" $ do
      it "complements 0 to all-ones (-1 in two's complement) for Int32" $ do
        bitNot (scalar @Int32 0) `shouldBe` scalar @Int32 (-1)
      it "complements -1 to 0 for Int32" $ do
        bitNot (scalar @Int32 (-1)) `shouldBe` scalar @Int32 0
      it "complements 0 to maxBound for Word32" $ do
        bitNot (scalar @Word32 0) `shouldBe` scalar @Word32 maxBound
      it "bitNot . bitNot == id" $ do
        let v = vector @Int32 4 [0, 1, -1, 42]
        bitNot (bitNot v) `shouldBe` v
      prop "bitNot is an involution (Int32)" $ \(xs :: [Int32]) ->
        not (null xs) ==>
          toList (bitNot (bitNot (vector @Int32 (length xs) xs))) == xs
      prop "bitNot agrees with Data.Bits.complement (Int32)" $ \(xs :: [Int32]) ->
        not (null xs) ==>
          toList (bitNot (vector @Int32 (length xs) xs)) == map complement xs
