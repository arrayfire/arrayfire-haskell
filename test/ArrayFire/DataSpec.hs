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
    describe "constant" $ do
      it "creates a scalar Float array" $
        constant @Float [1] 1 `shouldBe` scalar @Float 1
      it "creates a scalar Double array" $
        constant @Double [1] 2.5 `shouldBe` scalar @Double 2.5
      it "creates a scalar Int16 array" $
        constant @Int16 [1] 42 `shouldBe` scalar @Int16 42
      it "creates a scalar Int32 array" $
        constant @Int32 [1] (-7) `shouldBe` scalar @Int32 (-7)
      it "creates a scalar Word8 array" $
        constant @Word8 [1] 255 `shouldBe` scalar @Word8 255
      it "creates a scalar Word16 array" $
        constant @Word16 [1] 1000 `shouldBe` scalar @Word16 1000
      it "creates a scalar Word32 array" $
        constant @Word32 [1] 999 `shouldBe` scalar @Word32 999
      it "creates a CBool array" $
        constant @CBool [1] 1 `shouldBe` scalar @CBool 1
      it "creates a multi-element array with correct shape" $ do
        let a = constant @Double [3,3] 0
        getDims a `shouldBe` (3,3,1,1)
      it "all elements equal the scalar value" $
        constant @Float [4] 3.14 `shouldBe` vector @Float 4 [3.14, 3.14, 3.14, 3.14]

    describe "constantComplex" $ do
      it "creates a Complex Double array preserving imaginary part" $
        constantComplex [1] (1.0 :+ 2.0)
          `shouldBe` scalar @(Complex Double) (1.0 :+ 2.0)
      it "creates a Complex Float array preserving imaginary part" $
        constantComplex [1] (3.0 :+ 4.0 :: Complex Float)
          `shouldBe` scalar @(Complex Float) (3.0 :+ 4.0)
      it "creates a zero complex array" $
        constantComplex [2] (0 :+ 0 :: Complex Double)
          `shouldBe` vector @(Complex Double) 2 [0, 0]
      it "handles purely real complex values" $
        constantComplex [1] (5.0 :+ 0.0 :: Complex Double)
          `shouldBe` scalar @(Complex Double) (5.0 :+ 0.0)
      it "handles purely imaginary complex values" $
        constantComplex [1] (0.0 :+ 7.0 :: Complex Double)
          `shouldBe` scalar @(Complex Double) (0.0 :+ 7.0)

    describe "constantLong" $ do
      it "creates an Int array with value 1" $
        constantLong [1] 1 `shouldBe` scalar @Int 1
      it "creates an Int array with a negative value" $
        constantLong [1] (-42) `shouldBe` scalar @Int (-42)
      it "preserves maxBound :: Int without rounding" $
        constantLong [1] maxBound `shouldBe` scalar @Int maxBound
      it "preserves minBound :: Int without rounding" $
        constantLong [1] minBound `shouldBe` scalar @Int minBound
      it "creates a multi-element array" $
        constantLong [3] 7 `shouldBe` vector @Int 3 [7, 7, 7]

    describe "constantULong" $ do
      it "creates a Word64 array with value 1" $
        constantULong [1] 1 `shouldBe` scalar @Word64 1
      it "creates a Word64 array with value 0" $
        constantULong [1] 0 `shouldBe` scalar @Word64 0
      it "preserves maxBound :: Word64 without rounding" $
        constantULong [1] maxBound `shouldBe` scalar @Word64 maxBound
      it "creates a multi-element array" $
        constantULong [3] 100 `shouldBe` vector @Word64 3 [100, 100, 100]

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

    describe "reorder" $ do
      it "reorder [0,1] is identity for a 2D matrix" $ do
        let m = matrix @Double (3,4) [[1..3],[3..6],[6..9],[9..12]]
        reorder m [0,1] `shouldBe` m
      it "reorder [1,0] transposes a matrix" $ do
        let m = matrix @Double (2,3) [[1,2],[3,4],[5,6]]
        getDims (reorder m [1,0]) `shouldBe` (3,2,1,1)
      it "reorder [1,0] then [1,0] round-trips" $ do
        let m = matrix @Double (3,4) [[1..3],[3..6],[6..9],[9..12]]
        reorder (reorder m [1,0]) [1,0] `shouldBe` m
