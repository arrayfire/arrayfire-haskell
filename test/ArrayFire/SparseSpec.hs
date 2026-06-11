{-# LANGUAGE TypeApplications #-}
module ArrayFire.SparseSpec where

import qualified ArrayFire as A
import           Data.Int
import           Test.Hspec

-- 3×3 diagonal matrix diag(1,2,3), stored column-major:
-- col0=[1,0,0], col1=[0,2,0], col2=[0,0,3]
diag3 :: A.Array Double
diag3 = A.mkArray @Double [3,3] [1,0,0, 0,2,0, 0,0,3]

spec :: Spec
spec =
  describe "Sparse" $ do

    describe "createSparseArrayFromDense" $ do
      it "NNZ equals number of non-zero elements" $ do
        A.sparseGetNNZ (A.createSparseArrayFromDense diag3 A.CSR) `shouldBe` 3
      it "fully-dense matrix has NNZ equal to element count" $ do
        let full = A.mkArray @Double [2,2] [1,2,3,4]
        A.sparseGetNNZ (A.createSparseArrayFromDense full A.CSR) `shouldBe` 4
      it "storage format is preserved" $ do
        A.sparseGetStorage (A.createSparseArrayFromDense diag3 A.CSR) `shouldBe` A.CSR
      it "COO storage format is preserved" $ do
        A.sparseGetStorage (A.createSparseArrayFromDense diag3 A.COO) `shouldBe` A.COO

    describe "sparseToDense" $ do
      it "CSR round-trip preserves all values" $ do
        A.sparseToDense (A.createSparseArrayFromDense diag3 A.CSR) `shouldBe` diag3
      it "COO round-trip preserves all values" $ do
        let coo = A.createSparseArrayFromDense diag3 A.COO
        A.sparseToDense (A.sparseConvertTo coo A.CSR) `shouldBe` diag3

    describe "sparseConvertTo" $ do
      it "CSR → COO preserves NNZ" $ do
        let coo = A.sparseConvertTo (A.createSparseArrayFromDense diag3 A.CSR) A.COO
        A.sparseGetNNZ coo `shouldBe` 3
      it "CSR → COO storage tag changes" $ do
        let coo = A.sparseConvertTo (A.createSparseArrayFromDense diag3 A.CSR) A.COO
        A.sparseGetStorage coo `shouldBe` A.COO
      it "CSR → COO → Dense recovers original matrix" $ do
        let coo = A.sparseConvertTo (A.createSparseArrayFromDense diag3 A.CSR) A.COO
        A.sparseToDense (A.sparseConvertTo coo A.CSR) `shouldBe` diag3

    describe "sparseGetValues" $ do
      it "diagonal matrix CSR values are the diagonal entries in row order" $ do
        let sp = A.createSparseArrayFromDense diag3 A.CSR
        A.sparseGetValues sp `shouldBe` A.vector @Double 3 [1,2,3]

    describe "sparseGetRowIdx / sparseGetColIdx" $ do
      -- The underlying arrays are s32; we check length, not raw values.
      it "CSR row pointer array has nrows+1 elements" $ do
        let sp = A.createSparseArrayFromDense diag3 A.CSR
        A.getElements (A.sparseGetRowIdx sp) `shouldBe` 4
      it "CSR column index array has NNZ elements" $ do
        let sp = A.createSparseArrayFromDense diag3 A.CSR
        A.getElements (A.sparseGetColIdx sp) `shouldBe` 3

    describe "sparseGetInfo" $ do
      it "values component matches sparseGetValues" $ do
        let sp = A.createSparseArrayFromDense diag3 A.CSR
            (vals, _, _, _) = A.sparseGetInfo sp
        vals `shouldBe` A.sparseGetValues sp
      it "storage tag matches sparseGetStorage" $ do
        let sp = A.createSparseArrayFromDense diag3 A.CSR
            (_, _, _, storage) = A.sparseGetInfo sp
        storage `shouldBe` A.sparseGetStorage sp

    describe "createSparseArray" $ do
      -- Build a 3x3 diagonal sparse matrix directly from COO components:
      -- values = [1,2,3], rowIdx = [0,1,2], colIdx = [0,1,2]
      it "NNZ equals length of supplied values array" $ do
        let vals   = A.vector @Double 3 [1,2,3]
            rowIdx = A.vector @Int32  3 [0,1,2]
            colIdx = A.vector @Int32  3 [0,1,2]
            sp     = A.createSparseArray 3 3 vals rowIdx colIdx A.COO
        A.sparseGetNNZ sp `shouldBe` 3
      it "storage format matches the requested format" $ do
        let vals   = A.vector @Double 3 [1,2,3]
            rowIdx = A.vector @Int32  3 [0,1,2]
            colIdx = A.vector @Int32  3 [0,1,2]
            sp     = A.createSparseArray 3 3 vals rowIdx colIdx A.COO
        A.sparseGetStorage sp `shouldBe` A.COO
      it "converting to dense recovers the diagonal matrix" $ do
        let vals   = A.vector @Double 3 [1,2,3]
            rowIdx = A.vector @Int32  3 [0,1,2]
            colIdx = A.vector @Int32  3 [0,1,2]
            sp     = A.createSparseArray 3 3 vals rowIdx colIdx A.COO
        A.sparseToDense (A.sparseConvertTo sp A.CSR) `shouldBe` diag3
