{-# LANGUAGE TypeApplications #-}
module ArrayFire.BLASSpec where

import ArrayFire    hiding (not)

import Data.Complex
import Test.Hspec

spec :: Spec
spec =
  describe "BLAS spec" $ do
    it "Should matmul two matrices" $ do
      (matrix @Double (2,2) [[2,2],[2,2]] `matmul` matrix @Double (2,2) [[2,2],[2,2]]) None None
        `shouldBe` matrix @Double (2,2) [[8,8],[8,8]]
    it "Should dot product two vectors" $ do
      dot (vector @Double 2 (repeat 2)) (vector @Double 2 (repeat 2)) None None
        `shouldBe` scalar @Double 8
    it "Should produce scalar dot product between two vectors as a Complex number" $ do
      dotAll (vector @Double 2 (repeat 2)) (vector @Double 2 (repeat 2)) None None
        `shouldBe` 8.0 :+ 0.0
    it "Should take the transpose of a matrix" $ do
      transpose (matrix @Double (2,2) [[1,1],[2,2]]) False
        `shouldBe` matrix @Double (2,2) [[1,2],[1,2]]
    it "Should take the transpose of a matrix in place" $ do
      -- transposeInPlace is an IO () that mutates the underlying C buffer.
      -- All Haskell references sharing the same ForeignPtr see the result.
      -- Do not use the original binding after calling this.
      let m = matrix @Double (2,2) [[1,1],[2,2]]
      transposeInPlace m False
      m `shouldBe` matrix @Double (2,2) [[1,2],[1,2]]
    it "Should perform gemm: C = 1*A*B + 0*C (identity scaling)" $ do
      let a = matrix @Double (2,2) [[1,2],[3,4]]
          b = matrix @Double (2,2) [[1,0],[0,1]]
      gemm None None 1.0 a b 0.0 `shouldBe` a
    it "Should perform gemm: C = alpha*A*B with alpha=2" $ do
      -- b is column-major: col0=[3,4], col1=[5,6] → matrix [[3,5],[4,6]]
      -- 2 * I * b = 2b → col0=[6,8], col1=[10,12]
      let a = matrix @Double (2,2) [[1,0],[0,1]]
          b = matrix @Double (2,2) [[3,4],[5,6]]
      gemm None None 2.0 a b 0.0 `shouldBe` matrix @Double (2,2) [[6,8],[10,12]]
    it "Should perform gemm with transposed A: C = A^T * B" $ do
      let a = matrix @Double (2,2) [[1,3],[2,4]]
          b = matrix @Double (2,2) [[1,0],[0,1]]
      gemm Trans None 1.0 a b 0.0 `shouldBe` matrix @Double (2,2) [[1,2],[3,4]]
