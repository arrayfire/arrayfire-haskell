{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module ArrayFire.BLASSpec where

import ArrayFire    hiding (not)

import Data.Complex
import Test.Hspec
import Test.Hspec.QuickCheck (prop)

-- | Build a 4x4 'Double' matrix from an arbitrary (possibly short) list,
-- padding with zeros so the shape is always well-defined.
mat4 :: [Double] -> Array Double
mat4 xs = mkArray [4,4] (take 16 (xs ++ repeat 0))

-- | Element-wise closeness, tolerant of floating-point rounding in BLAS.
closeList :: [Double] -> [Double] -> Bool
closeList as bs =
  length as == length bs &&
  and (zipWith (\a b -> abs (a - b) <= 1e-9 + 1e-6 * max (abs a) (abs b)) as bs)

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
    it "Should perform gemm: alpha=1, A*I = A" $ do
      let a = matrix @Double (2,2) [[1,2],[3,4]]
          b = matrix @Double (2,2) [[1,0],[0,1]]
      gemm None None 1.0 a b `shouldBe` a
    it "Should perform gemm: alpha=2 scales the result" $ do
      -- b col-major: col0=[3,4], col1=[5,6]
      -- 2 * I * b = 2b → col0=[6,8], col1=[10,12]
      let a = matrix @Double (2,2) [[1,0],[0,1]]
          b = matrix @Double (2,2) [[3,4],[5,6]]
      gemm None None 2.0 a b `shouldBe` matrix @Double (2,2) [[6,8],[10,12]]
    it "Should perform gemm with transposed A" $ do
      let a = matrix @Double (2,2) [[1,3],[2,4]]
          b = matrix @Double (2,2) [[1,0],[0,1]]
      gemm Trans None 1.0 a b `shouldBe` matrix @Double (2,2) [[1,2],[3,4]]
    it "Should perform gemm: non-trivial A*B" $ do
      -- matrix (2,2) [[c0r0,c0r1],[c1r0,c1r1]] is column-major.
      -- A = [[1,3],[2,4]], B = [[5,7],[6,8]] (rows displayed by ArrayFire)
      -- A*B col0 = [1*5+3*6, 2*5+4*6] = [23,34]
      -- A*B col1 = [1*7+3*8, 2*7+4*8] = [31,46]
      let a = matrix @Double (2,2) [[1,2],[3,4]]
          b = matrix @Double (2,2) [[5,6],[7,8]]
      gemm None None 1.0 a b `shouldBe` matrix @Double (2,2) [[23,34],[31,46]]

    describe "algebraic properties" $ do
      -- Transposition only moves data, so double-transpose is exactly the
      -- identity (no floating-point rounding involved).
      prop "transpose is an involution" $ \(xs :: [Double]) ->
        let m = mat4 xs
        in toList (transpose (transpose m False) False) == toList m

      -- Multiplying by the identity matrix recovers the original.
      prop "A * I = A" $ \(xs :: [Double]) ->
        let a = mat4 xs
        in closeList (toList ((a `matmul` identity [4,4]) None None)) (toList a)

      -- (A^T B^T)^T = B A : transpose distributes over a product (reversed).
      prop "(A^T B^T)^T = B A" $ \(xs :: [Double]) (ys :: [Double]) ->
        let a   = mat4 xs
            b   = mat4 ys
            lhs = transpose ((transpose a False `matmul` transpose b False) None None) False
            rhs = (b `matmul` a) None None
        in closeList (toList lhs) (toList rhs)
