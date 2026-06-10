{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module ArrayFire.BLASSpec where

import ArrayFire    hiding (not, and, abs, max, mm, tr)

import Data.Complex
import Test.Hspec
import Test.Hspec.ApproxExpect (closeList)
import Test.Hspec.QuickCheck (prop)

-- | Build a 4x4 'Double' matrix from an arbitrary (possibly short) list,
-- padding with zeros so the shape is always well-defined.
mat4 :: [Double] -> Array Double
mat4 xs = mkArray [4,4] (take 16 (xs ++ repeat 0))

-- | Build a length-4 'Double' vector, padding with zeros.
vec4 :: [Double] -> Array Double
vec4 xs = vector 4 (take 4 (xs ++ repeat 0))

-- | Plain matrix product with default (None) operands.
mm :: Array Double -> Array Double -> Array Double
mm a b = (a `matmul` b) None None

-- | Transpose (no conjugation).
tr :: Array Double -> Array Double
tr a = transpose a False

-- | Scale every element of a 4x4 matrix by a constant.
scaleMat :: Double -> Array Double -> Array Double
scaleMat c a = mkArray [4,4] (map (c *) (toList a))


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

      -- Matrix multiplication is associative.
      prop "(A*B)*C = A*(B*C)" $ \(xs :: [Double]) (ys :: [Double]) (zs :: [Double]) ->
        let a = mat4 xs; b = mat4 ys; c = mat4 zs
        in closeList (toList (mm (mm a b) c)) (toList (mm a (mm b c)))

      -- Multiplication distributes over addition on the left.
      prop "A*(B+C) = A*B + A*C" $ \(xs :: [Double]) (ys :: [Double]) (zs :: [Double]) ->
        let a = mat4 xs; b = mat4 ys; c = mat4 zs
        in closeList (toList (mm a (b + c))) (toList (mm a b + mm a c))

      -- Multiplication distributes over addition on the right.
      prop "(A+B)*C = A*C + B*C" $ \(xs :: [Double]) (ys :: [Double]) (zs :: [Double]) ->
        let a = mat4 xs; b = mat4 ys; c = mat4 zs
        in closeList (toList (mm (a + b) c)) (toList (mm a c + mm b c))

      -- The identity is a left identity too (the existing case is right-sided).
      prop "I*A = A" $ \(xs :: [Double]) ->
        let a = mat4 xs
        in closeList (toList (mm (identity [4,4]) a)) (toList a)

      -- Transpose of a product reverses the order of the factors.
      prop "(A*B)^T = B^T * A^T" $ \(xs :: [Double]) (ys :: [Double]) ->
        let a = mat4 xs; b = mat4 ys
        in closeList (toList (tr (mm a b))) (toList (mm (tr b) (tr a)))

      -- Transpose is additive.
      prop "(A+B)^T = A^T + B^T" $ \(xs :: [Double]) (ys :: [Double]) ->
        let a = mat4 xs; b = mat4 ys
        in closeList (toList (tr (a + b))) (toList (tr a + tr b))

      -- Scalar factors pull through a product: (cA)*B = c(A*B).
      prop "(cA)*B = c(A*B)" $ \(c :: Double) (xs :: [Double]) (ys :: [Double]) ->
        let a = mat4 xs; b = mat4 ys
        in closeList (toList (mm (scaleMat c a) b)) (toList (scaleMat c (mm a b)))

      -- The zero matrix annihilates under multiplication.
      prop "A*0 = 0" $ \(xs :: [Double]) ->
        let a = mat4 xs
        in all (== 0) (toList (mm a (mat4 [])))

      -- gemm with alpha=1 and no transposition agrees with matmul.
      prop "gemm None None 1 A B = A*B" $ \(xs :: [Double]) (ys :: [Double]) ->
        let a = mat4 xs; b = mat4 ys
        in closeList (toList (gemm None None 1.0 a b)) (toList (mm a b))

      -- The dot product of real vectors is symmetric.
      prop "dot x y = dot y x" $ \(xs :: [Double]) (ys :: [Double]) ->
        let x = vec4 xs; y = vec4 ys
        in closeList (toList (dot x y None None)) (toList (dot y x None None))
