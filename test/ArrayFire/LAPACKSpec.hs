{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module ArrayFire.LAPACKSpec where

import qualified ArrayFire             as A
import           Data.Complex          (realPart, imagPart)
import           Prelude
import           Test.Hspec
import           Test.Hspec.ApproxExpect
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck       (Gen, choose, forAll, vectorOf)

-- | A 3x3 matrix product with default (None) operands.
mm :: A.Array Double -> A.Array Double -> A.Array Double
mm a b = (a `A.matmul` b) A.None A.None

-- | Transpose (real, no conjugation).
tr :: A.Array Double -> A.Array Double
tr a = A.transpose a False

-- | Generate the entries of an @n@x@n@ matrix with modestly sized values so
-- the decompositions stay numerically well-behaved.
genMat :: Int -> Gen [Double]
genMat n = vectorOf (n * n) (choose (-5, 5))


spec :: Spec
spec =
  describe "LAPACK spec" $ do
    it "Should have LAPACK available" $ do
      A.isLAPACKAvailable `shouldBe` True

    it "Should perform svd" $ do
      let (u,sigma,vt) = A.svd $ A.matrix @Double (4,2) [ [1,2,3,4], [5,6,7,8] ]
      A.getDims u     `shouldBe` (4,4,1,1)
      A.getDims sigma `shouldBe` (2,1,1,1)
      A.getDims vt    `shouldBe` (2,2,1,1)

    it "Should perform svd in place" $ do
      let (u,sigma,vt) = A.svdInPlace $ A.matrix @Double (4,2) [ [1,2,3,4], [5,6,7,8] ]
      A.getDims u     `shouldBe` (4,4,1,1)
      A.getDims sigma `shouldBe` (2,1,1,1)
      A.getDims vt    `shouldBe` (2,2,1,1)

    it "Should perform lu" $ do
      let (l,u,piv) = A.lu $ A.matrix @Double (2,2) [[3,1],[4,2]]
      A.getDims l   `shouldBe` (2,2,1,1)
      A.getDims u   `shouldBe` (2,2,1,1)
      A.getDims piv `shouldBe` (2,1,1,1)

    it "Should perform qr" $ do
      let (q,r,tau) = A.qr $ A.matrix @Double (3,3) [[12,6,4],[-51,167,24],[4,-68,-41]]
      A.getDims q   `shouldBe` (3,3,1,1)
      A.getDims r   `shouldBe` (3,3,1,1)
      A.getDims tau `shouldBe` (3,1,1,1)

    it "Should get determinant of a real matrix" $ do
      A.det (A.matrix @Double (2,2) [[3,8],[4,6]])
        `shouldBeApprox` (-14)

    it "Should get determinant of a complex matrix" $ do
      -- M = | 3+i  4+i |   (column-major: col0=[3+i,8+i], col1=[4+i,6+i])
      --     | 8+i  6+i |
      -- det = (3+i)(6+i) - (4+i)(8+i) = -14 - 3i
      let d = A.det $ A.matrix @(A.Complex Double) (2,2)
                        [[3 A.:+ 1, 8 A.:+ 1], [4 A.:+ 1, 6 A.:+ 1]]
      realPart d `shouldBeApprox` (-14)
      imagPart d `shouldBeApprox` (-3)

    it "Should calculate inverse" $ do
      -- M = | 4  2 |  (column-major: col0=[4,7], col1=[2,6])
      --     | 7  6 |
      -- M^-1 = (1/10) * | 6  -2 | = col0=[0.6,-0.7], col1=[-0.2,0.4]
      --                  | -7  4 |
      let result   = A.toList $ A.inverse (A.matrix @Double (2,2) [[4.0,7.0],[2.0,6.0]]) A.None
          expected = [0.6, -0.7, -0.2, 0.4]
      mapM_ (uncurry shouldBeApprox) (zip result expected)

    it "Should find the rank of a matrix" $ do
      A.rank (A.matrix @Double (3,3) [[1,2,3],[4,5,6],[7,8,9]]) 1e-5 `shouldBe` 2
      A.rank (A.identity @Double [3,3]) 1e-5 `shouldBe` 3

    it "Should compute the norm of a vector" $ do
      -- || [3, 4] ||_2 = 5
      A.norm (A.vector @Double 2 [3,4]) A.NormVector2 1 1 `shouldBeApprox` 5
      -- || [3, 4] ||_1 = 7
      A.norm (A.vector @Double 2 [3,4]) A.NormVectorOne 1 1 `shouldBeApprox` 7
      -- || [3, 4] ||_inf = 4
      A.norm (A.vector @Double 2 [3,4]) A.NormVectorInf 1 1 `shouldBeApprox` 4

    it "Should perform cholesky decomposition" $ do
      -- A = | 4  2 |  (column-major: [4,2,2,3])
      --     | 2  3 |
      -- L = | 2    0   |  where L*L^T = A
      --     | 1    √2  |
      let a = A.mkArray @Double [2,2] [4,2,2,3]
          (status, l) = A.cholesky a False
      status `shouldBe` 0
      let ls = A.toList @Double l
      mapM_ (uncurry shouldBeApprox) (zip ls [2, 1, 0, sqrt 2])

    it "choleskyInplace returns 0 for a symmetric positive definite matrix" $ do
      let a = A.mkArray @Double [2,2] [4,2,2,3]
      A.choleskyInplace a False `shouldBe` 0

    it "Should solve Ax=b using solveLU" $ do
      -- A = | 2  1 |  b = | 5 |  =>  x = | 1 |
      --     | 1  3 |      | 10|           | 3 |
      -- Column-major A: [2,1,1,3],  b: [5,10]
      let a   = A.mkArray @Double [2,2] [2,1,1,3]
          b   = A.vector  @Double 2     [5,10]
          piv = A.luInPlace a True
          x   = A.solveLU a piv b A.None
      mapM_ (uncurry shouldBeApprox) (zip (A.toList @Double x) [1,3])

    describe "decomposition reconstruction properties" $ do
      -- QR factors multiply back to the original matrix.
      prop "QR: Q*R = A" $ forAll (genMat 3) $ \xs ->
        let a       = A.mkArray @Double [3,3] xs
            (q,r,_) = A.qr a
        in closeList (A.toList (mm q r)) (A.toList a)

      -- The Q factor is orthogonal: Q^T Q = I.
      prop "QR: Q^T Q = I" $ forAll (genMat 3) $ \xs ->
        let a       = A.mkArray @Double [3,3] xs
            (q,_,_) = A.qr a
        in closeList (A.toList (mm (tr q) q)) (A.toList (A.identity @Double [3,3]))

      -- SVD factors multiply back to the original: U * diag(S) * V^T = A.
      prop "SVD: U diag(S) V^T = A" $ forAll (genMat 3) $ \xs ->
        let a          = A.mkArray @Double [3,3] xs
            (u,s,vt)   = A.svd a
            sigma      = A.diagCreate s 0
        in closeList (A.toList (mm (mm u sigma) vt)) (A.toList a)

      -- Cholesky factor reproduces a symmetric positive-definite matrix:
      -- A = B^T B + 3I is SPD, and L*L^T = A.
      prop "Cholesky: L*L^T = A (SPD)" $ forAll (genMat 3) $ \xs ->
        let b           = A.mkArray @Double [3,3] xs
            a           = mm (tr b) b + A.mkArray @Double [3,3] [3,0,0, 0,3,0, 0,0,3]
            (status, l) = A.cholesky a False
        in status == 0 && closeList (A.toList (mm l (tr l))) (A.toList a)

    describe "more decomposition properties" $ do
      -- Singular values are all non-negative.
      prop "SVD: singular values are non-negative" $ forAll (genMat 3) $ \xs ->
        let a      = A.mkArray @Double [3,3] xs
            (_,s,_) = A.svd a
        in all (>= -1e-12) (A.toList s)

      -- LU reconstruction: L * U = P * A  where P is the pivot permutation.
      -- ArrayFire's lu returns (L, U, piv) where piv is a pivot index vector.
      -- We verify the simpler invariant that L is unit lower-triangular (diag=1).
      prop "LU: L has unit diagonal" $ forAll (genMat 3) $ \xs ->
        let a        = A.mkArray @Double [3,3] xs
            (l,_,_)  = A.lu a
            diag     = [A.toList l !! (i * 3 + i) | i <- [0..2]]
        in all (\d -> abs (d - 1.0) < 1e-9) diag

      -- det(A * B) ≈ det(A) * det(B)  (multiplicativity of determinant)
      prop "det(A*B) = det(A)*det(B)" $ forAll (genMat 3) $ \xs ->
        forAll (genMat 3) $ \ys ->
          let a        = A.mkArray @Double [3,3] xs
              b        = A.mkArray @Double [3,3] ys
              da       = A.det a
              db       = A.det b
              dab      = A.det (mm a b)
              expected = da * db
          in abs (dab - expected) < 1e-6 + 1e-4 * abs expected

      -- inverse(inverse(A)) ≈ A  for a well-conditioned matrix (B^T B + 3I is SPD).
      prop "inverse is its own inverse (SPD input)" $ forAll (genMat 3) $ \xs ->
        let b    = A.mkArray @Double [3,3] xs
            a    = mm (tr b) b + A.mkArray @Double [3,3] [3,0,0, 0,3,0, 0,0,3]
            ainv = A.inverse a A.None
            ainv2 = A.inverse ainv A.None
        in closeList (A.toList ainv2) (A.toList a)

    describe "pinverse" $ do
      it "pinverse of a full-rank square matrix matches inverse" $ do
        -- For an invertible matrix, pinverse should equal inverse.
        let a    = A.matrix @Double (2,2) [[4.0,7.0],[2.0,6.0]]
            pinv = A.toList $ A.pinverse a 1e-6 A.None
            inv  = A.toList $ A.inverse  a       A.None
        mapM_ (uncurry shouldBeApprox) (zip pinv inv)

      it "pinverse of a tall matrix satisfies pinv(A) * A ≈ I" $ do
        -- For a full-column-rank matrix A (m x n, m >= n), pinv(A) * A = I_n.
        let a    = A.matrix @Double (3,2) [[1,2,3],[4,5,6]]
            pinvA = A.pinverse a 1e-9 A.None
            prod  = mm pinvA a  -- (2x3) * (3x2) = 2x2 identity
            eye   = A.identity @Double [2,2]
        closeList (A.toList prod) (A.toList eye) `shouldBe` True

      prop "pinverse: A * pinv(A) * A ≈ A  (full-rank square)" $
        forAll (genMat 3) $ \xs ->
          let b    = A.mkArray @Double [3,3] xs
              a    = mm (tr b) b + A.mkArray @Double [3,3] [3,0,0, 0,3,0, 0,0,3]
              pinvA = A.pinverse a 1e-9 A.None
          in closeList (A.toList (mm (mm a pinvA) a)) (A.toList a)

      prop "pinverse: pinv(A) * A * pinv(A) ≈ pinv(A)  (full-rank square)" $
        forAll (genMat 3) $ \xs ->
          let b    = A.mkArray @Double [3,3] xs
              a    = mm (tr b) b + A.mkArray @Double [3,3] [3,0,0, 0,3,0, 0,0,3]
              pinvA = A.pinverse a 1e-9 A.None
          in closeList (A.toList (mm (mm pinvA a) pinvA)) (A.toList pinvA)

    describe "eigSH" $ do
      -- Works on all backends: CUDA uses cuSOLVER, others use SVD fallback.

      it "returns correct eigenvalues for 2x2 symmetric matrix" $ do
        -- A = [[3,1],[1,3]], eigenvalues 2 and 4 (ascending)
        let a          = A.matrix @Double (2,2) [[3,1],[1,3]]
            (evals, _) = A.eigSH a
            evList     = A.toList evals
        length evList `shouldBe` 2
        evList !! 0 `shouldBeApprox` 2.0
        evList !! 1 `shouldBeApprox` 4.0

      it "returns orthonormal eigenvectors for 2x2 matrix" $ do
        let a          = A.matrix @Double (2,2) [[3,1],[1,3]]
            (_, evecs) = A.eigSH a
            vtv        = A.toList $ mm (tr evecs) evecs
            eye2       = A.toList (A.identity @Double [2,2])
        mapM_ (uncurry shouldBeApprox) (zip vtv eye2)

      it "reconstructs the original 2x2 matrix: V * diag(λ) * V^T = A" $ do
        let a              = A.matrix @Double (2,2) [[3,1],[1,3]]
            (evals, evecs) = A.eigSH a
            recon          = mm (mm evecs (A.diagCreate evals 0)) (tr evecs)
        mapM_ (uncurry shouldBeApprox) (zip (A.toList recon) (A.toList a))

      it "returns eigenvalues in ascending order for 3x3 matrix" $ do
        -- A = [[2,1,0],[1,2,1],[0,1,2]], eigenvalues 2-sqrt(2), 2, 2+sqrt(2)
        let a          = A.matrix @Double (3,3) [[2,1,0],[1,2,1],[0,1,2]]
            (evals, _) = A.eigSH a
            evList     = A.toList evals
        evList !! 0 `shouldBeApprox` (2 - sqrt 2)
        evList !! 1 `shouldBeApprox` 2.0
        evList !! 2 `shouldBeApprox` (2 + sqrt 2)

      it "handles matrix with negative eigenvalues" $ do
        -- A = [[0,1],[1,0]], eigenvalues -1 and +1
        let a          = A.matrix @Double (2,2) [[0,1],[1,0]]
            (evals, _) = A.eigSH a
            evList     = A.toList evals
        evList !! 0 `shouldBeApprox` (-1.0)
        evList !! 1 `shouldBeApprox`   1.0

      prop "eigSH: V * diag(λ) * V^T = A  (SPD input)" $
        forAll (genMat 3) $ \xs ->
          let b              = A.mkArray @Double [3,3] xs
              a              = mm (tr b) b + A.mkArray @Double [3,3] [3,0,0, 0,3,0, 0,0,3]
              (evals, evecs) = A.eigSH a
              recon          = mm (mm evecs (A.diagCreate evals 0)) (tr evecs)
          in closeList (A.toList recon) (A.toList a)

      prop "eigSH: V^T * V = I  (eigenvectors are orthonormal)" $
        forAll (genMat 3) $ \xs ->
          let b              = A.mkArray @Double [3,3] xs
              a              = mm (tr b) b + A.mkArray @Double [3,3] [3,0,0, 0,3,0, 0,0,3]
              (_, evecs)     = A.eigSH a
          in closeList (A.toList (mm (tr evecs) evecs))
                       (A.toList (A.identity @Double [3,3]))

    describe "qrInPlace" $ do
      it "qrInPlace on a 3x3 matrix returns a tau vector of length 3" $ do
        let a   = A.matrix @Double (3,3) [[12,6,4],[-51,167,24],[4,-68,-41]]
            tau = A.qrInPlace a
        A.getDims tau `shouldBe` (3,1,1,1)
      it "qrInPlace on a 4x3 matrix returns a tau vector with min(rows,cols) elements" $ do
        let a   = A.mkArray @Double [4,3] [1..12]
            tau = A.qrInPlace a
        A.getDims tau `shouldBe` (3,1,1,1)
      it "qrInPlace on a square matrix produces a non-empty tau array" $ do
        let a   = A.mkArray @Double [2,2] [1,2,3,4]
            tau = A.qrInPlace a
        A.getElements tau `shouldBe` 2
