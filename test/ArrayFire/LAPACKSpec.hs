{-# LANGUAGE TypeApplications #-}
module ArrayFire.LAPACKSpec where

import qualified ArrayFire            as A
import           ArrayFire.Backend    (getActiveBackend)
import           ArrayFire.Types      (Backend (..))
import           Control.Exception    (try, evaluate, SomeException)
import           Prelude
import           Test.Hspec
import           Test.Hspec.ApproxExpect

spec :: Spec
spec =
  describe "LAPACK spec" $ do
    it "Should have LAPACK available" $ do
      A.isLAPACKAvailable `shouldBe` True

    it "Should perform svd" $ do
      let (s,v,d) = A.svd $ A.matrix @Double (4,2) [ [1,2,3,4], [5,6,7,8] ]
      A.getDims s `shouldBe` (4,4,1,1)
      A.getDims v `shouldBe` (2,1,1,1)
      A.getDims d `shouldBe` (2,2,1,1)

    it "Should perform svd in place" $ do
      let (s,v,d) = A.svdInPlace $ A.matrix @Double (4,2) [ [1,2,3,4], [5,6,7,8] ]
      A.getDims s `shouldBe` (4,4,1,1)
      A.getDims v `shouldBe` (2,1,1,1)
      A.getDims d `shouldBe` (2,2,1,1)

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
      let (re, _im) = A.det $ A.matrix @Double (2,2) [[3,8],[4,6]]
      re `shouldBeApprox` (-14)

    it "Should get determinant of a complex matrix" $ do
      -- M = | 3+i  4+i |   (column-major: col0=[3+i,8+i], col1=[4+i,6+i])
      --     | 8+i  6+i |
      -- det = (3+i)(6+i) - (4+i)(8+i) = -14 - 3i
      let (re, im) = A.det $ A.matrix @(A.Complex Double) (2,2)
                       [[3 A.:+ 1, 8 A.:+ 1], [4 A.:+ 1, 6 A.:+ 1]]
      re `shouldBeApprox` (-14)
      im `shouldBeApprox` (-3)

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

    -- eigSH requires CUDA backend (cuSOLVER); skip gracefully on CPU/OpenCL.
    describe "eigSH (CUDA)" $ do
      let requireCUDA action = do
            backend <- getActiveBackend
            if backend == CUDA then action
            else pendingWith ("eigSH requires CUDA backend; active: " ++ show backend)

      it "returns correct eigenvalues for 2x2 symmetric matrix" $ requireCUDA $ do
        -- A = [[3,1],[1,3]], eigenvalues 2 and 4 (ascending)
        let a          = A.matrix @Double (2,2) [[3,1],[1,3]]
            (evals, _) = A.eigSH a
            evList     = A.toList evals
        length evList `shouldBe` 2
        evList !! 0 `shouldBeApprox` 2.0
        evList !! 1 `shouldBeApprox` 4.0

      it "returns orthonormal eigenvectors for 2x2 matrix" $ requireCUDA $ do
        let a          = A.matrix @Double (2,2) [[3,1],[1,3]]
            (_, evecs) = A.eigSH a
            vtv        = A.toList $ A.matmul (A.transpose evecs False) evecs A.None A.None
            eye2       = A.toList (A.identity @Double [2,2])
        mapM_ (uncurry shouldBeApprox) (zip vtv eye2)

      it "reconstructs the original 2x2 matrix" $ requireCUDA $ do
        let a              = A.matrix @Double (2,2) [[3,1],[1,3]]
            (evals, evecs) = A.eigSH a
            recon          = A.matmul (A.matmul evecs (A.diagCreate evals 0) A.None A.None)
                                      (A.transpose evecs False) A.None A.None
        mapM_ (uncurry shouldBeApprox) (zip (A.toList recon) (A.toList a))

      it "returns eigenvalues in ascending order for 3x3 matrix" $ requireCUDA $ do
        -- A = [[2,1,0],[1,2,1],[0,1,2]], eigenvalues 2-sqrt(2), 2, 2+sqrt(2)
        let a          = A.matrix @Double (3,3) [[2,1,0],[1,2,1],[0,1,2]]
            (evals, _) = A.eigSH a
            evList     = A.toList evals
        evList !! 0 `shouldBeApprox` (2 - sqrt 2)
        evList !! 1 `shouldBeApprox` 2.0
        evList !! 2 `shouldBeApprox` (2 + sqrt 2)

      it "reconstructs the original 3x3 matrix" $ requireCUDA $ do
        let a              = A.matrix @Double (3,3) [[2,1,0],[1,2,1],[0,1,2]]
            (evals, evecs) = A.eigSH a
            recon          = A.matmul (A.matmul evecs (A.diagCreate evals 0) A.None A.None)
                                      (A.transpose evecs False) A.None A.None
        mapM_ (uncurry shouldBeApprox) (zip (A.toList recon) (A.toList a))

      it "handles indefinite matrix with negative eigenvalues" $ requireCUDA $ do
        -- A = [[0,1],[1,0]], eigenvalues -1 and +1
        let a          = A.matrix @Double (2,2) [[0,1],[1,0]]
            (evals, _) = A.eigSH a
            evList     = A.toList evals
        evList !! 0 `shouldBeApprox` (-1.0)
        evList !! 1 `shouldBeApprox`   1.0
