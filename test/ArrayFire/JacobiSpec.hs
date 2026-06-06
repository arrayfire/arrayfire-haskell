{-# LANGUAGE TypeApplications #-}
module ArrayFire.JacobiSpec where

import qualified ArrayFire            as A
import           ArrayFire.Jacobi     (jacobi)
import           Prelude
import           Test.Hspec
import           Test.Hspec.ApproxExpect

spec :: Spec
spec =
  describe "Jacobi eigenvalue algorithm" $ do

    it "decomposes a 2x2 symmetric matrix" $ do
      -- A = [[3,1],[1,3]], eigenvalues 2 and 4
      let a          = A.matrix @Double (2, 2) [[3, 1], [1, 3]]
          (evals, _) = jacobi a 100 1e-12
          evList     = A.toList evals
      -- eigenvalues can come out in any order; check the sorted set
      minimum evList `shouldBeApprox` 2.0
      maximum evList `shouldBeApprox` 4.0

    it "produces orthonormal eigenvectors for a 2x2 matrix" $ do
      let a          = A.matrix @Double (2, 2) [[3, 1], [1, 3]]
          (_, evecs) = jacobi a 100 1e-12
          -- V^T * V should be identity
          vtv        = A.toList $ A.matmul (A.transpose evecs False) evecs A.None A.None
          eye2       = A.toList (A.identity @Double [2, 2])
      mapM_ (uncurry shouldBeApprox) (zip vtv eye2)

    it "reconstructs the original 2x2 matrix" $ do
      let a              = A.matrix @Double (2, 2) [[3, 1], [1, 3]]
          (evals, evecs) = jacobi a 100 1e-12
          -- A ≈ V * diag(λ) * V^T
          diag           = A.diagCreate evals 0
          recon          = A.matmul (A.matmul evecs diag A.None A.None)
                                    (A.transpose evecs False) A.None A.None
          reconList      = A.toList recon
          aList          = A.toList a
      mapM_ (uncurry shouldBeApprox) (zip reconList aList)

    it "decomposes a 3x3 symmetric matrix" $ do
      -- A = [[2,1,0],[1,2,1],[0,1,2]], eigenvalues 2-sqrt(2), 2, 2+sqrt(2)
      let a          = A.matrix @Double (3, 3) [[2, 1, 0], [1, 2, 1], [0, 1, 2]]
          (evals, _) = jacobi a 200 1e-12
          evList     = A.toList evals
      minimum evList `shouldBeApprox` (2 - sqrt 2)
      maximum evList `shouldBeApprox` (2 + sqrt 2)
      -- middle eigenvalue
      let mid = filter (\x -> x > 1.5 && x < 2.5) evList
      length mid `shouldBe` 1
      case mid of
        [v] -> v `shouldBeApprox` 2.0
        _   -> fail "expected exactly one middle eigenvalue"

    it "reconstructs the original 3x3 matrix" $ do
      let a              = A.matrix @Double (3, 3) [[2, 1, 0], [1, 2, 1], [0, 1, 2]]
          (evals, evecs) = jacobi a 200 1e-12
          diag           = A.diagCreate evals 0
          recon          = A.matmul (A.matmul evecs diag A.None A.None)
                                    (A.transpose evecs False) A.None A.None
          reconList      = A.toList recon
          aList          = A.toList a
      mapM_ (uncurry shouldBeApprox) (zip reconList aList)

    it "handles a diagonal matrix (already converged)" $ do
      let a          = A.matrix @Double (3, 3) [[1, 0, 0], [0, 2, 0], [0, 0, 3]]
          (evals, _) = jacobi a 10 1e-12
          evList     = A.toList evals
      minimum evList `shouldBeApprox` 1.0
      maximum evList `shouldBeApprox` 3.0

    -- 4×4 tridiagonal: A[i,i]=4, A[i,i±1]=1
    it "produces orthonormal eigenvectors for a 4x4 matrix" $ do
      let a          = A.matrix @Double (4, 4)
                         [ [4, 1, 0, 0]
                         , [1, 4, 1, 0]
                         , [0, 1, 4, 1]
                         , [0, 0, 1, 4] ]
          (_, evecs) = jacobi a 500 1e-12
          vtv        = A.toList $ A.matmul (A.transpose evecs False) evecs A.None A.None
          eye4       = A.toList (A.identity @Double [4, 4])
      mapM_ (uncurry shouldBeApprox) (zip vtv eye4)

    it "reconstructs the original 4x4 matrix" $ do
      let a              = A.matrix @Double (4, 4)
                             [ [4, 1, 0, 0]
                             , [1, 4, 1, 0]
                             , [0, 1, 4, 1]
                             , [0, 0, 1, 4] ]
          (evals, evecs) = jacobi a 500 1e-12
          diag           = A.diagCreate evals 0
          recon          = A.matmul (A.matmul evecs diag A.None A.None)
                                    (A.transpose evecs False) A.None A.None
          reconList      = A.toList recon
          aList          = A.toList a
      mapM_ (uncurry shouldBeApprox) (zip reconList aList)

    -- 5×5 Hilbert-like: A[i,j] = 1/(i+j+1), symmetric, positive-definite
    it "produces orthonormal eigenvectors for a 5x5 matrix" $ do
      let a          = A.matrix @Double (5, 5)
                         [ [h 0 0, h 0 1, h 0 2, h 0 3, h 0 4]
                         , [h 1 0, h 1 1, h 1 2, h 1 3, h 1 4]
                         , [h 2 0, h 2 1, h 2 2, h 2 3, h 2 4]
                         , [h 3 0, h 3 1, h 3 2, h 3 3, h 3 4]
                         , [h 4 0, h 4 1, h 4 2, h 4 3, h 4 4] ]
          h i j      = 1.0 / fromIntegral (i + j + 1 :: Int)
          (_, evecs) = jacobi a 1000 1e-10
          vtv        = A.toList $ A.matmul (A.transpose evecs False) evecs A.None A.None
          eye5       = A.toList (A.identity @Double [5, 5])
      mapM_ (uncurry shouldBeApprox) (zip vtv eye5)

    it "reconstructs the original 5x5 matrix" $ do
      let h i j      = 1.0 / fromIntegral (i + j + 1 :: Int)
          a              = A.matrix @Double (5, 5)
                             [ [h 0 0, h 0 1, h 0 2, h 0 3, h 0 4]
                             , [h 1 0, h 1 1, h 1 2, h 1 3, h 1 4]
                             , [h 2 0, h 2 1, h 2 2, h 2 3, h 2 4]
                             , [h 3 0, h 3 1, h 3 2, h 3 3, h 3 4]
                             , [h 4 0, h 4 1, h 4 2, h 4 3, h 4 4] ]
          (evals, evecs) = jacobi a 1000 1e-10
          diag           = A.diagCreate evals 0
          recon          = A.matmul (A.matmul evecs diag A.None A.None)
                                    (A.transpose evecs False) A.None A.None
          reconList      = A.toList recon
          aList          = A.toList a
      mapM_ (uncurry shouldBeApprox) (zip reconList aList)
