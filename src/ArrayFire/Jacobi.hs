{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
--------------------------------------------------------------------------------
-- |
-- Module      : ArrayFire.Jacobi
-- Copyright   : David Johnson (c) 2019-2026
-- License     : BSD 3
-- Maintainer  : David Johnson <code@dmj.io>
-- Stability   : Experimental
-- Portability : GHC
--
-- Jacobi eigenvalue algorithm for real symmetric matrices.
--
-- @
-- >>> let a = matrix \@Double (3,3) [[4,2,2],[2,3,0],[2,0,3]]
-- >>> let (evals, evecs) = jacobi a 100 1e-10
-- @
--
--------------------------------------------------------------------------------
module ArrayFire.Jacobi
  ( jacobi
  ) where
--------------------------------------------------------------------------------
import Foreign.Storable         (Storable)
--------------------------------------------------------------------------------
import ArrayFire.Algorithm      (sumAll)
import ArrayFire.Arith          (add, sub, mul)
import ArrayFire.Array          (getDims, getScalar, scalar)
import ArrayFire.Data           (diagCreate, diagExtract, identity)
import ArrayFire.Index          (index, assignSeq)
import ArrayFire.Internal.Types (Array, AFType)
import ArrayFire.Types          (Seq (..))
--------------------------------------------------------------------------------
-- | Jacobi eigenvalue decomposition for real symmetric n×n matrices.
--
-- Uses the cyclic Jacobi method: each sweep applies a Givens rotation to
-- every off-diagonal pair (p,q) with p < q. Repeats until the off-diagonal
-- Frobenius norm is below @tol@ or @maxSweeps@ is reached.
--
-- Returns @(eigenvalues, eigenvectors)@:
--
--   * @eigenvalues@ — length-n vector; element @i@ is the eigenvalue
--     associated with column @i@ of @eigenvectors@.
--   * @eigenvectors@ — n×n orthogonal matrix; columns are eigenvectors.
--
-- All intermediate computation stays on the GPU. The only CPU round-trips
-- are the three scalar reads per pair (apq, app, aqq) needed to compute the
-- rotation angle, and one scalar read per sweep for the convergence check.
--
-- The eigenvalues are /not/ sorted; use 'ArrayFire.Algorithm.sort' if needed.
jacobi
  :: forall a . (AFType a, RealFloat a, Storable a)
  => Array a   -- ^ real symmetric n×n matrix
  -> Int       -- ^ maximum number of sweeps
  -> a         -- ^ convergence tolerance (off-diagonal Frobenius norm)
  -> (Array a, Array a)
  -- ^ (eigenvalues vector, eigenvectors matrix)
jacobi mat maxSweeps tol = go 0 mat (identity @a [n, n])
  where
    (n, _, _, _) = getDims mat
    pairs        = [(p, q) | p <- [0 .. n - 2], q <- [p + 1 .. n - 1]]

    go sweepNum a v
      | sweepNum >= maxSweeps = finish a v
      | offDiagNorm a < tol   = finish a v
      | otherwise =
          let (a', v') = foldl' (applyPair n tol) (a, v) pairs
          in go (sweepNum + 1) a' v'

    finish a v = (diagExtract a 0, v)
--------------------------------------------------------------------------------
-- | Apply a single Givens rotation at position (p,q) entirely on the GPU.
--
-- Instead of building an n×n rotation matrix and doing two O(n³) matmuls,
-- this updates only the two affected rows of A (left multiply by Gᵀ) and
-- the two affected columns of A and V (right multiply by G). Each update is
-- O(n) and never materialises the full rotation matrix on either CPU or GPU.
--
-- The only GPU→CPU transfers are the three element reads for apq, app, aqq.
-- c and s are uploaded as [1]-element GPU scalars and broadcast across the
-- column/row vectors.
applyPair
  :: forall a . (AFType a, RealFloat a, Storable a)
  => Int -> a -> (Array a, Array a) -> (Int, Int) -> (Array a, Array a)
applyPair n tol (a, v) (p, q) =
  let apq = getElem a p q
  in if abs apq < tol
     then (a, v)
     else
       let app = getElem a p p
           aqq = getElem a q q
           tau = (aqq - app) / (2 * apq)
           t   | tau >= 0  = 1 / (tau + sqrt (1 + tau * tau))
               | otherwise = (-1) / (negate tau + sqrt (1 + tau * tau))
           c   = 1 / sqrt (1 + t * t)
           s   = t * c
           -- GPU scalar arrays; broadcast over column/row vectors via af_mul batch=1
           sc  = scalar c
           ss  = scalar s
           -- Left-multiply A by Gᵀ: update rows p and q
           rowAp   = getRow a p
           rowAq   = getRow a q
           rowAp'  = (sc `mul` rowAp) `sub` (ss `mul` rowAq)
           rowAq'  = (ss `mul` rowAp) `add` (sc `mul` rowAq)
           a1      = setRow (setRow a p rowAp') q rowAq'
           -- Right-multiply A by G: update cols p and q of the row-updated A
           colA1p  = getCol a1 p
           colA1q  = getCol a1 q
           colAp'  = (sc `mul` colA1p) `sub` (ss `mul` colA1q)
           colAq'  = (ss `mul` colA1p) `add` (sc `mul` colA1q)
           a2      = setCol (setCol a1 p colAp') q colAq'
           -- Right-multiply V by G: update cols p and q
           vp      = getCol v p
           vq      = getCol v q
           vp'     = (sc `mul` vp) `sub` (ss `mul` vq)
           vq'     = (ss `mul` vp) `add` (sc `mul` vq)
           v'      = setCol (setCol v p vp') q vq'
       in (a2, v')
  where
    n1           = fromIntegral (n - 1) :: Double
    getRow arr i = index arr    [Seq (fromIntegral i) (fromIntegral i) 1, Seq 0 n1 1]
    getCol arr j = index arr    [Seq 0 n1 1, Seq (fromIntegral j) (fromIntegral j) 1]
    setRow arr i = assignSeq arr [Seq (fromIntegral i) (fromIntegral i) 1, Seq 0 n1 1]
    setCol arr j = assignSeq arr [Seq 0 n1 1, Seq (fromIntegral j) (fromIntegral j) 1]
--------------------------------------------------------------------------------
-- Extract the scalar at row i, column j (GPU→CPU; called 3× per active pair).
getElem :: (AFType a, Storable a) => Array a -> Int -> Int -> a
getElem arr i j =
  getScalar $
    index arr [ Seq (fromIntegral i) (fromIntegral i) 1
              , Seq (fromIntegral j) (fromIntegral j) 1 ]
--------------------------------------------------------------------------------
-- Frobenius norm of the strict off-diagonal part of A (one GPU→CPU per sweep).
offDiagNorm :: forall a . (AFType a, RealFloat a) => Array a -> a
offDiagNorm a =
  let d       = diagCreate (diagExtract a 0) 0
      offDiag = a `sub` d
      sq      = offDiag `mul` offDiag
  in sqrt . realToFrac . fst $ sumAll sq
--------------------------------------------------------------------------------
