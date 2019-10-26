{-# LANGUAGE ViewPatterns        #-}
--------------------------------------------------------------------------------
-- |
-- Module      : ArrayFire.BLAS
-- Copyright   : David Johnson (c) 2019-2020
-- License     : BSD3
-- Maintainer  : David Johnson <djohnson.m@gmail.com>
-- Stability   : Experimental
-- Portability : GHC
--
-- Basic Linear Algebra Subprograms (BLAS) API
--
-- @
-- main :: IO ()
-- main = 'print' ('matmul' x y xProp yProp)
--  where
--     x,y :: 'Array' 'Double'
--     x = 'matrix' (2,3) [1..]
--     y = 'matrix' (3,2) [1..]
--
--     xProp, yProp :: 'MatProp'
--     xProp = None
--     yProp = None
-- @
-- @
-- ArrayFire Array
-- [2 2 1 1]
--   22.0000    28.0000
--   49.0000    64.0000
-- @
--------------------------------------------------------------------------------
module ArrayFire.BLAS where

import Data.Complex

import ArrayFire.FFI
import ArrayFire.Internal.BLAS
import ArrayFire.Internal.Types

-- | The following applies for Sparse-Dense matrix multiplication.
--
-- This function can be used with one sparse input. The sparse input must always be the lhs and the dense matrix must be rhs.
--
-- The sparse array can only be of 'CSR' format.
--
-- The returned array is always dense.
--
-- optLhs an only be one of AF_MAT_NONE, AF_MAT_TRANS, AF_MAT_CTRANS.
--
-- optRhs can only be AF_MAT_NONE.
--
-- >>> matmul (matrix @Double (2,2) [[1,2],[3,4]]) (matrix @Double (2,2) [[1,2],[3,4]]) None None
-- ArrayFire Array
-- [2 2 1 1]
--    7.0000    10.0000
--   15.0000    22.0000
matmul
  :: Array a
  -- ^ 2D matrix of Array a, left-hand side
  -> Array a
  -- ^ 2D matrix of Array a, right-hand side
  -> MatProp
  -- ^ Left hand side matrix options
  -> MatProp
  -- ^ Right hand side matrix options
  -> Array a
  -- ^ Output of 'matmul'
matmul arr1 arr2 prop1 prop2 = do
  op2 arr1 arr2 (\p a b -> af_matmul p a b (toMatProp prop1) (toMatProp prop2))

-- | Scalar dot product between two vectors. Also referred to as the inner product.
--
-- >>> dot (vector @Double 10 [1..]) (vector @Double 10 [1..]) None None
-- ArrayFire Array
-- [1 1 1 1]
--   385.0000
dot
  :: Array a
  -- ^ Left-hand side input
  -> Array a
  -- ^ Right-hand side input
  -> MatProp
  -- ^ Options for left-hand side. Currently only 'AF_MAT_NONE' and 'AF_MAT_CONJ' are supported.
  -> MatProp
  -- ^ Options for right-hand side. Currently only 'AF_MAT_NONE' and 'AF_MAT_CONJ' are supported.
  -> Array a
  -- ^ Output of 'dot'
dot arr1 arr2 prop1 prop2 =
  op2 arr1 arr2 (\p a b -> af_dot p a b (toMatProp prop1) (toMatProp prop2))

-- | Scalar dot product between two vectors. Also referred to as the inner product. Returns the result as a host scalar.
--
-- >>> dotAll (vector @Double 10 [1..]) (vector @Double 10 [1..]) None None
-- 385.0 :+ 0.0
dotAll
  :: Array a
  -- ^ Left-hand side array
  -> Array a
  -- ^ Right-hand side array
  -> MatProp
  -- ^ Options for left-hand side. Currently only AF_MAT_NONE and AF_MAT_CONJ are supported.
  -> MatProp
  -- ^ Options for right-hand side. Currently only AF_MAT_NONE and AF_MAT_CONJ are supported.
  -> Complex Double
  -- ^ Real and imaginary component result
dotAll arr1 arr2 prop1 prop2 = do
  let (real,imag) =
        infoFromArray22 arr1 arr2 $ \a b c d ->
          af_dot_all a b c d (toMatProp prop1) (toMatProp prop2)
  real :+ imag

-- | Transposes a matrix.
--
-- >>> matrix @Double (2,3) [[2,3,4],[4,5,6]]
-- ArrayFire Array
-- [2 3 1 1]
--    2.0000     3.0000
--    4.0000     4.0000
--    5.0000     6.0000
--
-- >>> transpose (matrix @Double (2,3) [[2,3,4],[4,5,6]]) True
-- ArrayFire Array
-- [3 2 1 1]
--    2.0000     4.0000     5.0000
--    3.0000     4.0000     6.0000
--
transpose
  :: Array a
  -- ^ Input matrix to be transposed
  -> Bool
  -- ^ Should perform conjugate transposition
  -> Array a
  -- ^ The transposed matrix
transpose arr1 (fromIntegral . fromEnum -> b) =
  arr1 `op1` (\x y -> af_transpose x y b)

-- | Transposes a matrix.
--
-- * Warning: This function mutates an array in-place, all subsequent references will be changed. Use carefully.
--
-- >>> array = matrix @Double (2,2) [[1..2],[3..4]]
-- >>> transposeInPlace array False
-- ()
--
transposeInPlace
  :: Array a
  -- ^ Input matrix to be transposed
  -> Bool
  -- ^ Should perform conjugate transposition
  -> IO ()
transposeInPlace arr (fromIntegral . fromEnum -> b) =
  arr `inPlace` (`af_transpose_inplace` b)
