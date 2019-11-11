{-# LANGUAGE ViewPatterns        #-}
--------------------------------------------------------------------------------
-- |
-- Module      : ArrayFire.LAPACK
-- Copyright   : David Johnson (c) 2019-2020
-- License     : BSD 3
-- Maintainer  : David Johnson <djohnson.m@gmail.com>
-- Stability   : Experimental
-- Portability : GHC
--
-- LAPACK — Linear Algebra PACKage
--
-- @
-- >>> (u,e,d) = svd (constant @Double [3,3] 10)
-- >>> u
-- ArrayFire Array
-- [3 3 1 1]
--    -0.5774     0.8165    -0.0000
--    -0.5774    -0.4082    -0.7071
--    -0.5774    -0.4082     0.7071
--
-- >>> e
-- ArrayFire Array
-- [3 1 1 1]
--    30.0000
--     0.0000
--     0.0000
--
-- >>> d
-- ArrayFire Array
-- [3 3 1 1]
--   -0.5774    -0.5774    -0.5774
--   -0.8165     0.4082     0.4082
--   -0.0000     0.7071    -0.7071
--
-- @
--------------------------------------------------------------------------------
module ArrayFire.LAPACK where

import ArrayFire.Internal.LAPACK
import ArrayFire.FFI
import ArrayFire.Types
import ArrayFire.Internal.Types

-- | Singular Value Decomposition
--
-- [ArrayFire Docs](http://arrayfire.org/docs/group__lapack__factor__func__svd.htm)
--
-- The arrayfire function only returns the non zero diagonal elements of S.
--
svd
  :: AFType a
  => Array a
  -- ^ the input Matrix
  -> (Array a, Array a, Array a)
  -- ^ Output 'Array' containing (U, diagonal values of sigma, V^H)
svd = (`op3p` af_svd)

-- | Singular Value Decomposition (in-place)
--
-- [ArrayFire Docs](http://arrayfire.org/docs/group__lapack__factor__func__svd.htm)
--
-- The arrayfire function only returns the non zero diagonal elements of S.
--
svdInPlace
  :: AFType a
  => Array a
  -- ^ the input matrix
  -> (Array a, Array a, Array a)
  -- ^ Output 'Array' containing (U, diagonal values of sigma, V^H)
svdInPlace = (`op3p` af_svd_inplace)

-- | Perform LU decomposition
--
-- [ArrayFire Docs](http://arrayfire.org/docs/group__lapack__factor__func__lu.htm)
--
-- C Interface for LU decomposition.
--
lu
  :: AFType a
  => Array a
  -- ^ is the input matrix
  -> (Array a, Array a, Array a)
  -- ^ Returns the output 'Array's (lower, upper, pivot)
lu = (`op3p` af_lu)

-- | Perform LU decomposition (in-place).
--
-- [ArrayFire Docs](http://arrayfire.org/docs/group__lapack__factor__func__lu.htm#ga0adcdc4b189c34644a7153c6ce9c4f7f)
--
-- C Interface for in place LU decomposition.
--
luInPlace
  :: AFType a
  => Array a
  -- ^ contains the input on entry, the packed LU decomposition on exit.
  -> Bool
  -- ^ specifies if the pivot is returned in original LAPACK compliant format
  -> Array a
  -- ^ will contain the permutation indices to map the input to the decomposition
luInPlace a (fromIntegral . fromEnum -> b) = a `op1` (\x y -> af_lu_inplace x y b)

-- | Perform QR decomposition
--
-- [ArrayFire Docs](http://arrayfire.org/docs/group__lapack__factor__func__qr.htm)
--
-- C Interface for QR decomposition.
--
qr
  :: AFType a
  => Array a
  -- ^ the input matrix
  -> (Array a, Array a, Array a)
  -- ^ Returns (q, r, tau) 'Array's
  -- /q/ is the orthogonal matrix from QR decomposition
  -- /r/ is the upper triangular matrix from QR decomposition
  -- /tau/ will contain additional information needed for solving a least squares problem using /q/ and /r/
qr = (`op3p` af_qr)

-- | Perform QR decomposition
--
-- [ArrayFire Docs](http://arrayfire.org/docs/group__lapack__factor__func__qr.htm)
--
-- C Interface for QR decomposition.
--
qrInPlace
  :: AFType a
  => Array a
  -- ^ is the input matrix on entry. It contains packed QR decomposition on exit
  -> Array a
  -- ^ will contain additional information needed for unpacking the data
qrInPlace = (`op1` af_qr_inplace)

-- | Perform Cholesky Decomposition
--
-- [ArrayFire Docs](http://arrayfire.org/docs/group__lapack__factor__func__cholesky.htm)
--
-- This function decomposes a positive definite matrix A into two triangular matrices.
--
cholesky
  :: AFType a
  => Array a
  -- ^ input 'Array'
  -> Bool
  -- ^ a boolean determining if out is upper or lower triangular
  -> (Int, Array a)
  -- ^ contains the triangular matrix. Multiply 'Int' with its conjugate transpose reproduces the input array.
  -- is 0 if cholesky decomposition passes, if not it returns the rank at which the decomposition failed.
cholesky a (fromIntegral . fromEnum -> b) = do
  let (x',y') = op1b a (\x y z -> af_cholesky x y z b)
  (fromIntegral x', y')

-- | Perform Cholesky Decomposition
--
-- [ArrayFire Docs](http://arrayfire.org/docs/group__lapack__factor__func__cholesky.htm)
--
-- C Interface for in place cholesky decomposition.
--
choleskyInplace
  :: AFType a
  => Array a
  -- ^ is the input matrix on entry. It contains the triangular matrix on exit.
  -> Bool
  -- ^ a boolean determining if in is upper or lower triangular
  -> Int
  -- ^ is 0 if cholesky decomposition passes, if not it returns the rank at which the decomposition failed.
choleskyInplace a (fromIntegral . fromEnum -> b) =
  fromIntegral $ infoFromArray a (\x y -> af_cholesky_inplace x y b)

-- | Solve a system of equations
--
-- [ArrayFire Docs](http://arrayfire.org/docs/group__lapack__solve__func__gen.htm)
--
solve
  :: AFType a
  => Array a
  -- ^ is the coefficient matrix
  -> Array a
  -- ^ is the measured values
  -> MatProp
  -- ^ determining various properties of matrix a
  -> Array a
  -- ^ is the matrix of unknown variables
solve a b m =
  op2 a b (\x y z -> af_solve x y z (toMatProp m))

-- | Solve a system of equations.
--
-- [ArrayFire Docs](http://arrayfire.org/docs/group__lapack__solve__lu__func__gen.htm)
--
solveLU
  :: AFType a
  => Array a
  -- ^ is the output matrix from packed LU decomposition of the coefficient matrix
  -> Array a
  -- ^ is the pivot array from packed LU decomposition of the coefficient matrix
  -> Array a
  -- ^ is the matrix of measured values
  -> MatProp
  -- ^ determining various properties of matrix a
  -> Array a
  -- ^ will contain the matrix of unknown variables
solveLU a b c m =
  op3 a b c (\x y z w -> af_solve_lu x y z w (toMatProp m))

-- | Invert a matrix.
--
-- [ArrayFire Docs](http://arrayfire.org/docs/group__lapack__ops__func__inv.htm)
--
-- C Interface for inverting a matrix.
--
inverse
  :: AFType a
  => Array a
  -- ^ is input matrix
  -> MatProp
  -- ^ determining various properties of matrix in
  -> Array a
  -- ^ will contain the inverse of matrix in
inverse a m =
  a `op1` (\x y  -> af_inverse x y (toMatProp m))

-- | Pseudo-inverse
--
-- Not implemented in /3.6.4/
--
-- [ArrayFire Docs](http://arrayfire.org/docs/group__lapack__factor__func__p_inv.htm)
--
-- pinverse
--   :: AFType a
--   => Array a
--   -> Double
--   -> MatProp
--   -> Array a
-- pinverse a d m =
--   op1 a (\x y  -> af_pinverse x y d (toMatProp m))

-- | Find the rank of the input matrix
--
-- [ArrayFire Docs](http://arrayfire.org/docs/group__lapack__factor__func__rank.htm)
--
-- This function uses af::qr to find the rank of the input matrix within the given tolerance.
--
rank
  :: AFType a
  => Array a
  -- ^ is input matrix
  -> Double
  -- ^ is the tolerance value
  -> Int
  -- ^ will contain the rank of in
rank a b =
  fromIntegral (a `infoFromArray` (\x y -> af_rank x y b))

-- | Find the determinant of a Matrix
--
-- [ArrayFire Docs](http://arrayfire.org/docs/group__lapack__ops__func__det.htm)
--
-- C Interface for finding the determinant of a matrix.
--
det
  :: AFType a
  => Array a
  -- ^ is input matrix
  -> (Double,Double)
  -- ^ will contain the real and imaginary part of the determinant of in
det = (`infoFromArray2` af_det)

-- | Find the norm of the input matrix.
--
-- [ArrayFire Docs](http://arrayfire.org/docs/group__lapack__ops__func__norm.htm)
--
-- This function can return the norm using various metrics based on the type paramter.
--
norm
  :: AFType a
  => Array a
  -- ^ is the input matrix
  -> NormType
  -- ^ specifies the 'NormType'
  -> Double
  -- ^ specifies the value of P when type is one of AF_NORM_VECTOR_P, AF_NORM_MATRIX_L_PQ is used. It is ignored for other values of type
  -> Double
  -- ^ specifies the value of Q when type is AF_NORM_MATRIX_L_PQ. This parameter is ignored if type is anything else
  -> Double
  -- ^ will contain the norm of in
norm arr (fromNormType -> a) b c =
  arr `infoFromArray` (\w y -> af_norm w y a b c)

-- | Is LAPACK available
--
-- [ArrayFire Docs](http://arrayfire.org/docs/group__lapack__helper__func__available.htm)
--
isLAPACKAvailable
  :: Bool
  -- ^ Returns if LAPACK is available
isLAPACKAvailable =
  toEnum . fromIntegral $ afCall1' af_is_lapack_available
