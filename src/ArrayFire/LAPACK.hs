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
-- LAPACK API
--
-- @
-- module Main where
--
-- import ArrayFire
--
-- main :: IO ()
-- main = print =<< getAvailableBackends
-- @
--
-- @
-- [nix-shell:~\/arrayfire]$ .\/main
-- [CPU,OpenCL]
-- @
--------------------------------------------------------------------------------
module ArrayFire.LAPACK where

import ArrayFire.Internal.LAPACK
import ArrayFire.FFI
import ArrayFire.Types
import ArrayFire.Internal.Types

-- | This function factorizes a matrix A into two unitary matrices U and Vt, and a diagonal matrix S such that
-- A=U∗S∗Vt
--
-- If A has M rows and N columns, U is of the size M x M , V is of size N x N, and S is of size M x N
--
-- The arrayfire function only returns the non zero diagonal elements of S.
--
svd
  :: AFType a
  => Array a
  -- ^ Input matrix
  -> (Array a, Array a, Array a)
  -- ^ 'u' is the output array containing U
  --
  -- 'v' is the output array containing the diagonal values of sigma, (singular values of the input matrix))
  --
  -- 'vt' is the output array containing V^H
svd = (`op3p` af_svd)

-- | Calculates 'mean' of 'Array' along user-specified dimension.
--
-- @
-- >>> print $ mean 0 ( vector @Int 10 [1..] )
-- @
-- @
-- ArrayFire Array
--   [1 1 1 1]
--      5.5000
-- @
svdInPlace
  :: AFType a
  => Array a
  -> (Array a, Array a, Array a)
svdInPlace = (`op3p` af_svd_inplace)

-- | Calculates 'mean' of 'Array' along user-specified dimension.
--
-- @
-- >>> print $ mean 0 ( vector @Int 10 [1..] )
-- @
-- @
-- ArrayFire Array
--   [1 1 1 1]
--      5.5000
-- @
lu
  :: AFType a
  => Array a
  -> (Array a, Array a, Array a)
lu = (`op3p` af_lu)

-- | Calculates 'mean' of 'Array' along user-specified dimension.
--
-- @
-- >>> print $ mean 0 ( vector @Int 10 [1..] )
-- @
-- @
-- ArrayFire Array
--   [1 1 1 1]
--      5.5000
-- @
luInPlace
  :: AFType a
  => Array a
  -> Bool
  -> Array a
luInPlace a (fromIntegral . fromEnum -> b) = a `op1` (\x y -> af_lu_inplace x y b)

-- | Calculates 'mean' of 'Array' along user-specified dimension.
--
-- @
-- >>> print $ mean 0 ( vector @Int 10 [1..] )
-- @
-- @
-- ArrayFire Array
--   [1 1 1 1]
--      5.5000
-- @
qr
  :: AFType a
  => Array a
  -> (Array a, Array a, Array a)
qr = (`op3p` af_qr)

-- | Calculates 'mean' of 'Array' along user-specified dimension.
--
-- @
-- >>> print $ mean 0 ( vector @Int 10 [1..] )
-- @
-- @
-- ArrayFire Array
--   [1 1 1 1]
--      5.5000
-- @
qrInPlace
  :: AFType a
  => Array a
  -> Array a
qrInPlace = (`op1` af_qr_inplace)

-- | Calculates 'mean' of 'Array' along user-specified dimension.
--
-- @
-- >>> print $ mean 0 ( vector @Int 10 [1..] )
-- @
-- @
-- ArrayFire Array
--   [1 1 1 1]
--      5.5000
-- @
cholesky
  :: AFType a
  => Array a
  -> Bool
  -> (Int, Array a)
cholesky a (fromIntegral . fromEnum -> b) = do
  let (x',y') = op1b a (\x y z -> af_cholesky x y z b)
  (fromIntegral x', y')

-- | Calculates 'mean' of 'Array' along user-specified dimension.
--
-- @
-- >>> print $ mean 0 ( vector @Int 10 [1..] )
-- @
-- @
-- ArrayFire Array
--   [1 1 1 1]
--      5.5000
-- @
choleskyInplace
  :: AFType a
  => Array a
  -> Bool
  -> Int
choleskyInplace a (fromIntegral . fromEnum -> b) =
  fromIntegral $ infoFromArray a (\x y -> af_cholesky_inplace x y b)

-- | Calculates 'mean' of 'Array' along user-specified dimension.
--
-- @
-- >>> print $ mean 0 ( vector @Int 10 [1..] )
-- @
-- @
-- ArrayFire Array
--   [1 1 1 1]
--      5.5000
-- @
solve
  :: AFType a
  => Array a
  -> Array a
  -> MatProp
  -> Array a
solve a b m =
  op2 a b (\x y z -> af_solve x y z (toMatProp m))

-- | Calculates 'mean' of 'Array' along user-specified dimension.
--
-- @
-- >>> print $ mean 0 ( vector @Int 10 [1..] )
-- @
-- @
-- ArrayFire Array
--   [1 1 1 1]
--      5.5000
-- @
solveLU
  :: AFType a
  => Array a
  -> Array a
  -> Array a
  -> MatProp
  -> Array a
solveLU a b c m =
  op3 a b c (\x y z w -> af_solve_lu x y z w (toMatProp m))

-- | Calculates 'mean' of 'Array' along user-specified dimension.
--
-- @
-- >>> print $ mean 0 ( vector @Int 10 [1..] )
-- @
-- @
-- ArrayFire Array
--   [1 1 1 1]
--      5.5000
-- @
inverse
  :: AFType a
  => Array a
  -> MatProp
  -> Array a
inverse a m =
  a `op1` (\x y  -> af_inverse x y (toMatProp m))

-- | Not implemented in 3.6.4
-- | Calculates 'mean' of 'Array' along user-specified dimension.
--
-- @
-- >>> print $ mean 0 ( vector @Int 10 [1..] )
-- @
-- @
-- ArrayFire Array
--   [1 1 1 1]
--      5.5000
-- @
pinverse
  :: AFType a
  => Array a
  -> Double
  -> MatProp
  -> Array a
pinverse a d m =
  a `op1` (\x y  -> af_pinverse x y d (toMatProp m))

-- | Calculates 'mean' of 'Array' along user-specified dimension.
--
-- @
-- >>> print $ mean 0 ( vector @Int 10 [1..] )
-- @
-- @
-- ArrayFire Array
--   [1 1 1 1]
--      5.5000
-- @
rank
  :: AFType a
  => Array a
  -> Double
  -> Int
rank a b =
  fromIntegral (a `infoFromArray` (\x y -> af_rank x y b))

-- | Calculates 'mean' of 'Array' along user-specified dimension.
--
-- @
-- >>> print $ mean 0 ( vector @Int 10 [1..] )
-- @
-- @
-- ArrayFire Array
--   [1 1 1 1]
--      5.5000
-- @
det
  :: AFType a
  => Array a
  -> (Double,Double)
det = (`infoFromArray2` af_det)

-- | Calculates 'mean' of 'Array' along user-specified dimension.
--
-- @
-- >>> print $ mean 0 ( vector @Int 10 [1..] )
-- @
-- @
-- ArrayFire Array
--   [1 1 1 1]
--      5.5000
-- @
norm
  :: AFType a
  => Array a
  -> NormType
  -> Double
  -> Double
  -> Double
norm arr (fromNormType -> a) b c =
  arr `infoFromArray` (\w y -> af_norm w y a b c)

-- | Calculates 'mean' of 'Array' along user-specified dimension.
--
-- @
-- >>> print $ mean 0 ( vector @Int 10 [1..] )
-- @
-- @
-- ArrayFire Array
--   [1 1 1 1]
--      5.5000
-- @
isLAPACKAvailable :: Bool
isLAPACKAvailable =
  toEnum . fromIntegral $ afCall1' af_is_lapack_available
