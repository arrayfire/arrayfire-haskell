module ArrayFire.LAPACK where

import Foreign.Marshal
import Foreign.Storable
import Foreign.C.String

import ArrayFire.Internal.LAPACK
import ArrayFire.Exception
import ArrayFire.FFI
import ArrayFire.Types
import ArrayFire.Internal.Defines

svd
  :: AFType a
  => Array a
  -> (Array a, Array a, Array a)
svd = (`op3p` af_svd)

svdInPlace
  :: AFType a
  => Array a
  -> (Array a, Array a, Array a)
svdInPlace = (`op3p` af_svd_inplace)

lu
  :: AFType a
  => Array a
  -> (Array a, Array a, Array a)
lu = (`op3p` af_lu)

luInPlace
  :: AFType a
  => Array a
  -> Bool
  -> Array a
luInPlace a b = a `op1` (\x y -> af_lu_inplace x y b)

qr
  :: AFType a
  => Array a
  -> (Array a, Array a, Array a)
qr = (`op3p` af_qr)

qrInPlace
  :: AFType a
  => Array a
  -> Array a
qrInPlace = (`op1` af_qr_inplace)

cholesky
  :: AFType a
  => Array a
  -> Bool
  -> (Int, Array a)
cholesky a b =
  op1b a (\x y z -> af_cholesky x y z b)

choleskyInplace
  :: AFType a
  => Array a
  -> Bool
  -> Int
choleskyInplace a b =
  infoFromArray a (\x y -> af_cholesky_inplace x y b)

solve
  :: AFType a
  => Array a
  -> Array a
  -> MatProp
  -> Array a
solve a b m =
  op2 a b (\x y z -> af_solve x y z (toMatProp m))

solveLU
  :: AFType a
  => Array a
  -> Array a
  -> Array a
  -> MatProp
  -> Array a
solveLU a b c m =
  op3 a b c (\x y z w -> af_solve_lu x y z w (toMatProp m))

inverse
  :: AFType a
  => Array a
  -> MatProp
  -> Array a
inverse a m =
  a `op1` (\x y  -> af_inverse x y (toMatProp m))

pinverse
  :: AFType a
  => Array a
  -> Double
  -> MatProp
  -> Array a
pinverse a d m =
  a `op1` (\x y  -> af_pinverse x y d (toMatProp m))

rank
  :: AFType a
  => Array a
  -> Double
  -> Int
rank a b =
  fromIntegral (a `infoFromArray` (\x y -> af_rank x y b))

det
  :: AFType a
  => Array a
  -> (Double,Double)
det = (`infoFromArray2` af_det)

norm
  :: AFType a
  => Array a
  -> AFNormType
  -> Double
  -> Double
  -> Double
norm arr a b c =
  arr `infoFromArray` (\w y -> af_norm w y a b c)

isLAPACKAvailable :: IO Bool
isLAPACKAvailable = afCall1 af_is_lapack_available
