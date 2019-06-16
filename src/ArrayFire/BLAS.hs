module ArrayFire.BLAS where

import Data.Complex

import ArrayFire.FFI
import ArrayFire.Internal.BLAS
import ArrayFire.Types

matmul :: Array a -> Array a -> MatProp -> MatProp -> Array a
matmul arr1 arr2 prop1 prop2 = do
  op2 arr1 arr2 (\p a b -> af_matmul p a b (toMatProp prop1) (toMatProp prop2))

dot :: Array a -> Array a -> MatProp -> MatProp -> Array a
dot arr1 arr2 prop1 prop2 = do
  op2 arr1 arr2 (\p a b -> af_dot p a b (toMatProp prop1) (toMatProp prop2))

-- | Scalar dot product between two vectors
dotAll
  :: Array a
  -> Array a
  -> MatProp
  -> MatProp
  -> Complex Double
dotAll arr1 arr2 prop1 prop2 = do
  let (real,imag) =
        infoFromArray22 arr1 arr2 $ \a b c d ->
          af_dot_all a b c d (toMatProp prop1) (toMatProp prop2)
  real :+ imag

transpose :: Array a -> Bool -> Array a
transpose arr1 b =
  arr1 `op1` (\x y -> af_transpose x y b)

transposeInPlace :: Array a -> Bool -> IO ()
transposeInPlace arr b =
  arr `inPlace` (`af_transpose_inplace` b)
