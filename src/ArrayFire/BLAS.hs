module ArrayFire.BLAS where

import Foreign.Marshal
import Foreign.Storable
import Foreign.C.String

import ArrayFire.Internal.BLAS
import ArrayFire.Exception
import ArrayFire.FFI
import ArrayFire.Types
import ArrayFire.Internal.Defines

matmul :: Array a -> Array a -> MatProp -> MatProp -> Array a
matmul arr1 arr2 prop1 prop2 = do
  op2 arr1 arr2 (\p a b -> af_matmul p a b (toMatProp prop1) (toMatProp prop2))

dot :: Array a -> Array a -> AFMatProp -> AFMatProp -> Array a
dot arr1 arr2 prop1 prop2 = do
  op2 arr1 arr2 (\p a b -> af_dot p a b prop1 prop2)

dotAll
  :: Array a
  -> Array a
  -> MatProp
  -> MatProp
  -> (Double, Double)
dotAll arr1 arr2 prop1 prop2 =
  infoFromArray22 arr1 arr2 $ \a b c d ->
    af_dot_all a b c d (toMatProp prop1) (toMatProp prop2)

transpose :: Array a -> Bool -> Array a
transpose arr1 b =
  arr1 `op1` (\x y -> af_transpose x y b)

transposeInPlace :: Array a -> Bool -> Array a
transposeInPlace arr1 b =
  arr1 `op1` (\x y -> af_transpose x y b)
