module ArrayFire.BLAS where

import Foreign.Marshal
import Foreign.Storable
import Foreign.C.String

-- import ArrayFire.Internal.Util
import ArrayFire.Internal.BLAS
import ArrayFire.Exception
import ArrayFire.Internal.Defines

matmul :: AFArray -> AFArray -> AFMatProp -> AFMatProp -> IO AFArray
matmul arr1 arr2 prop1 prop2 = do
  alloca $ \ptr -> do
    print =<< af_matmul ptr arr1 arr2 prop1 prop2
    peek ptr
