module ArrayFire.LAPACK where

import Foreign.Marshal
import Foreign.Storable
import Foreign.C.String

import ArrayFire.Internal.LAPACK
import ArrayFire.Exception
import ArrayFire.Internal.Defines

isLAPACKAvailable :: IO Bool
isLAPACKAvailable = do
  alloca $ \ptr -> do
    af_is_lapack_available ptr
    peek ptr
