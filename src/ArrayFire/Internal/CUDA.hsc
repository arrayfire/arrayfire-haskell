{-# LANGUAGE CPP #-}
module ArrayFire.Internal.CUDA where

import ArrayFire.Internal.Defines
import ArrayFire.Internal.Types
import Data.Word
import Data.Int
import Foreign.Ptr
import Foreign.C.Types

#include "af/cuda.h"
foreign import ccall unsafe "afcu_get_stream"
    afcu_get_stream :: Ptr CudaStreamT -> CInt -> IO AFErr
foreign import ccall unsafe "afcu_get_native_id"
    afcu_get_native_id :: Ptr CInt -> CInt -> IO AFErr
foreign import ccall unsafe "afcu_set_native_id"
    afcu_set_native_id :: CInt -> IO AFErr