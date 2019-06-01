{-# LANGUAGE CPP #-}
module ArrayFire.Internal.Backend where

import ArrayFire.Internal.Defines
import ArrayFire.Internal.Types
import Data.Word
import Data.Int
import Foreign.Ptr
import Foreign.C.Types

#include "af/backend.h"
foreign import ccall unsafe "af_set_backend"
    af_set_backend :: AFBackend -> IO AFErr
foreign import ccall unsafe "af_get_backend_count"
    af_get_backend_count :: Ptr CUInt -> IO AFErr
foreign import ccall unsafe "af_get_available_backends"
    af_get_available_backends :: Ptr Int -> IO AFErr
foreign import ccall unsafe "af_get_backend_id"
    af_get_backend_id :: Ptr AFBackend -> AFArray -> IO AFErr
foreign import ccall unsafe "af_get_active_backend"
    af_get_active_backend :: Ptr AFBackend -> IO AFErr
foreign import ccall unsafe "af_get_device_id"
    af_get_device_id :: Ptr Int -> AFArray -> IO AFErr