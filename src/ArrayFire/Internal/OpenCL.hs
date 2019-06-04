{-# LANGUAGE CPP #-}
module ArrayFire.Internal.OpenCL where

import ArrayFire.Internal.Defines
import ArrayFire.Internal.Types
import Data.Word
import Data.Int
import Foreign.Ptr
import Foreign.C.Types

foreign import ccall unsafe "afcl_get_context"
    afcl_get_context :: Ptr ClContext -> Bool -> IO AFErr
foreign import ccall unsafe "afcl_get_queue"
    afcl_get_queue :: Ptr ClCommandQueue -> Bool -> IO AFErr
foreign import ccall unsafe "afcl_get_device_id"
    afcl_get_device_id :: Ptr ClDeviceId -> IO AFErr
foreign import ccall unsafe "afcl_set_device_id"
    afcl_set_device_id :: ClDeviceId -> IO AFErr
foreign import ccall unsafe "afcl_add_device_context"
    afcl_add_device_context :: ClDeviceId -> ClContext -> ClCommandQueue -> IO AFErr
foreign import ccall unsafe "afcl_set_device_context"
    afcl_set_device_context :: ClDeviceId -> ClContext -> IO AFErr
foreign import ccall unsafe "afcl_delete_device_context"
    afcl_delete_device_context :: ClDeviceId -> ClContext -> IO AFErr
foreign import ccall unsafe "afcl_get_device_type"
    afcl_get_device_type :: Ptr AfclDeviceType -> IO AFErr
foreign import ccall unsafe "afcl_get_platform"
    afcl_get_platform :: Ptr AFCLPlatform -> IO AFErr