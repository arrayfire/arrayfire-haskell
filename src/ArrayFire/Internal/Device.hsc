{-# LANGUAGE CPP #-}
module ArrayFire.Internal.Device where

import ArrayFire.Internal.Defines



import Foreign.Ptr
import Foreign.C.Types

#include "af/device.h"
foreign import ccall unsafe "af_info"
    af_info :: IO AFErr
foreign import ccall unsafe "af_init"
    af_init :: IO AFErr
foreign import ccall unsafe "af_info_string"
    af_info_string :: Ptr (Ptr CChar) -> Bool -> IO AFErr
foreign import ccall unsafe "af_device_info"
    af_device_info :: Ptr CChar -> Ptr CChar -> Ptr CChar -> Ptr CChar -> IO AFErr
foreign import ccall unsafe "af_get_device_count"
    af_get_device_count :: Ptr Int -> IO AFErr
foreign import ccall unsafe "af_get_dbl_support"
    af_get_dbl_support :: Ptr Bool -> Int -> IO AFErr
foreign import ccall unsafe "af_set_device"
    af_set_device :: Int -> IO AFErr
foreign import ccall unsafe "af_get_device"
    af_get_device :: Ptr Int -> IO AFErr
foreign import ccall unsafe "af_sync"
    af_sync :: Int -> IO AFErr
foreign import ccall unsafe "af_alloc_device"
    af_alloc_device :: Ptr (Ptr ()) -> DimT -> IO AFErr
foreign import ccall unsafe "af_free_device"
    af_free_device :: Ptr () -> IO AFErr
foreign import ccall unsafe "af_alloc_pinned"
    af_alloc_pinned :: Ptr (Ptr ()) -> DimT -> IO AFErr
foreign import ccall unsafe "af_free_pinned"
    af_free_pinned :: Ptr () -> IO AFErr
foreign import ccall unsafe "af_alloc_host"
    af_alloc_host :: Ptr (Ptr ()) -> DimT -> IO AFErr
foreign import ccall unsafe "af_free_host"
    af_free_host :: Ptr () -> IO AFErr
foreign import ccall unsafe "af_device_array"
    af_device_array :: Ptr AFArray -> Ptr () -> CUInt -> Ptr DimT -> AFDtype -> IO AFErr
foreign import ccall unsafe "af_device_mem_info"
    af_device_mem_info :: Ptr CSize -> Ptr CSize -> Ptr CSize -> Ptr CSize -> IO AFErr
foreign import ccall unsafe "af_print_mem_info"
    af_print_mem_info :: Ptr CChar -> Int -> IO AFErr
foreign import ccall unsafe "af_device_gc"
    af_device_gc :: IO AFErr
foreign import ccall unsafe "af_set_mem_step_size"
    af_set_mem_step_size :: CSize -> IO AFErr
foreign import ccall unsafe "af_get_mem_step_size"
    af_get_mem_step_size :: Ptr CSize -> IO AFErr
foreign import ccall unsafe "af_lock_device_ptr"
    af_lock_device_ptr :: AFArray -> IO AFErr
foreign import ccall unsafe "af_unlock_device_ptr"
    af_unlock_device_ptr :: AFArray -> IO AFErr
foreign import ccall unsafe "af_lock_array"
    af_lock_array :: AFArray -> IO AFErr
foreign import ccall unsafe "af_is_locked_array"
    af_is_locked_array :: Ptr Bool -> AFArray -> IO AFErr
foreign import ccall unsafe "af_get_device_ptr"
    af_get_device_ptr :: Ptr (Ptr ()) -> AFArray -> IO AFErr
