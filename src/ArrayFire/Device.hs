{-# LANGUAGE ViewPatterns        #-}
--------------------------------------------------------------------------------
-- |
-- Module      : ArrayFire.Device
-- Copyright   : David Johnson (c) 2019-2020
-- License     : BSD 3
-- Maintainer  : David Johnson <djohnson.m@gmail.com>
-- Stability   : Experimental
-- Portability : GHC
--
--------------------------------------------------------------------------------
module ArrayFire.Device where

import Foreign.C.String
import ArrayFire.Internal.Device
import ArrayFire.FFI

info :: IO ()
info = afCall af_info

afInit :: IO ()
afInit = afCall af_init

getInfoString :: IO String
getInfoString = peekCString =<< afCall1 (flip af_info_string 1)

-- af_err af_device_info(char* d_name, char* d_platform, char *d_toolkit, char* d_compute);

getDeviceCount :: IO Int
getDeviceCount = fromIntegral <$> afCall1 af_get_device_count

-- af_err af_get_dbl_support(bool* available, const int device);

setDevice :: Int -> IO ()
setDevice (fromIntegral -> x) = afCall (af_set_device x)

getDevice :: IO Int
getDevice = fromIntegral <$> afCall1 af_get_device

-- af_err af_sync(const int device);
-- af_err af_alloc_device(void **ptr, const dim_t bytes);
-- af_err af_free_device(void *ptr);
-- af_err af_alloc_pinned(void **ptr, const dim_t bytes);
-- af_err af_free_pinned(void *ptr);
-- af_err af_alloc_host(void **ptr, const dim_t bytes);
-- af_err af_free_host(void *ptr);
-- af_err af_device_array(af_array *arr, const void *data, const unsigned ndims, const dim_t * const dims, const af_dtype type);
-- af_err af_device_mem_info(size_t *alloc_bytes, size_t *alloc_buffers, size_t *lock_bytes, size_t *lock_buffers);
-- af_err af_print_mem_info(const char *msg, const int device_id);
-- af_err af_device_gc();
-- af_err af_set_mem_step_size(const size_t step_bytes);
-- af_err af_get_mem_step_size(size_t *step_bytes);
-- af_err af_lock_device_ptr(const af_array arr);
-- af_err af_unlock_device_ptr(const af_array arr);
-- af_err af_lock_array(const af_array arr);
-- af_err af_is_locked_array(bool *res, const af_array arr);
-- af_err af_get_device_ptr(void **ptr, const af_array arr);
