module Data.Array.Fire.Internal.Backend where

import Data.Array.Fire.Internal.Defines

#include "backend.h"

import Foreign.Ptr

-- AFAPI af_err af_set_backend(const af_backend bknd);
foreign import ccall unsafe "af_set_backend"
  setBackend :: AFBackend -> IO AFError
-- AFAPI af_err af_get_backend_count(unsigned* num_backends);
foreign import ccall unsafe "af_set_backend"
  getAFBackendCount :: Ptr Int -> IO AFError
-- AFAPI af_err af_get_available_backends(int* backends);
foreign import ccall unsafe "af_get_available_backends"
  getAvailableAFBackends :: Ptr Int -> IO AFError
-- AFAPI af_err af_get_backend_id(af_backend *backend, const af_array in);
foreign import ccall unsafe "af_get_backend_id"
  getAFBackendId :: Ptr AFBackend -> AFArray -> IO AFError
-- AFAPI af_err af_get_active_backend(af_backend *backend);
foreign import ccall unsafe "af_get_active_backend"
  getActiveAFBackend :: Ptr AFBackend -> IO AFError
-- AFAPI af_err af_get_device_id(int *device, const af_array in);
foreign import ccall unsafe "af_get_device_id"
  getDeviceID :: Ptr Int -> AFArray -> IO AFError

