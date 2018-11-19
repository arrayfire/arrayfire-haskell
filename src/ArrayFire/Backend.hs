module ArrayFire.Backend where

import Foreign.Marshal
import Foreign.Storable
import Foreign.C.String

import ArrayFire.Internal.Backend
import ArrayFire.Exception
import ArrayFire.Internal.Defines

setBackend :: AFBackend -> IO ()
setBackend backend = print =<< af_set_backend backend

getBackendCount :: IO Int
getBackendCount = do
  alloca $ \ptr -> do
   print =<< af_get_backend_count ptr
   fromIntegral <$> peek ptr

-- af_get_available_backends :: Ptr Int -> IO AFErr
-- af_get_backend_id :: Ptr AFBackend -> AFArray -> IO AFErr

getActiveBackend = do
  alloca $ \ptr -> do
   print =<< af_get_active_backend ptr
   peek ptr

-- af_get_device_id :: Ptr Int -> AFArray -> IO AFErr
