module ArrayFire.Backend where

import Foreign.Marshal
import Foreign.Storable
import Foreign.C.String

import ArrayFire.Internal.Backend
import ArrayFire.Exception
import ArrayFire.FFI
import ArrayFire.Types
import ArrayFire.Internal.Defines

setBackend :: Backend -> IO ()
setBackend = afCall . af_set_backend . toAFBackend

getBackendCount :: IO Int
getBackendCount =
  fromIntegral <$>
    afCall1 af_get_backend_count

getAvailableBackends :: IO [Backend]
getAvailableBackends =
  toBackends <$> afCall1 af_get_available_backends

getBackendID :: Array a -> Backend
getBackendID = toBackend . flip infoFromArray af_get_backend_id

getActiveBackend :: IO Backend
getActiveBackend = toBackend <$> afCall1 af_get_active_backend

getDeviceID :: Array a -> Int
getDeviceID = flip infoFromArray af_get_device_id
