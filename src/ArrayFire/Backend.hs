module ArrayFire.Backend where

import Foreign.Marshal
import Foreign.Storable
import Foreign.C.String

import ArrayFire.Internal.Backend
import ArrayFire.Exception
import ArrayFire.FFI
import ArrayFire.Types
import ArrayFire.Internal.Defines

setBackend :: AFBackend -> IO ()
setBackend backend =
  afCall (af_set_backend backend)

getBackendCount :: IO Int
getBackendCount =
  fromIntegral <$>
    afCall1 af_get_backend_count

getAvailableBackends :: IO Int
getAvailableBackends =
  afCall1 af_get_available_backends

getBackendId :: Array a -> AFBackend
getBackendId = flip infoFromArray af_get_backend_id

getActiveBackend :: IO AFBackend
getActiveBackend = afCall1 af_get_active_backend

getDeviceID :: Array a -> Int
getDeviceID = flip infoFromArray af_get_device_id
