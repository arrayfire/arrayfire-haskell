module ArrayFire.Device where

import Foreign.Marshal
import Foreign.Storable
import Foreign.C.String

-- import ArrayFire.Internal.Util
import ArrayFire.Internal.Device
import ArrayFire.Internal.Defines

getInfo :: IO ()
getInfo = () <$ af_info

getInfoString :: IO String
getInfoString = do
  putStrLn "get info string"
  alloca $ \ptr -> do
    r <- af_info_string ptr True
    peekCString =<< peek ptr

getDeviceCount :: IO Int
getDeviceCount = do
  putStrLn "get device count"
  alloca $ \ptr -> do
    r <- af_get_device_count ptr
    peek ptr

getDevice :: IO Int
getDevice = do
  putStrLn "get device"
  alloca $ \ptr -> do
    r <- af_get_device ptr
    peek ptr
