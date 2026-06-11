{-# LANGUAGE ViewPatterns        #-}
--------------------------------------------------------------------------------
-- |
-- Module      : ArrayFire.Device
-- Copyright   : David Johnson (c) 2019-2026
-- License     : BSD3
-- Maintainer  : David Johnson <code@dmj.io>
-- Stability   : Experimental
-- Portability : GHC
--
-- Information about ArrayFire API and devices
--
-- >>> info
--  ArrayFire v3.6.4 (OpenCL, 64-bit Mac OSX, build 1b8030c5)
-- [0] APPLE: AMD Radeon Pro 555X Compute Engine, 4096 MB
-- -1- APPLE: Intel(R) UHD Graphics 630, 1536 MB
--
--------------------------------------------------------------------------------
module ArrayFire.Device where

import Control.Exception (finally)
import Foreign.C.String
import ArrayFire.Internal.Device
import ArrayFire.FFI

foreign import ccall unsafe "af_notify_shutdown"
  afNotifyShutdown :: IO ()

-- | Bracket for ArrayFire usage.  Wrap your @main@ (or top-level IO action)
-- with this to ensure the safe-finalizer shutdown flag is set before GHC's
-- finalizer thread runs, preventing a "double free or corruption" abort when
-- GC-managed array handles outlive ArrayFire's C++ allocator teardown.
--
-- @
-- main :: IO ()
-- main = withArrayFire $ do
--   ...
-- @
withArrayFire :: IO a -> IO a
withArrayFire action = action `finally` afNotifyShutdown

-- | Retrieve info from ArrayFire API
--
-- @
-- ArrayFire v3.6.4 (OpenCL, 64-bit Mac OSX, build 1b8030c5)
-- [0] APPLE: AMD Radeon Pro 555X Compute Engine, 4096 MB
-- -1- APPLE: Intel(R) UHD Graphics 630, 1536 MB
-- @
info :: IO ()
info = afCall af_info

-- | Calls /af_init/ C function from ArrayFire API
--
-- >>> afInit
-- ()
afInit :: IO ()
afInit = afCall af_init

-- | Retrieves ArrayFire device information as 'String', same as 'info'.
--
-- >>> getInfoString
-- "ArrayFire v3.6.4 (OpenCL, 64-bit Mac OSX, build 1b8030c5)\n[0] APPLE: AMD Radeon Pro 555X Compute Engine, 4096 MB\n-1- APPLE: Intel(R) UHD Graphics 630, 1536 MB\n"
getInfoString :: IO String
getInfoString = peekCString =<< afCall1 (flip af_info_string 1)

-- | Retrieves count of devices
--
-- >>> getDeviceCount
-- 2
getDeviceCount :: IO Int
getDeviceCount = fromIntegral <$> afCall1 af_get_device_count

-- | Sets a device by 'Int'
--
-- >>> setDevice 0
-- ()
setDevice :: Int -> IO ()
setDevice (fromIntegral -> x) = afCall (af_set_device x)

-- | Retrieves device identifier
--
-- >>> getDevice
-- 0
getDevice :: IO Int
getDevice = fromIntegral <$> afCall1 af_get_device

-- | Runs the device garbage collector, freeing any cached memory buffers.
deviceGC :: IO ()
deviceGC = afCall af_device_gc
