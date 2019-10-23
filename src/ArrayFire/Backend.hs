--------------------------------------------------------------------------------
-- |
-- Module      : ArrayFire.Backend
-- Copyright   : David Johnson (c) 2019-2020
-- License     : BSD 3
-- Maintainer  : David Johnson <djohnson.m@gmail.com>
-- Stability   : Experimental
-- Portability : GHC
--
-- Set and get available ArrayFire 'Backend's.
--
-- @
-- module Main where
--
-- import ArrayFire
--
-- main :: IO ()
-- main = print =<< getAvailableBackends
-- @
--
-- @
-- [CPU,OpenCL]
-- @
--------------------------------------------------------------------------------
module ArrayFire.Backend where

import ArrayFire.FFI
import ArrayFire.Internal.Backend
import ArrayFire.Internal.Types

-- | Set specific 'Backend' to use
--
-- >>> setBackend OpenCL
-- ()
setBackend
  :: Backend
  -- ^ 'Backend' to use for 'Array' construction
  -> IO ()
setBackend = afCall . af_set_backend . toAFBackend

-- | Retrieve count of Backends available
--
-- >>> getBackendCount
-- 2
--
getBackendCount :: IO Int
getBackendCount =
  fromIntegral <$>
    afCall1 af_get_backend_count

-- | Retrieve available 'Backend's
--
-- >>> mapM_ print =<< getAvailableBackends
-- CPU
-- OpenCL
getAvailableBackends :: IO [Backend]
getAvailableBackends =
  toBackends . fromIntegral <$>
    afCall1 af_get_available_backends

-- | Retrieve 'Backend' that specific 'Array' was created from
--
-- >>> getBackend (scalar @Double 2.0)
-- OpenCL
getBackend :: Array a -> Backend
getBackend = toBackend . flip infoFromArray af_get_backend_id

-- | Retrieve active 'Backend'
--
-- >>> getActiveBackend
-- OpenCL
getActiveBackend :: IO Backend
getActiveBackend = toBackend <$> afCall1 af_get_active_backend

-- | Retrieve Device ID that 'Array' was created from
--
-- >>> getDeviceID (scalar \@Double 2.0)
-- 1
getDeviceID :: Array a -> Int
getDeviceID = fromIntegral . flip infoFromArray af_get_device_id
