{-# LANGUAGE ViewPatterns    #-}
{-# LANGUAGE RecordWildCards #-}
--------------------------------------------------------------------------------
-- |
-- Module      : ArrayFire.Exception
-- Copyright   : David Johnson (c) 2019-2020
-- License     : BSD 3
-- Maintainer  : David Johnson <code@dmj.io>
-- Stability   : Experimental
-- Portability : GHC
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
-- [nix-shell:~\/arrayfire]$ .\/main
-- [CPU,OpenCL]
-- @
--------------------------------------------------------------------------------
module ArrayFire.Exception where

import Control.Exception hiding (TypeError)
import Data.Typeable
import Control.Monad
import Foreign.C.String
import Foreign.Ptr
import ArrayFire.Internal.Exception
import ArrayFire.Internal.Defines

-- | String representation of ArrayFire exception
errorToString :: AFErr -> IO String
errorToString = peekCString <=< af_err_to_string

-- | ArrayFire exception type
data AFExceptionType
  = NoMemoryError
  | DriverError
  | RuntimeError
  | InvalidArrayError
  | ArgError
  | SizeError
  | TypeError
  | DiffTypeError
  | BatchError
  | DeviceError
  | NotSupportedError
  | NotConfiguredError
  | NonFreeError
  | NoDblError
  | NoGfxError
  | LoadLibError
  | LoadSymError
  | BackendMismatchError
  | InternalError
  | UnknownError
  | UnhandledError
  deriving (Show, Eq, Typeable)

-- | Exception type for ArrayFire API
data AFException
  = AFException
  { afExceptionType :: AFExceptionType
  -- ^ The Exception type to throw
  , afExceptionCode :: Int
  -- ^ Code representing the exception
  , afExceptionMsg  :: String
  -- ^ Exception message
  } deriving (Show, Eq, Typeable)

instance Exception AFException

-- | Conversion function helper
toAFExceptionType :: AFErr -> AFExceptionType
toAFExceptionType (AFErr 101) = NoMemoryError
toAFExceptionType (AFErr 102) = DriverError
toAFExceptionType (AFErr 103) = RuntimeError
toAFExceptionType (AFErr 201) = InvalidArrayError
toAFExceptionType (AFErr 202) = ArgError
toAFExceptionType (AFErr 203) = SizeError
toAFExceptionType (AFErr 204) = TypeError
toAFExceptionType (AFErr 205) = DiffTypeError
toAFExceptionType (AFErr 207) = BatchError
toAFExceptionType (AFErr 208) = DeviceError
toAFExceptionType (AFErr 301) = NotSupportedError
toAFExceptionType (AFErr 302) = NotConfiguredError
toAFExceptionType (AFErr 303) = NonFreeError
toAFExceptionType (AFErr 401) = NoDblError
toAFExceptionType (AFErr 402) = NoGfxError
toAFExceptionType (AFErr 501) = LoadLibError
toAFExceptionType (AFErr 502) = LoadSymError
toAFExceptionType (AFErr 503) = BackendMismatchError
toAFExceptionType (AFErr 998) = InternalError
toAFExceptionType (AFErr 999) = UnknownError
toAFExceptionType (AFErr _) = UnhandledError

-- | Throws an ArrayFire Exception
throwAFError :: AFErr -> IO ()
throwAFError exitCode =
  unless (exitCode == afSuccess) $ do
    let AFErr (fromIntegral -> afExceptionCode) = exitCode
        afExceptionType = toAFExceptionType exitCode
    afExceptionMsg <- errorToString exitCode
    throwIO AFException {..}

foreign import ccall unsafe "&af_release_random_engine"
  af_release_random_engine_finalizer :: FunPtr (AFRandomEngine -> IO ())

foreign import ccall unsafe "&af_destroy_window"
  af_release_window_finalizer :: FunPtr (AFWindow -> IO ())

foreign import ccall unsafe "&af_release_array"
  af_release_array_finalizer :: FunPtr (AFArray -> IO ())
