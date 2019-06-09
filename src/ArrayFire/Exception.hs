{-# LANGUAGE RecordWildCards #-}
module ArrayFire.Exception where

import Control.Exception hiding (TypeError)
import Data.Typeable
import Control.Monad
import Foreign.C.String
import Foreign.Marshal
import Foreign.Storable
import Foreign.Ptr

import ArrayFire.Internal.Exception
import ArrayFire.Internal.Defines

errorToString :: AFErr -> IO String
errorToString = peekCString <=< af_err_to_string

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

data AFException
  = AFException
  { afExceptionType :: AFExceptionType
  , afExceptionCode :: Int
  , afExceptionMsg  :: String
  } deriving (Show, Eq, Typeable)

instance Exception AFException

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

throwAFError :: AFErr -> IO ()
throwAFError exitCode =
  unless (exitCode == afSuccess) $ do
    let AFErr afExceptionCode = exitCode
        afExceptionType = toAFExceptionType exitCode
    afExceptionMsg <- errorToString exitCode
    throwIO AFException {..}

foreign import ccall unsafe "&af_release_random_engine"
  af_release_random_engine_finalizer :: FunPtr (AFRandomEngine -> IO ())

foreign import ccall unsafe "&af_release_array"
  af_release_array_finalizer :: FunPtr (AFArray -> IO ())
