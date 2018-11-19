module ArrayFire.Exception where

import Control.Monad
import Foreign.C.String
import Foreign.Marshal
import Foreign.Storable

import ArrayFire.Internal.Exception
import ArrayFire.Internal.Defines

errorToString :: AFErr -> IO String
errorToString = peekCString <=< af_err_to_string
