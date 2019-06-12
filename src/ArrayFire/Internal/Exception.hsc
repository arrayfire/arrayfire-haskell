{-# LANGUAGE CPP #-}
module ArrayFire.Internal.Exception where

import ArrayFire.Internal.Defines
import Foreign.Ptr
import Foreign.C.Types

#include "af/defines.h"
foreign import ccall unsafe "af_get_last_error"
    af_get_last_error :: Ptr (Ptr CChar) -> Ptr DimT -> IO ()
foreign import ccall unsafe "af_err_to_string"
    af_err_to_string :: AFErr -> IO (Ptr CChar)
