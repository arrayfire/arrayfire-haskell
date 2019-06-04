{-# LANGUAGE CPP #-}
module ArrayFire.Internal.Exception where

import ArrayFire.Internal.Defines
import ArrayFire.Internal.Types
import Data.Word
import Data.Int
import Foreign.Ptr
import Foreign.C.Types

foreign import ccall unsafe "af_get_last_error"
    af_get_last_error :: Ptr (Ptr CChar) -> Ptr DimT -> IO ()
foreign import ccall unsafe "af_err_to_string"
    af_err_to_string :: AFErr -> IO (Ptr CChar)