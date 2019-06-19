{-# LANGUAGE CPP #-}
module ArrayFire.Internal.Util where

import ArrayFire.Internal.Defines
import ArrayFire.Internal.Types
import Data.Word
import Data.Int
import Foreign.Ptr
import Foreign.C.Types

#include "af/util.h"
foreign import ccall unsafe "af_print_array"
    af_print_array :: AFArray -> IO AFErr
foreign import ccall unsafe "af_print_array_gen"
    af_print_array_gen :: Ptr CChar -> AFArray -> CInt -> IO AFErr
foreign import ccall unsafe "af_save_array"
    af_save_array :: Ptr CInt -> Ptr CChar -> AFArray -> Ptr CChar -> CBool -> IO AFErr
foreign import ccall unsafe "af_read_array_index"
    af_read_array_index :: Ptr AFArray -> Ptr CChar -> CUInt -> IO AFErr
foreign import ccall unsafe "af_read_array_key"
    af_read_array_key :: Ptr AFArray -> Ptr CChar -> Ptr CChar -> IO AFErr
foreign import ccall unsafe "af_read_array_key_check"
    af_read_array_key_check :: Ptr CInt -> Ptr CChar -> Ptr CChar -> IO AFErr
foreign import ccall unsafe "af_array_to_string"
    af_array_to_string :: Ptr (Ptr CChar) -> Ptr CChar -> AFArray -> CInt -> CBool -> IO AFErr
foreign import ccall unsafe "af_example_function"
    af_example_function :: Ptr AFArray -> AFArray -> AFSomeEnum -> IO AFErr
foreign import ccall unsafe "af_get_version"
    af_get_version :: Ptr CInt -> Ptr CInt -> Ptr CInt -> IO AFErr
foreign import ccall unsafe "af_get_revision"
    af_get_revision :: IO (Ptr CChar)
foreign import ccall unsafe "af_get_size_of"
    af_get_size_of :: Ptr CSize -> AFDtype -> IO AFErr