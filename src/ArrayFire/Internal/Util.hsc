{-# LANGUAGE CPP #-}
module ArrayFire.Internal.Util where

import ArrayFire.Internal.Defines



import Foreign.Ptr
import Foreign.C.Types

#include "af/util.h"
foreign import ccall unsafe "af_print_array"
    af_print_array :: AFArray -> IO AFErr
foreign import ccall unsafe "af_print_array_gen"
    af_print_array_gen :: Ptr CChar -> AFArray -> Int -> IO AFErr
foreign import ccall unsafe "af_save_array"
    af_save_array :: Ptr Int -> Ptr CChar -> AFArray -> Ptr CChar -> Bool -> IO AFErr
foreign import ccall unsafe "af_read_array_index"
    af_read_array_index :: Ptr AFArray -> Ptr CChar -> CUInt -> IO AFErr
foreign import ccall unsafe "af_read_array_key"
    af_read_array_key :: Ptr AFArray -> Ptr CChar -> Ptr CChar -> IO AFErr
foreign import ccall unsafe "af_read_array_key_check"
    af_read_array_key_check :: Ptr Int -> Ptr CChar -> Ptr CChar -> IO AFErr
foreign import ccall unsafe "af_array_to_string"
    af_array_to_string :: Ptr (Ptr CChar) -> Ptr CChar -> AFArray -> Int -> Bool -> IO AFErr
foreign import ccall unsafe "af_example_function"
    af_example_function :: Ptr AFArray -> AFArray -> AFSomeEnum -> IO AFErr
foreign import ccall unsafe "af_get_version"
    af_get_version :: Ptr Int -> Ptr Int -> Ptr Int -> IO AFErr
foreign import ccall unsafe "af_get_revision"
    af_get_revision :: IO (Ptr CChar)
foreign import ccall unsafe "af_get_size_of"
    af_get_size_of :: Ptr CSize -> AFDtype -> IO AFErr
