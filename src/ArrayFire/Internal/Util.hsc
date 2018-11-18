module ArrayFire.Internal.Util where

import ArrayFire.Internal.Defines

import Data.Word

import Data.Int

#include "util.h"

#include "extra.h"

import Foreign.Ptr

foreign import ccall unsafe "af_print_array_gen"
    af_print_array_gen :: Ptr Char -> AFArray -> Int -> IO AFErr
foreign import ccall unsafe "af_save_array"
    af_save_array :: Ptr Int -> Ptr Char -> AFArray -> Ptr Char -> Bool -> IO AFErr
foreign import ccall unsafe "af_read_array_index"
    af_read_array_index :: Ptr AFArray -> Ptr Char -> Word32 -> IO AFErr
foreign import ccall unsafe "af_read_array_key"
    af_read_array_key :: Ptr AFArray -> Ptr Char -> Ptr Char -> IO AFErr
foreign import ccall unsafe "af_read_array_key_check"
    af_read_array_key_check :: Ptr Int -> Ptr Char -> Ptr Char -> IO AFErr
foreign import ccall unsafe "af_array_to_string"
    af_array_to_string :: Ptr (Ptr Char) -> Ptr Char -> AFArray -> Int -> Bool -> IO AFErr
foreign import ccall unsafe "af_example_function"
    af_example_function :: Ptr AFArray -> AFArray -> AFSomeenumT -> IO AFErr
foreign import ccall unsafe "af_get_version"
    af_get_version :: Ptr Int -> Ptr Int -> Ptr Int -> IO AFErr
foreign import ccall unsafe "af_get_revision"
    af_get_revision :: IO (Ptr Char)
foreign import ccall unsafe "af_get_size_of"
    af_get_size_of :: Ptr Word -> AFDtype -> IO AFErr