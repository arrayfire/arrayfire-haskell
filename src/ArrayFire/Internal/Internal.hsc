module ArrayFire.Internal.Internal where

import ArrayFire.Internal.Defines

import Data.Word

import Data.Int

#include "internal.h"

#include "extra.h"

import Foreign.Ptr

foreign import ccall unsafe "af_get_strides"
    af_get_strides :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> AFArray -> IO AFErr
foreign import ccall unsafe "af_get_offset"
    af_get_offset :: Ptr Word64 -> AFArray -> IO AFErr
foreign import ccall unsafe "af_get_raw_ptr"
    af_get_raw_ptr :: Ptr (Ptr ()) -> AFArray -> IO AFErr
foreign import ccall unsafe "af_is_linear"
    af_is_linear :: Ptr Bool -> AFArray -> IO AFErr
foreign import ccall unsafe "af_is_owner"
    af_is_owner :: Ptr Bool -> AFArray -> IO AFErr
foreign import ccall unsafe "af_get_allocated_bytes"
    af_get_allocated_bytes :: Ptr Word -> AFArray -> IO AFErr