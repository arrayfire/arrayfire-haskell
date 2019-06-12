{-# LANGUAGE CPP #-}
module ArrayFire.Internal.Internal where

import ArrayFire.Internal.Defines



import Foreign.Ptr
import Foreign.C.Types

#include "af/internal.h"
foreign import ccall unsafe "af_create_strided_array"
    af_create_strided_array :: Ptr AFArray -> Ptr () -> DimT -> CUInt -> Ptr DimT -> Ptr DimT -> AFDtype -> AFSource -> IO AFErr
foreign import ccall unsafe "af_get_strides"
    af_get_strides :: Ptr DimT -> Ptr DimT -> Ptr DimT -> Ptr DimT -> AFArray -> IO AFErr
foreign import ccall unsafe "af_get_offset"
    af_get_offset :: Ptr DimT -> AFArray -> IO AFErr
foreign import ccall unsafe "af_get_raw_ptr"
    af_get_raw_ptr :: Ptr (Ptr ()) -> AFArray -> IO AFErr
foreign import ccall unsafe "af_is_linear"
    af_is_linear :: Ptr Bool -> AFArray -> IO AFErr
foreign import ccall unsafe "af_is_owner"
    af_is_owner :: Ptr Bool -> AFArray -> IO AFErr
foreign import ccall unsafe "af_get_allocated_bytes"
    af_get_allocated_bytes :: Ptr CSize -> AFArray -> IO AFErr
