{-# LANGUAGE CPP #-}
module ArrayFire.Internal.Array where

import ArrayFire.Internal.Defines
import ArrayFire.Internal.Types
import Data.Word
import Data.Int
import Foreign.Ptr
import Foreign.C.Types

#include "af/array.h"
foreign import ccall unsafe "af_create_array"
    af_create_array :: Ptr AFArray -> Ptr () -> CUInt -> Ptr DimT -> AFDtype -> IO AFErr
foreign import ccall unsafe "af_create_handle"
    af_create_handle :: Ptr AFArray -> CUInt -> Ptr DimT -> AFDtype -> IO AFErr
foreign import ccall unsafe "af_copy_array"
    af_copy_array :: Ptr AFArray -> AFArray -> IO AFErr
foreign import ccall unsafe "af_write_array"
    af_write_array :: AFArray -> Ptr () -> CSize -> AFSource -> IO AFErr
foreign import ccall unsafe "af_get_data_ptr"
    af_get_data_ptr :: Ptr () -> AFArray -> IO AFErr
foreign import ccall unsafe "af_release_array"
    af_release_array :: AFArray -> IO AFErr
foreign import ccall unsafe "af_retain_array"
    af_retain_array :: Ptr AFArray -> AFArray -> IO AFErr
foreign import ccall unsafe "af_get_data_ref_count"
    af_get_data_ref_count :: Ptr Int -> AFArray -> IO AFErr
foreign import ccall unsafe "af_eval"
    af_eval :: AFArray -> IO AFErr
foreign import ccall unsafe "af_eval_multiple"
    af_eval_multiple :: Int -> Ptr AFArray -> IO AFErr
foreign import ccall unsafe "af_set_manual_eval_flag"
    af_set_manual_eval_flag :: Bool -> IO AFErr
foreign import ccall unsafe "af_get_manual_eval_flag"
    af_get_manual_eval_flag :: Ptr Bool -> IO AFErr
foreign import ccall unsafe "af_get_elements"
    af_get_elements :: Ptr DimT -> AFArray -> IO AFErr
foreign import ccall unsafe "af_get_type"
    af_get_type :: Ptr AFDtype -> AFArray -> IO AFErr
foreign import ccall unsafe "af_get_dims"
    af_get_dims :: Ptr DimT -> Ptr DimT -> Ptr DimT -> Ptr DimT -> AFArray -> IO AFErr
foreign import ccall unsafe "af_get_numdims"
    af_get_numdims :: Ptr CUInt -> AFArray -> IO AFErr
foreign import ccall unsafe "af_is_empty"
    af_is_empty :: Ptr Bool -> AFArray -> IO AFErr
foreign import ccall unsafe "af_is_scalar"
    af_is_scalar :: Ptr Bool -> AFArray -> IO AFErr
foreign import ccall unsafe "af_is_row"
    af_is_row :: Ptr Bool -> AFArray -> IO AFErr
foreign import ccall unsafe "af_is_column"
    af_is_column :: Ptr Bool -> AFArray -> IO AFErr
foreign import ccall unsafe "af_is_vector"
    af_is_vector :: Ptr Bool -> AFArray -> IO AFErr
foreign import ccall unsafe "af_is_complex"
    af_is_complex :: Ptr Bool -> AFArray -> IO AFErr
foreign import ccall unsafe "af_is_real"
    af_is_real :: Ptr Bool -> AFArray -> IO AFErr
foreign import ccall unsafe "af_is_double"
    af_is_double :: Ptr Bool -> AFArray -> IO AFErr
foreign import ccall unsafe "af_is_single"
    af_is_single :: Ptr Bool -> AFArray -> IO AFErr
foreign import ccall unsafe "af_is_realfloating"
    af_is_realfloating :: Ptr Bool -> AFArray -> IO AFErr
foreign import ccall unsafe "af_is_floating"
    af_is_floating :: Ptr Bool -> AFArray -> IO AFErr
foreign import ccall unsafe "af_is_integer"
    af_is_integer :: Ptr Bool -> AFArray -> IO AFErr
foreign import ccall unsafe "af_is_bool"
    af_is_bool :: Ptr Bool -> AFArray -> IO AFErr
foreign import ccall unsafe "af_is_sparse"
    af_is_sparse :: Ptr Bool -> AFArray -> IO AFErr
foreign import ccall unsafe "af_get_scalar"
    af_get_scalar :: Ptr () -> AFArray -> IO AFErr