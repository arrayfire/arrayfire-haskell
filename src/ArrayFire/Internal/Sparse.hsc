module ArrayFire.Internal.Sparse where

import ArrayFire.Internal.Defines

import Data.Word

import Data.Int

#include "sparse.h"

#include "extra.h"

import Foreign.Ptr

foreign import ccall unsafe "af_create_sparse_array_from_ptr"
    af_create_sparse_array_from_ptr :: Ptr AFArray -> Word64 -> Word64 -> Word64 -> Ptr () -> Ptr Int -> Ptr Int -> AFDtype -> AFStorage -> AFSource -> IO AFErr
foreign import ccall unsafe "af_create_sparse_array_from_dense"
    af_create_sparse_array_from_dense :: Ptr AFArray -> AFArray -> AFStorage -> IO AFErr
foreign import ccall unsafe "af_sparse_convert_to"
    af_sparse_convert_to :: Ptr AFArray -> AFArray -> AFStorage -> IO AFErr
foreign import ccall unsafe "af_sparse_to_dense"
    af_sparse_to_dense :: Ptr AFArray -> AFArray -> IO AFErr
foreign import ccall unsafe "af_sparse_get_info"
    af_sparse_get_info :: Ptr AFArray -> Ptr AFArray -> Ptr AFArray -> Ptr AFStorage -> AFArray -> IO AFErr
foreign import ccall unsafe "af_sparse_get_values"
    af_sparse_get_values :: Ptr AFArray -> AFArray -> IO AFErr
foreign import ccall unsafe "af_sparse_get_row_idx"
    af_sparse_get_row_idx :: Ptr AFArray -> AFArray -> IO AFErr
foreign import ccall unsafe "af_sparse_get_col_idx"
    af_sparse_get_col_idx :: Ptr AFArray -> AFArray -> IO AFErr
foreign import ccall unsafe "af_sparse_get_nnz"
    af_sparse_get_nnz :: Ptr Word64 -> AFArray -> IO AFErr
foreign import ccall unsafe "af_sparse_get_storage"
    af_sparse_get_storage :: Ptr AFStorage -> AFArray -> IO AFErr