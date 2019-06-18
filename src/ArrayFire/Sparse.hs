{-# LANGUAGE ViewPatterns #-}
--------------------------------------------------------------------------------
-- |
-- Module      : ArrayFire.Sparse
-- Copyright   : David Johnson (c) 2019-2020
-- License     : BSD 3
-- Maintainer  : David Johnson <djohnson.m@gmail.com>
-- Stability   : Experimental
-- Portability : GHC
--
--------------------------------------------------------------------------------
module ArrayFire.Sparse where

import ArrayFire.Types
import ArrayFire.FFI
import ArrayFire.Internal.Sparse


createSparseArray
  :: AFType a
  => Int
  -> Int
  -> Array a
  -> Array a
  -> Array a
  -> Storage
  -> Array a
createSparseArray (fromIntegral -> r) (fromIntegral -> c) arr1 arr2 arr3 s =
  op3 arr1 arr2 arr3 (\p ar1 ar2 ar3 -> af_create_sparse_array p r c ar1 ar2 ar3 (toStorage s))

-- af_err af_create_sparse_array_from_ptr(af_array *out, const dim_t nRows, const dim_t nCols, const dim_t nNZ, const void * const values, const int * const rowIdx, const int * const colIdx, const af_dtype type, const af_storage stype, const af_source src);

createSparseArrayFromDense
  :: AFType a
  => Array a
  -> Storage
  -> Array a
createSparseArrayFromDense a s =
  a `op1` (\p x -> af_create_sparse_array_from_dense p x (toStorage s))

sparseConvertTo
  :: AFType a
  => Array a
  -> Storage
  -> Array a
sparseConvertTo a s =
  a `op1` (\p x -> af_sparse_convert_to p x (toStorage s))

sparseToDense
  :: AFType a
  => Array a
  -> Array a
sparseToDense = (`op1` af_sparse_to_dense)

sparseGetInfo
  :: AFType a
  => Array a
  -> (Array a, Array a, Array a, Storage)
sparseGetInfo x = do
  let (a,b,c,d) = x `op3p1` af_sparse_get_info
  (a,b,c,fromStorage d)

sparseGetValues
  :: AFType a
  => Array a
  -> Array a
sparseGetValues = (`op1` af_sparse_get_values)

sparseGetRowIdx
  :: AFType a
  => Array a
  -> Array a
sparseGetRowIdx = (`op1` af_sparse_get_row_idx)

sparseGetColIdx
  :: AFType a
  => Array a
  -> Array a
sparseGetColIdx = (`op1` af_sparse_get_col_idx)

sparseGetNNZ
  :: AFType a
  => Array a
  -> Int
sparseGetNNZ a =
  fromIntegral (a `infoFromArray` af_sparse_get_nnz)

sparseGetStorage
  :: AFType a
  => Array a
  -> Storage
sparseGetStorage a =
  fromStorage (a `infoFromArray` af_sparse_get_storage)
