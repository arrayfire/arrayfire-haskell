{-# LANGUAGE ViewPatterns #-}
--------------------------------------------------------------------------------
-- |
-- Module      : ArrayFire.Sparse
-- Copyright   : David Johnson (c) 2019-2020
-- License     : BSD3
-- Maintainer  : David Johnson <djohnson.m@gmail.com>
-- Stability   : Experimental
-- Portability : GHC
--
-- [ArrayFire Docs](http://arrayfire.org/docs/group__sparse__func.htm)
-- Functions to create and handle sparse arrays and matrix operations.
--
-- *Note*
-- Sparse functionality support was added to ArrayFire in v3.4.0.
--
-- >>> createSparseArray 10 10 (matrix @Double (10,10) [[1,2],[3,4]]) (vector @Int32 10 [1..]) (vector @Int32 10 [1..]) CSR
--
--
--------------------------------------------------------------------------------
module ArrayFire.Sparse where

import ArrayFire.Types
import ArrayFire.FFI
import ArrayFire.Internal.Sparse
import ArrayFire.Internal.Types
import Data.Int

-- | This function converts af::array of values, row indices and column indices into a sparse array.
--
-- [ArrayFire Docs](http://arrayfire.org/docs/group__sparse__func__create.htm#ga42c5cf729a232c1cbbcfe0f664f3b986)
--
-- *Note*
-- This function only create references of these arrays into the sparse data structure and does not do deep copies.
--
-- >>> createSparseArray 10 10 (matrix @Double (10,10) [[1,2],[3,4]]) (vector @Int32 10 [1..]) (vector @Int32 10 [1..]) CSR
--
createSparseArray
  :: (AFType a, Fractional a)
  => Int
  -- ^ is the number of rows in the dense matrix
  -> Int
  -- ^ is the number of columns in the dense matrix
  -> Array a
  -- ^ is the 'Array' containing the non-zero elements of the matrix
  -> Array Int32
  -- ^ is the row indices for the sparse array
  -> Array Int32
  -- ^ the column indices for the sparse array
  -> Storage
  -- ^ the storage format of the sparse array
  -> Array a
  -- ^ Sparse Array
createSparseArray (fromIntegral -> r) (fromIntegral -> c) arr1 arr2 arr3 s =
  op3Int arr1 arr2 arr3 (\p ar1 ar2 ar3 -> af_create_sparse_array p r c ar1 ar2 ar3 (toStorage s))

-- af_err af_create_sparse_array_from_ptr(af_array *out, const dim_t nRows, const dim_t nCols, const dim_t nNZ, const void * const values, const int * const rowIdx, const int * const colIdx, const af_dtype type, const af_storage stype, const af_source src);

-- | This function converts a dense af_array into a sparse array.
--
-- [ArrayFire Docs](http://arrayfire.org/docs/group__sparse__func__create.htm#ga52e3b2895cf9e9d697a06b4b44190d92)
--
-- *Note*
-- This function only create references of these arrays into the sparse data structure and does not do deep copies.
--
-- >>> createSparseArrayFromDense (matrix @Double (2,2) [[1,2],[3,4]]) CSR
-- ArrayFire Array
-- Storage Format : AF_STORAGE_CSR
-- [2 2 1 1]
-- ArrayFire Array: Values
-- [4 1 1 1]
--     1.0000     3.0000     2.0000     4.0000
-- ArrayFire Array: RowIdx
-- [3 1 1 1]
--          0          2          4
-- ArrayFire Array: ColIdx
-- [4 1 1 1]
--          0          1          0          1
--
createSparseArrayFromDense
  :: (AFType a, Fractional a)
  => Array a
  -- ^ is the source dense matrix
  -> Storage
  -- ^ is the storage format of the sparse array
  -> Array a
  -- ^ 'Array' for the sparse array with the given storage type
createSparseArrayFromDense a s =
  a `op1` (\p x -> af_create_sparse_array_from_dense p x (toStorage s))

-- | Convert an existing sparse array into a different storage format.
--
-- [ArrayFire Docs](http://arrayfire.org/docs/group__sparse__func__convert__to.htm)
--
-- Converting storage formats is allowed between 'CSR', 'COO' and DENSE.
--
-- When converting to DENSE, a dense array is returned.
--
-- *Note*
-- 'CSC' is currently not supported.
--
-- >>> array = createSparseArrayFromDense (matrix @Double (2,2) [[1,2],[3,4]]) CSR
-- >>> array
-- ArrayFire Array
-- Storage Format : AF_STORAGE_CSR
-- [2 2 1 1]
-- ArrayFire Array: Values
-- [4 1 1 1]
--     1.0000
--     3.0000
--     2.0000
--     4.0000

-- ArrayFire Array: RowIdx
-- [3 1 1 1]
--          0
--          2
--          4

-- ArrayFire Array: ColIdx
-- [4 1 1 1]
--          0
--          1
--          0
--          1
--
-- >>> sparseConvertTo array COO
-- ArrayFire Array
-- Storage Format : AF_STORAGE_COO
-- [2 2 1 1]
-- ArrayFire Array: Values
-- [4 1 1 1]
--     1.0000
--     2.0000
--     3.0000
--     4.0000

-- ArrayFire Array: RowIdx
-- [4 1 1 1]
--          0
--          1
--          0
--          1

-- ArrayFire Array: ColIdx
-- [4 1 1 1]
--          0
--          0
--          1
--          1
--
sparseConvertTo
  :: (AFType a, Fractional a)
  => Array a
  -- ^ is the source sparse matrix to be converted
  -> Storage
  -- ^ is the storage format of the output sparse array
  -> Array a
  -- ^ the sparse array with the given storage type
sparseConvertTo a s =
  a `op1` (\p x -> af_sparse_convert_to p x (toStorage s))

-- | Returns a dense array from a sparse input
--
-- [ArrayFire Docs](http://arrayfire.org/docs/group__sparse__func__dense.htm#ga80c3d8db78d537b74d9caebcf359b6a5)
--
-- Converts the sparse matrix into a dense matrix and returns it
--
-- >>> array = createSparseArrayFromDense (matrix @Double (2,2) [[1,2],[3,4]]) CSR
-- >>> array
-- ArrayFire Array
-- Storage Format : AF_STORAGE_CSR
-- [2 2 1 1]
-- ArrayFire Array: Values
-- [4 1 1 1]
--     1.0000
--     3.0000
--     2.0000
--     4.0000
--
-- ArrayFire Array: RowIdx
-- [3 1 1 1]
--          0
--          2
--          4
--
-- ArrayFire Array: ColIdx
-- [4 1 1 1]
--          0
--          1
--          0
--          1
--
sparseToDense
  :: (AFType a, Fractional a)
  => Array a
  -> Array a
sparseToDense = (`op1` af_sparse_to_dense)

-- | Returns reference to components of the input sparse array.
--
-- [ArrayFire Docs](http://arrayfire.org/docs/group__sparse__func__info.htm#gae6b553df80e21c174d374e82d8505ba5)
--
-- Returns reference to values, row indices, column indices and storage format of an input sparse array
--
-- >>> (values, cols, rows, storage) = sparseGetInfo $ createSparseArrayFromDense (matrix @Double (2,2) [[1,2],[3,4]]) CSR
-- >>> values
-- ArrayFire Array
-- [4 1 1 1]
--     1.0000
--     3.0000
--     2.0000
--     4.0000
--
-- >>> cols
-- ArrayFire Array
-- [3 1 1 1]
--          0
--          2
--          4
--
-- >>> rows
-- ArrayFire Array
-- [4 1 1 1]
--          0
--          1
--          0
--          1
--
-- >>> storage
-- CSR
--
sparseGetInfo
  :: (AFType a, Fractional a)
  => Array a
  -> (Array a, Array a, Array a, Storage)
sparseGetInfo x = do
  let (a,b,c,d) = x `op3p1` af_sparse_get_info
  (a,b,c,fromStorage d)

-- | Returns reference to the values component of the sparse array.
--
-- [ArrayFire Docs](http://arrayfire.org/docs/group__sparse__func__values.htm)
--
-- Returns reference to the values component of the sparse array.
-- Values is the 'Array' containing the non-zero elements of the dense matrix.
--
-- >>> sparseGetValues (createSparseArrayFromDense (matrix @Double (2,2) [[1,2],[3,4]]) CSR)
-- ArrayFire Array
-- [4 1 1 1]
--     1.0000
--     3.0000
--     2.0000
--     4.0000
--
sparseGetValues
  :: (AFType a, Fractional a)
  => Array a
  -> Array a
sparseGetValues = (`op1` af_sparse_get_values)

-- | Returns reference to the row indices component of the sparse array. More...
--
-- [ArrayFire Docs](http://arrayfire.org/docs/group__sparse__func__row__idx.htm)
--
-- Returns reference to the row indices component of the sparse array.
-- Row indices is the 'Array' containing the column indices of the sparse array.
--
-- >>> sparseGetRowIdx (createSparseArrayFromDense (matrix @Double (2,2) [[1,2],[3,4]]) CSR)
-- ArrayFire Array
-- [3 1 1 1]
--          0
--          2
--          4
--
sparseGetRowIdx
  :: (AFType a, Fractional a)
  => Array a
  -> Array a
sparseGetRowIdx = (`op1` af_sparse_get_row_idx)

-- | Returns reference to the column indices component of the sparse array. More...
--
-- [ArrayFire Docs](http://arrayfire.org/docs/group__sparse__func__col__idx.htm)
--
-- Returns reference to the column indices component of the sparse array.
-- Column indices is the 'Array' containing the column indices of the sparse array.
--
-- >>> sparseGetColIdx (createSparseArrayFromDense (matrix @Double (2,2) [[1,2],[3,4]]) CSR)
-- ArrayFire Array
-- [4 1 1 1]
--          0
--          1
--          0
--          1
--
sparseGetColIdx
  :: (AFType a, Fractional a)
  => Array a
  -> Array a
sparseGetColIdx = (`op1` af_sparse_get_col_idx)

-- | Returns the storage type of a sparse array.
--
-- [ArrayFire Docs](http://arrayfire.org/docs/group__sparse__func__storage.htm)
--
-- Returns the number of non zero elements in the sparse array.
-- This is always equal to the size of the values array.
--
-- >>> sparseGetStorage $ createSparseArrayFromDense (matrix @Double (2,2) [[1,2],[3,4]]) CSR
-- CSR
--
sparseGetStorage
  :: (AFType a, Fractional a)
  => Array a
  -> Storage
sparseGetStorage a =
  fromStorage (a `infoFromArray` af_sparse_get_storage)

-- | Returns the number of non zero elements in the sparse array. More...
--
-- [ArrayFire Docs](http://arrayfire.org/docs/group__sparse__func__nnz.htm#ga0c1ad61d829c02a280c28820eb91f03e)
--
-- Returns the number of non zero elements in the sparse array.
-- This is always equal to the size of the values array.
--
-- >>> sparseGetNNZ $ createSparseArrayFromDense (matrix @Double (2,2) [[1,2],[3,4]]) CSR
-- 4
--
sparseGetNNZ
  :: (AFType a, Fractional a)
  => Array a
  -> Int
sparseGetNNZ a =
  fromIntegral (a `infoFromArray` af_sparse_get_nnz)
