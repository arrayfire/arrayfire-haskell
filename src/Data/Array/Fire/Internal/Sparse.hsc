module Data.Array.Fire.Internal.Sparse where

 -- AFAPI af_err af_create_sparse_array(
 --                 af_array *out,
 --                 const dim_t nRows, const dim_t nCols,
 --                 const af_array values, const af_array rowIdx, const af_array colIdx,
 --                 const af_storage stype);

 --      AFAPI af_err af_create_sparse_array_from_ptr(
 --                 af_array *out,
 --                 const dim_t nRows, const dim_t nCols, const dim_t nNZ,
 --                 const void * const values,
 --                 const int * const rowIdx, const int * const colIdx,
 --                 const af_dtype type, const af_storage stype,
 --                 const af_source src);

 --    AFAPI af_err af_create_sparse_array_from_dense(
 --                 af_array *out, const af_array dense,
 --                 const af_storage stype);

 --      AFAPI af_err af_sparse_convert_to(af_array *out, const af_array in,
 --                                      const af_storage destStorage);

 --    AFAPI af_err af_sparse_to_dense(af_array *out, const af_array sparse);
 --    AFAPI af_err af_sparse_get_info(af_array *values, af_array *rowIdx, af_array *colIdx, af_storage *stype,
 --                                    const af_array in);
 --    AFAPI af_err af_sparse_get_values(af_array *out, const af_array in);
 --    AFAPI af_err af_sparse_get_row_idx(af_array *out, const af_array in);
 --    AFAPI af_err af_sparse_get_col_idx(af_array *out, const af_array in);
 --    AFAPI af_err af_sparse_get_nnz(dim_t *out, const af_array in);
 --    AFAPI af_err af_sparse_get_storage(af_storage *out, const af_array in);
