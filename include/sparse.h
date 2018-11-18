#include "defines.h"

af_err af_create_sparse_array(af_array *out, const dim_t nRows, const dim_t nCols, const af_array values, const af_array rowIdx, const af_array colIdx, const af_storage stype);
af_err af_create_sparse_array_from_ptr(af_array *out, const dim_t nRows, const dim_t nCols, const dim_t nNZ, const void * const values, const int * const rowIdx, const int * const colIdx, const af_dtype type, const af_storage stype, const af_source src);
af_err af_create_sparse_array_from_dense(af_array *out, const af_array dense, const af_storage stype);
af_err af_sparse_convert_to(af_array *out, const af_array in, const af_storage destStorage);
af_err af_sparse_to_dense(af_array *out, const af_array sparse);
af_err af_sparse_get_info(af_array *values, af_array *rowIdx, af_array *colIdx, af_storage *stype, const af_array in);
af_err af_sparse_get_values(af_array *out, const af_array in);
af_err af_sparse_get_row_idx(af_array *out, const af_array in);
af_err af_sparse_get_col_idx(af_array *out, const af_array in);
af_err af_sparse_get_nnz(dim_t *out, const af_array in);
af_err af_sparse_get_storage(af_storage *out, const af_array in);
