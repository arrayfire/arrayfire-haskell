module Data.Array.Fire.Internal.Array where

import Data.Array.Fire.Internal.Defines

-- #include "array.h"

import Foreign.Ptr

-- AFAPI af_err af_create_array(af_array *arr, const void * const data, const unsigned ndims, const dim_t * const dims, const af_dtype type);
-- AFAPI af_err af_create_handle(af_array *arr, const unsigned ndims, const dim_t * const dims, const af_dtype type);
-- AFAPI af_err af_copy_array(af_array *arr, const af_array in);
-- AFAPI af_err af_write_array(af_array arr, const void *data, const size_t bytes, af_source src);
-- AFAPI af_err af_get_data_ptr(void *data, const af_array arr);
-- AFAPI af_err af_release_array(af_array arr);
-- AFAPI af_err af_retain_array(af_array *out, const af_array in);
-- AFAPI af_err af_get_data_ref_count(int *use_count, const af_array in);
-- AFAPI af_err af_eval(af_array in);
-- AFAPI af_err af_eval_multiple(const int num, af_array *arrays);
-- AFAPI af_err af_set_manual_eval_flag(bool flag);
-- AFAPI af_err af_get_manual_eval_flag(bool *flag);
-- AFAPI af_err af_get_elements(dim_t *elems, const af_array arr);
-- AFAPI af_err af_get_type(af_dtype *type, const af_array arr);
-- AFAPI af_err af_get_dims(dim_t *d0, dim_t *d1, dim_t *d2, dim_t *d3,d  const af_array arr);
-- AFAPI af_err af_get_numdims(unsigned *result, const af_array arr);
-- AFAPI af_err af_is_empty        (bool *result, const af_array arr);
-- AFAPI af_err af_is_scalar       (bool *result, const af_array arr);
-- AFAPI af_err af_is_row          (bool *result, const af_array arr);
-- AFAPI af_err af_is_column       (bool *result, const af_array arr);
-- AFAPI af_err af_is_vector       (bool *result, const af_array arr);
-- AFAPI af_err af_is_complex      (bool *result, const af_array arr);
-- AFAPI af_err af_is_real         (bool *result, const af_array arr);
-- AFAPI af_err af_is_double       (bool *result, const af_array arr);
-- AFAPI af_err af_is_single       (bool *result, const af_array arr);
-- AFAPI af_err af_is_realfloating (bool *result, const af_array arr);
-- AFAPI af_err af_is_floating     (bool *result, const af_array arr);
-- AFAPI af_err af_is_integer      (bool *result, const af_array arr);
-- AFAPI af_err af_is_bool         (bool *result, const af_array arr);
-- AFAPI af_err af_is_sparse       (bool *result, const af_array arr);
-- AFAPI af_err af_get_scalar(void* output_value, const af_array arr);













