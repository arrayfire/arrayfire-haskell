#include "defines.h"

af_err af_constant(af_array *arr, const double val, const unsigned ndims, const dim_t * const dims, const af_dtype type);
af_err af_constant_complex(af_array *arr, const double real, const double imag, const unsigned ndims, const dim_t * const dims, const af_dtype type);
af_err af_constant_long (af_array *arr, const  intl val, const unsigned ndims, const dim_t * const dims);
af_err af_constant_ulong(af_array *arr, const uintl val, const unsigned ndims, const dim_t * const dims);
af_err af_range(af_array *out, const unsigned ndims, const dim_t * const dims, const int seq_dim, const af_dtype type);
af_err af_iota(af_array *out, const unsigned ndims, const dim_t * const dims, const unsigned t_ndims, const dim_t * const tdims, const af_dtype type);
af_err af_identity(af_array *out, const unsigned ndims, const dim_t * const dims, const af_dtype type);
af_err af_diag_create(af_array *out, const af_array in, const int num);
af_err af_diag_extract(af_array *out, const af_array in, const int num);
af_err af_join(af_array *out, const int dim, const af_array first, const af_array second);
af_err af_join_many(af_array *out, const int dim, const unsigned n_arrays, const af_array *inputs);
af_err af_tile(af_array *out, const af_array in, const unsigned x, const unsigned y, const unsigned z, const unsigned w);
af_err af_reorder(af_array *out, const af_array in, const unsigned x, const unsigned y, const unsigned z, const unsigned w);
af_err af_shift(af_array *out, const af_array in, const int x, const int y, const int z, const int w);
af_err af_moddims(af_array *out, const af_array in, const unsigned ndims, const dim_t * const dims);
af_err af_flat(af_array *out, const af_array in);
af_err af_flip(af_array *out, const af_array in, const unsigned dim);
af_err af_lower(af_array *out, const af_array in, bool is_unit_diag);
af_err af_upper(af_array *out, const af_array in, bool is_unit_diag);
af_err af_select(af_array *out, const af_array cond, const af_array a, const af_array b);
af_err af_select_scalar_r(af_array *out, const af_array cond, const af_array a, const double b);
af_err af_select_scalar_l(af_array *out, const af_array cond, const double a, const af_array b);
af_err af_replace(af_array a, const af_array cond, const af_array b);
af_err af_replace_scalar(af_array a, const af_array cond, const double b);
