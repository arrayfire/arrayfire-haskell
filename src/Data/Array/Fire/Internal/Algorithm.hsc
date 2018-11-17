module Data.Array.Fire.Internal.Algorithm where

import Data.Array.Fire.Internal.Defines

#include "algorithm.h"

-- 33 functions

-- AFAPI af_err af_sum(af_array *out, const af_array in, const int dim);
-- AFAPI af_err af_sum_nan(af_array *out, const af_array in, const int dim, const double nanval);
-- AFAPI af_err af_product(af_array *out, const af_array in, const int dim);
-- AFAPI af_err af_product_nan(af_array *out, const af_array in, const int dim, const double nanval);
-- AFAPI af_err af_min(af_array *out, const af_array in, const int dim);
-- AFAPI af_err af_max(af_array *out, const af_array in, const int dim);
-- AFAPI af_err af_all_true(af_array *out, const af_array in, const int dim);
-- AFAPI af_err af_any_true(af_array *out, const af_array in, const int dim);
-- AFAPI af_err af_count(af_array *out, const af_array in, const int dim);
-- AFAPI af_err af_sum_all(double *real, double *imag, const af_array in);
-- AFAPI af_err af_sum_nan_all(double *real, double *imag, const af_array in, const double nanval);
-- AFAPI af_err af_product_all(double *real, double *imag, const af_array in);
-- AFAPI af_err af_product_nan_all(double *real, double *imag, const af_array in, const double nanval);
-- AFAPI af_err af_min_all(double *real, double *imag, const af_array in);
-- AFAPI af_err af_max_all(double *real, double *imag, const af_array in);
-- AFAPI af_err af_all_true_all(double *real, double *imag, const af_array in);
-- AFAPI af_err af_any_true_all(double *real, double *imag, const af_array in);
-- AFAPI af_err af_count_all(double *real, double *imag, const af_array in);
-- AFAPI af_err af_imin(af_array *out, af_array *idx, const af_array in, const int dim);
-- AFAPI af_err af_imax(af_array *out, af_array *idx, const af_array in, const int dim);
-- AFAPI af_err af_imin_all(double *real, double *imag, unsigned *idx, const af_array in);
-- AFAPI af_err af_imax_all(double *real, double *imag, unsigned *idx, const af_array in);
-- AFAPI af_err af_accum(af_array *out, const af_array in, const int dim);
-- AFAPI af_err af_scan(af_array *out, const af_array in, const int dim, af_binary_op op, bool inclusive_scan);
-- AFAPI af_err af_scan_by_key(af_array *out, const af_array key, const af_array in, const int dim, af_binary_op op, bool inclusive_scan);
-- AFAPI af_err af_where(af_array *idx, const af_array in);
-- AFAPI af_err af_diff1(af_array *out, const af_array in, const int dim);
-- AFAPI af_err af_diff2(af_array *out, const af_array in, const int dim);
-- AFAPI af_err af_sort(af_array *out, const af_array in, const unsigned dim, const bool isAscending);
-- AFAPI af_err af_sort_index(af_array *out, af_array *indices, const af_array in, const unsigned dim, const bool isAscending);
-- AFAPI af_err af_sort_by_key(af_array *out_keys, af_array *out_values, const af_array keys, const af_array values, const unsigned dim, const bool isAscending);
-- AFAPI af_err af_set_unique(af_array *out, const af_array in, const bool is_sorted);
-- AFAPI af_err af_set_union(af_array *out, const af_array first, const af_array second, const bool is_unique);
-- AFAPI af_err af_set_intersect(af_array *out, const af_array first, const af_array second, const bool is_unique);
















