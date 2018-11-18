#include "defines.h"

af_err af_mean(af_array *out, const af_array in, const dim_t dim);
af_err af_mean_weighted(af_array *out, const af_array in, const af_array weights, const dim_t dim);
af_err af_var(af_array *out, const af_array in, const bool isbiased, const dim_t dim);
af_err af_var_weighted(af_array *out, const af_array in, const af_array weights, const dim_t dim);
af_err af_meanvar(af_array *mean, af_array *var, const af_array in, const af_array weights, const af_var_bias bias, const dim_t dim);
af_err af_stdev(af_array *out, const af_array in, const dim_t dim);
af_err af_cov(af_array* out, const af_array X, const af_array Y, const bool isbiased);
af_err af_median(af_array* out, const af_array in, const dim_t dim);
af_err af_mean_all(double *real, double *imag, const af_array in);
af_err af_mean_all_weighted(double *real, double *imag, const af_array in, const af_array weights);
af_err af_var_all(double *realVal, double *imagVal, const af_array in, const bool isbiased);
af_err af_var_all_weighted(double *realVal, double *imagVal, const af_array in, const af_array weights);
af_err af_stdev_all(double *real, double *imag, const af_array in);
af_err af_median_all(double *realVal, double *imagVal, const af_array in);
af_err af_corrcoef(double *realVal, double *imagVal, const af_array X, const af_array Y);
af_err af_topk(af_array *values, af_array *indices, const af_array in, const int k, const int dim, const af_topk_function order);
