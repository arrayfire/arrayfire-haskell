module Data.Array.Fire.Internal.LAPACK where

--   AFAPI af_err af_svd(af_array *u, af_array *s, af_array *vt, const af_array in);
--   AFAPI af_err af_svd_inplace(af_array *u, af_array *s, af_array *vt, af_array in);
--   AFAPI af_err af_lu(af_array *lower, af_array *upper, af_array *pivot, const af_array in);
--   AFAPI af_err af_lu_inplace(af_array *pivot, af_array in, const bool is_lapack_piv);
--   AFAPI af_err af_qr(af_array *q, af_array *r, af_array *tau, const af_array in);
--   AFAPI af_err af_qr_inplace(af_array *tau, af_array in);
--   AFAPI af_err af_cholesky(af_array *out, int *info, const af_array in, const bool is_upper);
--   AFAPI af_err af_cholesky_inplace(int *info, af_array in, const bool is_upper);
--   AFAPI af_err af_solve(af_array *x, const af_array a, const af_array b, const af_mat_prop options);
--   AFAPI af_err af_solve_lu(af_array *x, const af_array a, const af_array piv, const af_array b, const af_mat_prop options);
--   AFAPI af_err af_inverse(af_array *out, const af_array in, const af_mat_prop options);
--   AFAPI af_err af_pinverse(af_array *out, const af_array in, const double tol, const af_mat_prop options);
--   AFAPI af_err af_rank(unsigned *rank, const af_array in, const double tol);
--   AFAPI af_err af_det(double *det_real, double *det_imag, const af_array in);
--   AFAPI af_err af_norm(double *out, const af_array in, const af_norm_type type, const double p, const double q);
--   AFAPI af_err af_is_lapack_available(bool *out);









