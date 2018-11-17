module Data.Array.Fire.Internal.BLAS where

import Data.Array.Fire.Internal.Defines

-- #include "blah.h"

import Foreign.Ptr


-- AFAPI af_err af_matmul( af_array *out ,const af_array lhs, const af_array rhs, const af_mat_prop optLhs, const af_mat_prop optRhs);
-- AFAPI af_err af_dot(af_array *out, const af_array lhs, const af_array rhs, const af_mat_prop optLhs, const af_mat_prop optRhs);
-- AFAPI af_err af_dot_all(double *real, double *imag, const af_array lhs, const af_array rhs, const af_mat_prop optLhs, const af_mat_prop optRhs);
-- AFAPI af_err af_transpose(af_array *out, af_array in, const bool conjugate);
-- AFAPI af_err af_transpose_inplace(af_array in, const bool conjugate);



