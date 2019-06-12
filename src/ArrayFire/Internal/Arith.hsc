{-# LANGUAGE CPP #-}
module ArrayFire.Internal.Arith where

import ArrayFire.Internal.Defines



import Foreign.Ptr


#include "af/arith.h"
foreign import ccall unsafe "af_add"
    af_add :: Ptr AFArray -> AFArray -> AFArray -> Bool -> IO AFErr
foreign import ccall unsafe "af_sub"
    af_sub :: Ptr AFArray -> AFArray -> AFArray -> Bool -> IO AFErr
foreign import ccall unsafe "af_mul"
    af_mul :: Ptr AFArray -> AFArray -> AFArray -> Bool -> IO AFErr
foreign import ccall unsafe "af_div"
    af_div :: Ptr AFArray -> AFArray -> AFArray -> Bool -> IO AFErr
foreign import ccall unsafe "af_lt"
    af_lt :: Ptr AFArray -> AFArray -> AFArray -> Bool -> IO AFErr
foreign import ccall unsafe "af_gt"
    af_gt :: Ptr AFArray -> AFArray -> AFArray -> Bool -> IO AFErr
foreign import ccall unsafe "af_le"
    af_le :: Ptr AFArray -> AFArray -> AFArray -> Bool -> IO AFErr
foreign import ccall unsafe "af_ge"
    af_ge :: Ptr AFArray -> AFArray -> AFArray -> Bool -> IO AFErr
foreign import ccall unsafe "af_eq"
    af_eq :: Ptr AFArray -> AFArray -> AFArray -> Bool -> IO AFErr
foreign import ccall unsafe "af_neq"
    af_neq :: Ptr AFArray -> AFArray -> AFArray -> Bool -> IO AFErr
foreign import ccall unsafe "af_and"
    af_and :: Ptr AFArray -> AFArray -> AFArray -> Bool -> IO AFErr
foreign import ccall unsafe "af_or"
    af_or :: Ptr AFArray -> AFArray -> AFArray -> Bool -> IO AFErr
foreign import ccall unsafe "af_not"
    af_not :: Ptr AFArray -> AFArray -> IO AFErr
foreign import ccall unsafe "af_bitand"
    af_bitand :: Ptr AFArray -> AFArray -> AFArray -> Bool -> IO AFErr
foreign import ccall unsafe "af_bitor"
    af_bitor :: Ptr AFArray -> AFArray -> AFArray -> Bool -> IO AFErr
foreign import ccall unsafe "af_bitxor"
    af_bitxor :: Ptr AFArray -> AFArray -> AFArray -> Bool -> IO AFErr
foreign import ccall unsafe "af_bitshiftl"
    af_bitshiftl :: Ptr AFArray -> AFArray -> AFArray -> Bool -> IO AFErr
foreign import ccall unsafe "af_bitshiftr"
    af_bitshiftr :: Ptr AFArray -> AFArray -> AFArray -> Bool -> IO AFErr
foreign import ccall unsafe "af_cast"
    af_cast :: Ptr AFArray -> AFArray -> AFDtype -> IO AFErr
foreign import ccall unsafe "af_minof"
    af_minof :: Ptr AFArray -> AFArray -> AFArray -> Bool -> IO AFErr
foreign import ccall unsafe "af_maxof"
    af_maxof :: Ptr AFArray -> AFArray -> AFArray -> Bool -> IO AFErr
foreign import ccall unsafe "af_clamp"
    af_clamp :: Ptr AFArray -> AFArray -> AFArray -> AFArray -> Bool -> IO AFErr
foreign import ccall unsafe "af_rem"
    af_rem :: Ptr AFArray -> AFArray -> AFArray -> Bool -> IO AFErr
foreign import ccall unsafe "af_mod"
    af_mod :: Ptr AFArray -> AFArray -> AFArray -> Bool -> IO AFErr
foreign import ccall unsafe "af_abs"
    af_abs :: Ptr AFArray -> AFArray -> IO AFErr
foreign import ccall unsafe "af_arg"
    af_arg :: Ptr AFArray -> AFArray -> IO AFErr
foreign import ccall unsafe "af_sign"
    af_sign :: Ptr AFArray -> AFArray -> IO AFErr
foreign import ccall unsafe "af_round"
    af_round :: Ptr AFArray -> AFArray -> IO AFErr
foreign import ccall unsafe "af_trunc"
    af_trunc :: Ptr AFArray -> AFArray -> IO AFErr
foreign import ccall unsafe "af_floor"
    af_floor :: Ptr AFArray -> AFArray -> IO AFErr
foreign import ccall unsafe "af_ceil"
    af_ceil :: Ptr AFArray -> AFArray -> IO AFErr
foreign import ccall unsafe "af_hypot"
    af_hypot :: Ptr AFArray -> AFArray -> AFArray -> Bool -> IO AFErr
foreign import ccall unsafe "af_sin"
    af_sin :: Ptr AFArray -> AFArray -> IO AFErr
foreign import ccall unsafe "af_cos"
    af_cos :: Ptr AFArray -> AFArray -> IO AFErr
foreign import ccall unsafe "af_tan"
    af_tan :: Ptr AFArray -> AFArray -> IO AFErr
foreign import ccall unsafe "af_asin"
    af_asin :: Ptr AFArray -> AFArray -> IO AFErr
foreign import ccall unsafe "af_acos"
    af_acos :: Ptr AFArray -> AFArray -> IO AFErr
foreign import ccall unsafe "af_atan"
    af_atan :: Ptr AFArray -> AFArray -> IO AFErr
foreign import ccall unsafe "af_atan2"
    af_atan2 :: Ptr AFArray -> AFArray -> AFArray -> Bool -> IO AFErr
foreign import ccall unsafe "af_cplx2"
    af_cplx2 :: Ptr AFArray -> AFArray -> AFArray -> Bool -> IO AFErr
foreign import ccall unsafe "af_cplx"
    af_cplx :: Ptr AFArray -> AFArray -> IO AFErr
foreign import ccall unsafe "af_real"
    af_real :: Ptr AFArray -> AFArray -> IO AFErr
foreign import ccall unsafe "af_imag"
    af_imag :: Ptr AFArray -> AFArray -> IO AFErr
foreign import ccall unsafe "af_conjg"
    af_conjg :: Ptr AFArray -> AFArray -> IO AFErr
foreign import ccall unsafe "af_sinh"
    af_sinh :: Ptr AFArray -> AFArray -> IO AFErr
foreign import ccall unsafe "af_cosh"
    af_cosh :: Ptr AFArray -> AFArray -> IO AFErr
foreign import ccall unsafe "af_tanh"
    af_tanh :: Ptr AFArray -> AFArray -> IO AFErr
foreign import ccall unsafe "af_asinh"
    af_asinh :: Ptr AFArray -> AFArray -> IO AFErr
foreign import ccall unsafe "af_acosh"
    af_acosh :: Ptr AFArray -> AFArray -> IO AFErr
foreign import ccall unsafe "af_atanh"
    af_atanh :: Ptr AFArray -> AFArray -> IO AFErr
foreign import ccall unsafe "af_root"
    af_root :: Ptr AFArray -> AFArray -> AFArray -> Bool -> IO AFErr
foreign import ccall unsafe "af_pow"
    af_pow :: Ptr AFArray -> AFArray -> AFArray -> Bool -> IO AFErr
foreign import ccall unsafe "af_pow2"
    af_pow2 :: Ptr AFArray -> AFArray -> IO AFErr
foreign import ccall unsafe "af_exp"
    af_exp :: Ptr AFArray -> AFArray -> IO AFErr
foreign import ccall unsafe "af_sigmoid"
    af_sigmoid :: Ptr AFArray -> AFArray -> IO AFErr
foreign import ccall unsafe "af_expm1"
    af_expm1 :: Ptr AFArray -> AFArray -> IO AFErr
foreign import ccall unsafe "af_erf"
    af_erf :: Ptr AFArray -> AFArray -> IO AFErr
foreign import ccall unsafe "af_erfc"
    af_erfc :: Ptr AFArray -> AFArray -> IO AFErr
foreign import ccall unsafe "af_log"
    af_log :: Ptr AFArray -> AFArray -> IO AFErr
foreign import ccall unsafe "af_log1p"
    af_log1p :: Ptr AFArray -> AFArray -> IO AFErr
foreign import ccall unsafe "af_log10"
    af_log10 :: Ptr AFArray -> AFArray -> IO AFErr
foreign import ccall unsafe "af_log2"
    af_log2 :: Ptr AFArray -> AFArray -> IO AFErr
foreign import ccall unsafe "af_sqrt"
    af_sqrt :: Ptr AFArray -> AFArray -> IO AFErr
foreign import ccall unsafe "af_cbrt"
    af_cbrt :: Ptr AFArray -> AFArray -> IO AFErr
foreign import ccall unsafe "af_factorial"
    af_factorial :: Ptr AFArray -> AFArray -> IO AFErr
foreign import ccall unsafe "af_tgamma"
    af_tgamma :: Ptr AFArray -> AFArray -> IO AFErr
foreign import ccall unsafe "af_lgamma"
    af_lgamma :: Ptr AFArray -> AFArray -> IO AFErr
foreign import ccall unsafe "af_iszero"
    af_iszero :: Ptr AFArray -> AFArray -> IO AFErr
foreign import ccall unsafe "af_isinf"
    af_isinf :: Ptr AFArray -> AFArray -> IO AFErr
foreign import ccall unsafe "af_isnan"
    af_isnan :: Ptr AFArray -> AFArray -> IO AFErr
