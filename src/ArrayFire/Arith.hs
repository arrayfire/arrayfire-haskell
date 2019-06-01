module ArrayFire.Arith where

import Foreign.Marshal
import Foreign.Storable
import Foreign.C.String

import ArrayFire.Exception
import ArrayFire.Types
import ArrayFire.FFI
import ArrayFire.Internal.Arith
import ArrayFire.Internal.Defines

add
  :: AFType a
  => Array a
  -> Array a
  -> Batch
  -> Array a
add x y batch =
  x `op2` y $ \arr arr1 arr2 ->
    af_add arr arr1 arr2 batch

sub
  :: AFType a
  => Array a
  -> Array a
  -> Batch
  -> Array a
sub x y batch = do
  x `op2` y $ \arr arr1 arr2 ->
    af_add arr arr1 arr2 batch

mul
  :: AFType a
  => Array a
  -> Array a
  -> Batch
  -> Array a
mul x y batch = do
  x `op2` y $ \arr arr1 arr2 ->
    af_mul arr arr1 arr2 batch

div
  :: AFType a
  => Array a
  -> Array a
  -> Batch
  -> Array a
div x y batch = do
  x `op2` y $ \arr arr1 arr2 ->
    af_div arr arr1 arr2 batch

lt
  :: AFType a
  => Array a
  -> Array a
  -> Batch
  -> Array a
lt x y batch = do
  x `op2` y $ \arr arr1 arr2 ->
    af_lt arr arr1 arr2 batch

gt
  :: AFType a
  => Array a
  -> Array a
  -> Batch
  -> Array a
gt x y batch = do
  x `op2` y $ \arr arr1 arr2 ->
    af_gt arr arr1 arr2 batch

le
  :: AFType a
  => Array a
  -> Array a
  -> Batch
  -> Array a
le x y batch = do
  x `op2` y $ \arr arr1 arr2 ->
    af_le arr arr1 arr2 batch

ge
  :: AFType a
  => Array a
  -> Array a
  -> Batch
  -> Array a
ge x y batch = do
  x `op2` y $ \arr arr1 arr2 ->
    af_ge arr arr1 arr2 batch

eq
  :: AFType a
  => Array a
  -> Array a
  -> Batch
  -> Array a
eq x y batch = do
  x `op2` y $ \arr arr1 arr2 ->
    af_eq arr arr1 arr2 batch

neq
  :: AFArray
  -> AFArray
  -> Batch
  -> IO AFArray
neq arr1 arr2 batch = do
  alloca $ \arr -> do
    r <- af_neq arr arr1 arr2 batch
    putStrLn =<< errorToString r
    peek arr

and
  :: AFArray
  -> AFArray
  -> Batch
  -> IO AFArray
and arr1 arr2 batch = do
  alloca $ \arr -> do
    r <- af_and arr arr1 arr2 batch
    putStrLn =<< errorToString r
    peek arr

or
  :: AFArray
  -> AFArray
  -> Batch
  -> IO AFArray
or arr1 arr2 batch = do
  alloca $ \arr -> do
    r <- af_or arr arr1 arr2 batch
    putStrLn =<< errorToString r
    peek arr

not
  :: AFArray
  -> IO AFArray
not arr1 = do
  alloca $ \arr -> do
    r <- af_not arr arr1
    putStrLn =<< errorToString r
    peek arr

bitAnd
  :: AFArray
  -> AFArray
  -> Batch
  -> IO AFArray
bitAnd arr1 arr2 batch = do
  alloca $ \arr -> do
    r <- af_bitand arr arr1 arr2 batch
    putStrLn =<< errorToString r
    peek arr

bitOr
  :: AFArray
  -> AFArray
  -> Batch
  -> IO AFArray
bitOr arr1 arr2 batch = do
  alloca $ \arr -> do
    r <- af_bitor arr arr1 arr2 batch
    putStrLn =<< errorToString r
    peek arr

bitXor
  :: AFArray
  -> AFArray
  -> Batch
  -> IO AFArray
bitXor arr1 arr2 batch = do
  alloca $ \arr -> do
    r <- af_bitxor arr arr1 arr2 batch
    putStrLn =<< errorToString r
    peek arr

bitShiftL
  :: AFArray
  -> AFArray
  -> Batch
  -> IO AFArray
bitShiftL arr1 arr2 batch = do
  alloca $ \arr -> do
    r <- af_bitshiftl arr arr1 arr2 batch
    putStrLn =<< errorToString r
    peek arr

bitShiftR
  :: AFArray
  -> AFArray
  -> Batch
  -> IO AFArray
bitShiftR arr1 arr2 batch = do
  alloca $ \arr -> do
    r <- af_bitshiftr arr arr1 arr2 batch
    putStrLn =<< errorToString r
    peek arr

cast
  :: AFArray
  -> AFDtype
  -> IO AFArray
cast afArr typ = do
  alloca $ \arr -> do
    r <- af_cast arr afArr typ
    putStrLn =<< errorToString r
    peek arr

minOf
  :: AFArray
  -> AFArray
  -> Batch
  -> IO AFArray
minOf arr1 arr2 batch = do
  alloca $ \arr -> do
    r <- af_minof arr arr1 arr2 batch
    putStrLn =<< errorToString r
    peek arr

maxOf
  :: AFArray
  -> AFArray
  -> Batch
  -> IO AFArray
maxOf arr1 arr2 batch = do
  alloca $ \arr -> do
    r <- af_maxof arr arr1 arr2 batch
    putStrLn =<< errorToString r
    peek arr

clamp
  :: AFArray
  -> AFArray
  -> AFArray
  -> Batch
  -> IO AFArray
clamp a b c batch = do
  alloca $ \arr -> do
    r <- af_clamp arr a b c batch
    putStrLn =<< errorToString r
    peek arr

rem
  :: AFArray
  -> AFArray
  -> Batch
  -> IO AFArray
rem arr1 arr2 batch = do
  alloca $ \arr -> do
    r <- af_rem arr arr1 arr2 batch
    putStrLn =<< errorToString r
    peek arr

mod
  :: AFArray
  -> AFArray
  -> Batch
  -> IO AFArray
mod arr1 arr2 batch = do
  alloca $ \arr -> do
    r <- af_mod arr arr1 arr2 batch
    putStrLn =<< errorToString r
    peek arr

abs
  :: AFArray
  -> IO AFArray
abs arr1 = do
  alloca $ \arr -> do
    r <- af_abs arr arr1
    putStrLn =<< errorToString r
    peek arr

arg
  :: AFArray
  -> IO AFArray
arg arr1 = do
  alloca $ \arr -> do
    r <- af_arg arr arr1
    putStrLn =<< errorToString r
    peek arr

sign
  :: AFArray
  -> IO AFArray
sign arr1 = do
  alloca $ \arr -> do
    r <- af_sign arr arr1
    putStrLn =<< errorToString r
    peek arr

round
  :: AFArray
  -> IO AFArray
round arr1 = do
  alloca $ \arr -> do
    r <- af_round arr arr1
    putStrLn =<< errorToString r
    peek arr

trunc
  :: AFArray
  -> IO AFArray
trunc arr1 = do
  alloca $ \arr -> do
    r <- af_trunc arr arr1
    putStrLn =<< errorToString r
    peek arr

floor
  :: AFArray
  -> IO AFArray
floor arr1 = do
  alloca $ \arr -> do
    r <- af_floor arr arr1
    putStrLn =<< errorToString r
    peek arr

ceil
  :: AFArray
  -> IO AFArray
ceil arr1 = do
  alloca $ \arr -> do
    r <- af_ceil arr arr1
    putStrLn =<< errorToString r
    peek arr

-- hypot

sin
  :: AFArray
  -> IO AFArray
sin arr1 = do
  alloca $ \arr -> do
    r <- af_sin arr arr1
    putStrLn =<< errorToString r
    peek arr

cos
  :: AFArray
  -> IO AFArray
cos arr1 = do
  alloca $ \arr -> do
    r <- af_cos arr arr1
    putStrLn =<< errorToString r
    peek arr

tan
  :: AFArray
  -> IO AFArray
tan arr1 = do
  alloca $ \arr -> do
    r <- af_tan arr arr1
    putStrLn =<< errorToString r
    peek arr

asin
  :: AFArray
  -> IO AFArray
asin arr1 = do
  alloca $ \arr -> do
    r <- af_asin arr arr1
    putStrLn =<< errorToString r
    peek arr

acos
  :: AFArray
  -> IO AFArray
acos arr1 = do
  alloca $ \arr -> do
    r <- af_acos arr arr1
    putStrLn =<< errorToString r
    peek arr

atan
  :: AFArray
  -> IO AFArray
atan arr1 = do
  alloca $ \arr -> do
    r <- af_atan arr arr1
    putStrLn =<< errorToString r
    peek arr

--af_atan2
atan2
  :: AFArray
  -> AFArray
  -> Batch
  -> IO AFArray
atan2 arr1 arr2 batch = do
  alloca $ \arr -> do
    r <- af_atan2 arr arr1 arr2 batch
    putStrLn =<< errorToString r
    peek arr

cplx2
  :: AFArray
  -> AFArray
  -> Batch
  -> IO AFArray
cplx2 arr1 arr2 batch = do
  alloca $ \arr -> do
    r <- af_cplx2 arr arr1 arr2 batch
    putStrLn =<< errorToString r
    peek arr

cplx
  :: AFArray
  -> IO AFArray
cplx arr1 = do
  alloca $ \arr -> do
    r <- af_cplx arr arr1
    putStrLn =<< errorToString r
    peek arr

real
  :: AFArray
  -> IO AFArray
real arr1 = do
  alloca $ \arr -> do
    r <- af_real arr arr1
    putStrLn =<< errorToString r
    peek arr

imag
  :: AFArray
  -> IO AFArray
imag arr1 = do
  alloca $ \arr -> do
    r <- af_imag arr arr1
    putStrLn =<< errorToString r
    peek arr

conjg
  :: AFArray
  -> IO AFArray
conjg arr1 = do
  alloca $ \arr -> do
    r <- af_conjg arr arr1
    putStrLn =<< errorToString r
    peek arr

sinh
  :: AFArray
  -> IO AFArray
sinh arr1 = do
  alloca $ \arr -> do
    r <- af_sinh arr arr1
    putStrLn =<< errorToString r
    peek arr

cosh
  :: AFArray
  -> IO AFArray
cosh arr1 = do
  alloca $ \arr -> do
    r <- af_cosh arr arr1
    putStrLn =<< errorToString r
    peek arr

tanh
  :: AFArray
  -> IO AFArray
tanh arr1 = do
  alloca $ \arr -> do
    r <- af_tanh arr arr1
    putStrLn =<< errorToString r
    peek arr

root
  :: AFArray
  -> AFArray
  -> Batch
  -> IO AFArray
root arr1 arr2 batch = do
  alloca $ \arr -> do
    r <- af_root arr arr1 arr2 batch
    putStrLn =<< errorToString r
    peek arr

pow
  :: AFArray
  -> AFArray
  -> Batch
  -> IO AFArray
pow arr1 arr2 batch = do
  alloca $ \arr -> do
    r <- af_pow arr arr1 arr2 batch
    putStrLn =<< errorToString r
    peek arr

pow2
  :: AFArray
  -> IO AFArray
pow2 arr1 = do
  alloca $ \arr -> do
    r <- af_pow2 arr arr1
    putStrLn =<< errorToString r
    peek arr

exp
  :: AFArray
  -> IO AFArray
exp arr1 = do
  alloca $ \arr -> do
    r <- af_exp arr arr1
    putStrLn =<< errorToString r
    peek arr

sigmoid
  :: AFArray
  -> IO AFArray
sigmoid arr1 = do
  alloca $ \arr -> do
    r <- af_sigmoid arr arr1
    putStrLn =<< errorToString r
    peek arr

expm1
  :: AFArray
  -> IO AFArray
expm1 arr1 = do
  alloca $ \arr -> do
    r <- af_expm1 arr arr1
    putStrLn =<< errorToString r
    peek arr

erf
  :: AFArray
  -> IO AFArray
erf arr1 = do
  alloca $ \arr -> do
    r <- af_erf arr arr1
    putStrLn =<< errorToString r
    peek arr

erfc
  :: AFArray
  -> IO AFArray
erfc arr1 = do
  alloca $ \arr -> do
    r <- af_erfc arr arr1
    putStrLn =<< errorToString r
    peek arr

log
  :: AFArray
  -> IO AFArray
log arr1 = do
  alloca $ \arr -> do
    r <- af_log arr arr1
    putStrLn =<< errorToString r
    peek arr

log1p
  :: AFArray
  -> IO AFArray
log1p arr1 = do
  alloca $ \arr -> do
    r <- af_log1p arr arr1
    putStrLn =<< errorToString r
    peek arr

log10
  :: AFArray
  -> IO AFArray
log10 arr1 = do
  alloca $ \arr -> do
    r <- af_log10 arr arr1
    putStrLn =<< errorToString r
    peek arr

log2
  :: AFArray
  -> IO AFArray
log2 arr1 = do
  alloca $ \arr -> do
    r <- af_log2 arr arr1
    putStrLn =<< errorToString r
    peek arr

sqrt
  :: AFArray
  -> IO AFArray
sqrt arr1 = do
  alloca $ \arr -> do
    r <- af_sqrt arr arr1
    putStrLn =<< errorToString r
    peek arr

cbrt
  :: AFArray
  -> IO AFArray
cbrt arr1 = do
  alloca $ \arr -> do
    r <- af_cbrt arr arr1
    putStrLn =<< errorToString r
    peek arr

factorial
  :: AFArray
  -> IO AFArray
factorial arr1 = do
  alloca $ \arr -> do
    r <- af_factorial arr arr1
    putStrLn =<< errorToString r
    peek arr

tgamma
  :: AFArray
  -> IO AFArray
tgamma arr1 = do
  alloca $ \arr -> do
    r <- af_tgamma arr arr1
    putStrLn =<< errorToString r
    peek arr

lgamma
  :: AFArray
  -> IO AFArray
lgamma arr1 = do
  alloca $ \arr -> do
    r <- af_lgamma arr arr1
    putStrLn =<< errorToString r
    peek arr

isZero
  :: AFArray
  -> IO AFArray
isZero arr1 = do
  alloca $ \arr -> do
    r <- af_iszero arr arr1
    putStrLn =<< errorToString r
    peek arr

isInf
  :: AFArray
  -> IO AFArray
isInf arr1 = do
  alloca $ \arr -> do
    r <- af_isinf arr arr1
    putStrLn =<< errorToString r
    peek arr

isNaN
  :: AFArray
  -> IO AFArray
isNaN arr1 = do
  alloca $ \arr -> do
    r <- af_isnan arr arr1
    putStrLn =<< errorToString r
    peek arr
