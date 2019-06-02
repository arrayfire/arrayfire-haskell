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
  :: AFType a
  => Array a
  -> Array a
  -> Batch
  -> Array a
neq x y batch = do
  x `op2` y $ \arr arr1 arr2 ->
    af_neq arr arr1 arr2 batch

and
  :: AFType a
  => Array a
  -> Array a
  -> Batch
  -> Array a
and x y batch = do
  x `op2` y $ \arr arr1 arr2 ->
    af_and arr arr1 arr2 batch

or
  :: AFType a
  => Array a
  -> Array a
  -> Batch
  -> Array a
or x y batch = do
  x `op2` y $ \arr arr1 arr2 ->
    af_or arr arr1 arr2 batch

not
  :: AFType a
  => Array a
  -> Array a
not = flip op1 af_not

bitAnd
  :: AFType a
  => Array a
  -> Array a
  -> Batch
  -> Array a
bitAnd x y batch = do
  x `op2` y $ \arr arr1 arr2 ->
    af_bitand arr arr1 arr2 batch

bitOr
  :: AFType a
  => Array a
  -> Array a
  -> Batch
  -> Array a
bitOr x y batch = do
  x `op2` y $ \arr arr1 arr2 ->
    af_bitor arr arr1 arr2 batch

bitXor
  :: AFType a
  => Array a
  -> Array a
  -> Batch
  -> Array a
bitXor x y batch = do
  x `op2` y $ \arr arr1 arr2 ->
    af_bitxor arr arr1 arr2 batch

bitShiftL
  :: AFType a
  => Array a
  -> Array a
  -> Batch
  -> Array a
bitShiftL x y batch = do
  x `op2` y $ \arr arr1 arr2 ->
    af_bitshiftl arr arr1 arr2 batch

bitShiftR
  :: AFType a
  => Array a
  -> Array a
  -> Batch
  -> Array a
bitShiftR x y batch = do
  x `op2` y $ \arr arr1 arr2 ->
    af_bitshiftr arr arr1 arr2 batch

-- fix me
cast
  :: (AFType a, AFType b)
  => AFArray a
  -> AFArray b
  -> IO AFArray
cast afArr typ = do
  alloca $ \arr -> do
    r <- af_cast arr afArr typ
    putStrLn =<< errorToString r
    peek arr

minOf
  :: AFType a
  => Array a
  -> Array a
  -> Batch
  -> Array a
minOf x y batch = do
  x `op2` y $ \arr arr1 arr2 ->
    af_minof arr arr1 arr2 batch

maxOf
  :: AFType a
  => Array a
  -> Array a
  -> Batch
  -> Array a
maxOf x y batch = do
  x `op2` y $ \arr arr1 arr2 ->
    af_maxof arr arr1 arr2 batch

-- fix me
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
  :: AFType a
  => Array a
  -> Array a
  -> Batch
  -> Array a
rem x y batch = do
  x `op2` y $ \arr arr1 arr2 ->
    af_rem arr arr1 arr2 batch

mod
  :: AFType a
  => Array a
  -> Array a
  -> Batch
  -> Array a
mod x y batch = do
  x `op2` y $ \arr arr1 arr2 ->
    af_mod arr arr1 arr2 batch

abs
  :: AFType a
  => Array a
  -> Array a
abs = flip op1 af_abs

arg
  :: AFType a
  => Array a
  -> Array a
arg = flip op1 af_arg

sign
  :: AFType a
  => Array a
  -> Array a
sign = flip op1 af_sign

round
  :: AFType a
  => Array a
  -> Array a
round = flip op1 af_round

trunc
  :: AFType a
  => Array a
  -> Array a
trunc = flip op1 af_trunc

floor
  :: AFType a
  => Array a
  -> Array a
floor = flip op1 af_floor

ceil
  :: AFType a
  => Array a
  -> Array a
ceil = flip op1 af_ceil

sin
  :: AFType a
  => Array a
  -> Array a
sin = flip op1 af_sin

cos
  :: AFType a
  => Array a
  -> Array a
cos = flip op1 af_cos

tan
  :: AFType a
  => Array a
  -> Array a
tan = flip op1 af_tan

asin
  :: AFType a
  => Array a
  -> Array a
asin = flip op1 af_asin

acos
  :: AFType a
  => Array a
  -> Array a
acos = flip op1 af_acos

atan
  :: AFType a
  => Array a
  -> Array a
atan = flip op1 af_atan

atan2
  :: AFType a
  => Array a
  -> Array a
  -> Batch
  -> Array a
atan2 x y batch = do
  x `op2` y $ \arr arr1 arr2 ->
    af_atan2 arr arr1 arr2 batch

cplx2
  :: AFType a
  => Array a
  -> Array a
  -> Batch
  -> Array a
cplx2 x y batch = do
  x `op2` y $ \arr arr1 arr2 ->
    af_cplx2 arr arr1 arr2 batch

cplx
  :: AFType a
  => Array a
  -> Array a
cplx = flip op1 af_cplx

real
  :: AFType a
  => Array a
  -> Array a
real = flip op1 af_real

imag
  :: AFType a
  => Array a
  -> Array a
imag = flip op1 af_imag

conjg
  :: AFType a
  => Array a
  -> Array a
conjg = flip op1 af_conjg

sinh
  :: AFType a
  => Array a
  -> Array a
sinh = flip op1 af_sinh

cosh
  :: AFType a
  => Array a
  -> Array a
cosh = flip op1 af_cosh

tanh
  :: AFType a
  => Array a
  -> Array a
tanh = flip op1 af_tanh

root
  :: AFType a
  => Array a
  -> Array a
  -> Batch
  -> Array a
root x y batch = do
  x `op2` y $ \arr arr1 arr2 ->
    af_root arr arr1 arr2 batch

pow
  :: AFType a
  => Array a
  -> Array a
  -> Batch
  -> Array a
pow x y batch = do
  x `op2` y $ \arr arr1 arr2 ->
    af_pow arr arr1 arr2 batch

pow2
  :: AFType a
  => Array a
  -> Array a
pow2 = flip op1 af_pow2

exp
  :: AFType a
  => Array a
  -> Array a
exp = flip op1 af_exp

sigmoid
  :: AFType a
  => Array a
  -> Array a
sigmoid = flip op1 af_sigmoid

expm1
  :: AFType a
  => Array a
  -> Array a
expm1 = flip op1 af_expm1

erf
  :: AFType a
  => Array a
  -> Array a
erf = flip op1 af_erf

erfc
  :: AFType a
  => Array a
  -> Array a
erfc = flip op1 af_erfc

log
  :: AFType a
  => Array a
  -> Array a
log = flip op1 af_log

log1p
  :: AFType a
  => Array a
  -> Array a
log1p = flip op1 af_log1p

log10
  :: AFType a
  => Array a
  -> Array a
log10 = flip op1 af_log10

log2
  :: AFType a
  => Array a
  -> Array a
log2 = flip op1 af_log2

sqrt
  :: AFType a
  => Array a
  -> Array a
sqrt = flip op1 af_sqrt

cbrt
  :: AFType a
  => Array a
  -> Array a
cbrt = flip op1 af_cbrt

factorial
  :: AFType a
  => Array a
  -> Array a
factorial = flip op1 af_factorial

tgamma
  :: AFType a
  => Array a
  -> Array a
tgamma = flip op1 af_tgamma

lgamma
  :: AFType a
  => Array a
  -> Array a
lgamma = flip op1 af_lgamma

isZero
  :: AFType a
  => Array a
  -> Array a
isZero = flip op1 af_iszero

isInf
  :: AFType a
  => Array a
  -> Array a
isInf = flip op1 af_isinf

isNan
  :: AFType a
  => Array a
  -> Array a
isNan = flip op1 af_isnan
