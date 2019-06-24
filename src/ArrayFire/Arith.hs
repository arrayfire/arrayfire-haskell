{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}
--------------------------------------------------------------------------------
-- |
-- Module      : ArrayFire.Arith
-- Copyright   : David Johnson (c) 2019-2020
-- License     : BSD 3
-- Maintainer  : David Johnson <djohnson.m@gmail.com>
-- Stability   : Experimental
-- Portability : GHC
--
-- Arithmetic functions over 'Array'
--
-- @
-- module Main where
--
-- import ArrayFire
--
-- main :: IO ()
-- main = print =<< getAvailableBackends
-- @
--
-- @
-- [nix-shell:~\/arrayfire]$ .\/main
-- [CPU,OpenCL]
-- @
--------------------------------------------------------------------------------
module ArrayFire.Arith where

import Prelude                  (Bool(..), ($), (.), flip, fromEnum, fromIntegral, Real)

import Data.Coerce
import Data.Proxy

import ArrayFire.FFI
import ArrayFire.Internal.Arith
import ArrayFire.Internal.Types

-- | Adds two 'Array' objects
--
-- @
-- >>> (scalar \@Int 1 \`add\` scalar \@Int 1) True
-- @
--
add
  :: AFType a
  => Array a
  -- ^ First input
  -> Array a
  -- ^ Second input
  -> Bool
  -- ^ Use batch
  -> Array a
  -- ^ Result of add
add x y (fromIntegral . fromEnum -> batch) =
  x `op2` y $ \arr arr1 arr2 ->
    af_add arr arr1 arr2 batch

-- | Subtracts two 'Array' objects
--
-- @
-- >>> (scalar @Int 1 \`sub\` scalar @Int 1) True
-- @
--
sub
  :: AFType a
  => Array a
  -- ^ First input
  -> Array a
  -- ^ Second input
  -> Bool
  -- ^ Use batch
  -> Array a
  -- ^ Result of sub
sub x y (fromIntegral . fromEnum -> batch) = do
  x `op2` y $ \arr arr1 arr2 ->
    af_sub arr arr1 arr2 batch

-- | Multiply two 'Array' objects
--
-- @
-- >>> (scalar @Int 1 \`mul\` scalar @Int 1) True
-- @
--
mul
  :: AFType a
  => Array a
  -- ^ First input
  -> Array a
  -- ^ Second input
  -> Bool
  -- ^ Use batch
  -> Array a
  -- ^ Result of mul
mul x y (fromIntegral . fromEnum -> batch) = do
  x `op2` y $ \arr arr1 arr2 ->
    af_mul arr arr1 arr2 batch

-- | Divide two 'Array' objects
--
-- @
-- >>> (scalar @Int 1 \`mul\` scalar @Int 1) True
-- @
--
div
  :: AFType a
  => Array a
  -- ^ First input
  -> Array a
  -- ^ Second input
  -> Bool
  -- ^ Use batch
  -> Array a
  -- ^ Result of div
div x y (fromIntegral . fromEnum -> batch) = do
  x `op2` y $ \arr arr1 arr2 ->
    af_div arr arr1 arr2 batch

-- | Test if on 'Array' is less than another 'Array'
--
-- @
-- >>> (scalar \@Int 1 \`lt\` scalar \@Int 1) True
-- @
--
lt
  :: AFType a
  => Array a
  -- ^ First input
  -> Array a
  -- ^ Second input
  -> Bool
  -- ^ Use batch
  -> Array a
  -- ^ Result of less than
lt x y (fromIntegral . fromEnum -> batch) = do
  x `op2` y $ \arr arr1 arr2 ->
    af_lt arr arr1 arr2 batch

-- | Test if on 'Array' is less greater another 'Array'
--
-- @
-- >>> (scalar \@Int 1 \`gt\` scalar \@Int 1) True
-- @
--
gt
  :: AFType a
  => Array a
  -- ^ First input
  -> Array a
  -- ^ Second input
  -> Bool
  -- ^ Use batch
  -> Array a
  -- ^ Result of gt
gt x y (fromIntegral . fromEnum -> batch) = do
  x `op2` y $ \arr arr1 arr2 ->
    af_gt arr arr1 arr2 batch

-- | Test if on 'Array' is less than or equal to another 'Array'
--
-- @
-- >>> (scalar \@Int 1 \`le\` scalar \@Int 1) True
-- @
le
  :: AFType a
  => Array a
  -- ^ First input
  -> Array a
  -- ^ Second input
  -> Bool
  -- ^ Use batch
  -> Array a
  -- ^ Result of less than or equal
le x y (fromIntegral . fromEnum -> batch) = do
  x `op2` y $ \arr arr1 arr2 ->
    af_le arr arr1 arr2 batch

-- | Test if on 'Array' is greater than or equal to another 'Array'
--
-- @
-- >>> (scalar \@Int 1 \`ge\` scalar \@Int 1) True
-- @
ge
  :: AFType a
  => Array a
  -- ^ First input
  -> Array a
  -- ^ Second input
  -> Bool
  -- ^ Use batch
  -> Array a
  -- ^ Result of greater than or equal
ge x y (fromIntegral . fromEnum -> batch) = do
  x `op2` y $ \arr arr1 arr2 ->
    af_ge arr arr1 arr2 batch

-- | Test if on 'Array' is equal to another 'Array'
--
-- @
-- >>> (scalar \@Int 1 \`eq\` scalar \@Int 1) True
-- @
eq
  :: AFType a
  => Array a
  -- ^ First input
  -> Array a
  -- ^ Second input
  -> Bool
  -- ^ Use batch
  -> Array a
  -- ^ Result of equal
eq x y (fromIntegral . fromEnum -> batch) = do
  x `op2` y $ \arr arr1 arr2 ->
    af_eq arr arr1 arr2 batch

-- | Test if on 'Array' is not equal to another 'Array'
--
-- @
-- >>> (scalar \@Int 1 \`neq\` scalar \@Int 1) True
-- @
neq
  :: AFType a
  => Array a
  -- ^ First input
  -> Array a
  -- ^ Second input
  -> Bool
  -- ^ Use batch
  -> Array a
  -- ^ Result of not equal
neq x y (fromIntegral . fromEnum -> batch) = do
  x `op2` y $ \arr arr1 arr2 ->
    af_neq arr arr1 arr2 batch

-- | Logical 'and' one 'Array' with another
--
-- @
-- >>> (scalar \@Int 1 \`and\` scalar \@Int 1) True
-- @
and
  :: AFType a
  => Array a
  -- ^ First input
  -> Array a
  -- ^ Second input
  -> Bool
  -- ^ Use batch
  -> Array a
  -- ^ Result of and
and x y (fromIntegral . fromEnum -> batch) = do
  x `op2` y $ \arr arr1 arr2 ->
    af_and arr arr1 arr2 batch

-- | Logical 'or' one 'Array' with another
--
-- @
-- >>> ('scalar' \@'Int' 1 \`or\` 'scalar' \@'Int' 1) 'True'
-- @
or
  :: AFType a
  => Array a
  -- ^ First input
  -> Array a
  -- ^ Second input
  -> Bool
  -- ^ Use batch
  -> Array a
  -- ^ Result of or
or x y (fromIntegral . fromEnum -> batch) = do
  x `op2` y $ \arr arr1 arr2 ->
    af_or arr arr1 arr2 batch

-- | Not the values of an 'Array'
--
-- @
-- >>> not ('scalar' \@'Int' 1)
-- @
not
  :: AFType a
  => Array a
  -- ^ Input 'Array'
  -> Array a
  -- ^ Result of 'not' on an 'Array'
not = flip op1 af_not

-- | Bitwise and the values in one 'Array' against another 'Array'
--
-- @
-- >>> 'bitAnd' ('scalar' \@'Int' 1) ('scalar' \@'Int' 1) 'False'
-- @
bitAnd
  :: AFType a
  => Array a
  -- ^ First input
  -> Array a
  -- ^ Second input
  -> Bool
  -- ^ Use batch
  -> Array a
  -- ^ Result of bitwise and
bitAnd x y (fromIntegral . fromEnum -> batch) = do
  x `op2` y $ \arr arr1 arr2 ->
    af_bitand arr arr1 arr2 batch

-- | Bitwise or the values in one 'Array' against another 'Array'
--
-- @
-- >>> 'bitOr' ('scalar' \@'Int' 1) ('scalar' \@'Int' 1) 'False'
-- @
bitOr
  :: AFType a
  => Array a
  -- ^ First input
  -> Array a
  -- ^ Second input
  -> Bool
  -- ^ Use batch
  -> Array a
  -- ^ Result of bit or
bitOr x y (fromIntegral . fromEnum -> batch) = do
  x `op2` y $ \arr arr1 arr2 ->
    af_bitor arr arr1 arr2 batch

-- | Bitwise xor the values in one 'Array' against another 'Array'
--
-- @
-- >>> 'bitXor' ('scalar' \@'Int' 1) ('scalar' \@'Int' 1) 'False'
-- @
bitXor
  :: AFType a
  => Array a
  -- ^ First input
  -> Array a
  -- ^ Second input
  -> Bool
  -- ^ Use batch
  -> Array a
  -- ^ Result of bit xor
bitXor x y (fromIntegral . fromEnum -> batch) = do
  x `op2` y $ \arr arr1 arr2 ->
    af_bitxor arr arr1 arr2 batch

-- | Left bit shift the values in one 'Array' against another 'Array'
--
-- @
-- >>> 'bitShiftL' ('scalar' \@'Int' 1) ('scalar' \@'Int' 1) 'False'
-- @
bitShiftL
  :: AFType a
  => Array a
  -- ^ First input
  -> Array a
  -- ^ Second input
  -> Bool
  -- ^ Use batch
  -> Array a
  -- ^ Result of bit shift left
bitShiftL x y (fromIntegral . fromEnum -> batch) = do
  x `op2` y $ \arr arr1 arr2 ->
    af_bitshiftl arr arr1 arr2 batch

-- | Right bit shift the values in one 'Array' against another 'Array'
--
-- @
-- >>> 'bitShiftR' ('scalar' \@'Int' 1) ('scalar' \@'Int' 1) 'False'
-- @
bitShiftR
  :: AFType a
  => Array a
  -- ^ First input
  -> Array a
  -- ^ Second input
  -> Bool
  -- ^ Use batch
  -> Array a
  -- ^ Result of bit shift right
bitShiftR x y (fromIntegral . fromEnum -> batch) = do
  x `op2` y $ \arr arr1 arr2 ->
    af_bitshiftr arr arr1 arr2 batch

-- | Cast one 'Array' into another
--
-- @
-- >>> 'cast' ('scalar' \@'Int' 1) :: 'Array' 'Double'
-- @
cast
  :: forall a b . (AFType a, AFType b)
  => Array a
  -- ^ Input array to cast
  -> Array b
    -- ^ Result of cast
cast afArr =
  coerce $ afArr `op1` (\x y -> af_cast x y dtyp)
    where
      dtyp = afType (Proxy @ b)

-- | Find the minimum of two 'Array's
--
-- @
-- >>> 'minOf' ('scalar' \@'Int' 1) ('scalar' \@'Int' 0) 'False'
-- @
minOf
  :: AFType a
  => Array a
  -- ^ First input
  -> Array a
  -- ^ Second input
  -> Bool
  -- ^ Use batch
  -> Array a
  -- ^ Result of minimum of
minOf x y (fromIntegral . fromEnum -> batch) = do
  x `op2` y $ \arr arr1 arr2 ->
    af_minof arr arr1 arr2 batch

-- | Find the maximum of two 'Array's
--
-- @
-- >>> 'maxOf' ('scalar' \@'Int' 1) ('scalar' \@'Int' 0) 'False'
-- @
maxOf
  :: AFType a
  => Array a
  -- ^ First input
  -> Array a
  -- ^ Second input
  -> Bool
  -- ^ Use batch
  -> Array a
  -- ^ Result of maximum of
maxOf x y (fromIntegral . fromEnum -> batch) = do
  x `op2` y $ \arr arr1 arr2 ->
    af_maxof arr arr1 arr2 batch

-- | Should take the clamp
--
-- @
-- >>> 'clamp' ('scalar' \@'Int' 2) ('scalar' \@'Int' 1) ('scalar' \@'Int' 3)
-- @
clamp
  :: Array a
  -- ^ First input
  -> Array a
  -- ^ Second input
  -> Array a
  -- ^ Third input
  -> Bool
  -- ^ Use batch
  -> Array a
  -- ^ Result of clamp
clamp a b c (fromIntegral . fromEnum -> batch) =
  op3 a b c $ \arr arr1 arr2 arr3 ->
    af_clamp arr arr1 arr2 arr3 batch

-- | Find the remainder of two 'Array's
--
-- @
-- >>> 'clamp' ('vector' \@'Int' 10 [1..])
-- @
rem
  :: AFType a
  => Array a
  -- ^ First input
  -> Array a
  -- ^ Second input
  -> Bool
  -- ^ Use batch
  -> Array a
  -- ^ Result of remainder
rem x y (fromIntegral . fromEnum -> batch) = do
  x `op2` y $ \arr arr1 arr2 ->
    af_rem arr arr1 arr2 batch

-- | Take the 'mod' of two 'Array's
--
-- @
-- >>> 'clamp' ('vector' \@'Int' 10 [1..])
-- @
mod
  :: AFType a
  => Array a
  -- ^ First input
  -> Array a
  -- ^ Second input
  -> Bool
  -- ^ Use batch
  -> Array a
  -- ^ Result of mod
mod x y (fromIntegral . fromEnum -> batch) = do
  x `op2` y $ \arr arr1 arr2 ->
    af_mod arr arr1 arr2 batch

-- | Take the absolute value of an array
--
-- @
-- >>> 'abs' ('scalar' \@'Int' (-1))
-- @
abs
  :: AFType a
  => Array a
  -- ^ Input array
  -> Array a
  -- ^ Result of calling 'abs'
abs = flip op1 af_abs

-- | Find the maximum of two 'Array's
--
-- @
-- >>> 'clamp' ('vector' \@'Int' 10 [1..])
-- @
arg
  :: AFType a
  => Array a
  -- ^ Input array
  -> Array a
  -- ^ Result of calling 'arg'
arg = flip op1 af_arg

-- | Find the maximum of two 'Array's
--
-- @
-- >>> 'clamp' ('vector' \@'Int' 10 [1..])
-- @
sign
  :: AFType a
  => Array a
  -- ^ Input array
  -> Array a
  -- ^ Result of calling 'sign'
sign = flip op1 af_sign

-- | Find the maximum of two 'Array's
--
-- @
-- >>> 'clamp' ('vector' \@'Int' 10 [1..])
-- @
round
  :: AFType a
  => Array a
  -- ^ Input array
  -> Array a
  -- ^ Result of calling 'round'
round = flip op1 af_round

-- | Find the maximum of two 'Array's
--
-- @
-- >>> 'clamp' ('vector' \@'Int' 10 [1..])
-- @
trunc
  :: AFType a
  => Array a
  -- ^ Input array
  -> Array a
  -- ^ Result of calling 'trunc'
trunc = flip op1 af_trunc

-- | Find the maximum of two 'Array's
--
-- @
-- >>> 'clamp' ('vector' \@'Int' 10 [1..])
-- @
floor
  :: AFType a
  => Array a
  -- ^ Input array
  -> Array a
  -- ^ Result of calling 'floor'
floor = flip op1 af_floor

-- | Find the maximum of two 'Array's
--
-- @
-- >>> 'clamp' ('vector' \@'Int' 10 [1..])
-- @
ceil
  :: AFType a
  => Array a
  -- ^ Input array
  -> Array a
  -- ^ Result of calling 'ceil'
ceil = flip op1 af_ceil

-- | Find the maximum of two 'Array's
--
-- @
-- >>> 'clamp' ('vector' \@'Int' 10 [1..])
-- @
sin
  :: AFType a
  => Array a
  -- ^ Input array
  -> Array a
  -- ^ Result of calling 'sin'
sin = flip op1 af_sin

-- | Find the maximum of two 'Array's
--
-- @
-- >>> 'clamp' ('vector' \@'Int' 10 [1..])
-- @
cos
  :: AFType a
  => Array a
  -- ^ Input array
  -> Array a
  -- ^ Result of calling 'cos'
cos = flip op1 af_cos

-- | Find the maximum of two 'Array's
--
-- @
-- >>> 'clamp' ('vector' \@'Int' 10 [1..])
-- @
tan
  :: AFType a
  => Array a
  -- ^ Input array
  -> Array a
  -- ^ Result of calling 'tan'
tan = flip op1 af_tan

-- | Find the maximum of two 'Array's
--
-- @
-- >>> 'clamp' ('vector' \@'Int' 10 [1..])
-- @
asin
  :: AFType a
  => Array a
  -- ^ Input array
  -> Array a
  -- ^ Result of calling 'asin'
asin = flip op1 af_asin

-- | Find the maximum of two 'Array's
--
-- @
-- >>> 'clamp' ('vector' \@'Int' 10 [1..])
-- @
acos
  :: AFType a
  => Array a
  -- ^ Input array
  -> Array a
  -- ^ Result of calling 'acos'
acos = flip op1 af_acos

-- | Find the maximum of two 'Array's
--
-- @
-- >>> 'clamp' ('vector' \@'Int' 10 [1..])
-- @
atan
  :: AFType a
  => Array a
  -- ^ Input array
  -> Array a
  -- ^ Result of calling 'atan'
atan = flip op1 af_atan

-- | Find the maximum of two 'Array's
--
-- @
-- >>> 'clamp' ('vector' \@'Int' 10 [1..])
-- @
atan2
  :: AFType a
  => Array a
  -- ^ First input
  -> Array a
  -- ^ Second input
  -> Bool
  -- ^ Use batch
  -> Array a
  -- ^ Result of atan2
atan2 x y (fromIntegral . fromEnum -> batch) = do
  x `op2` y $ \arr arr1 arr2 ->
    af_atan2 arr arr1 arr2 batch

-- | Find the maximum of two 'Array's
--
-- @
-- >>> 'clamp' ('vector' \@'Int' 10 [1..])
-- @
cplx2
  :: AFType a
  => Array a
  -- ^ First input
  -> Array a
  -- ^ Second input
  -> Bool
  -- ^ Use batch
  -> Array a
  -- ^ Result of cplx2
cplx2 x y (fromIntegral . fromEnum -> batch) = do
  x `op2` y $ \arr arr1 arr2 ->
    af_cplx2 arr arr1 arr2 batch

-- | Find the maximum of two 'Array's
--
-- @
-- >>> 'clamp' ('vector' \@'Int' 10 [1..])
-- @
cplx
  :: AFType a
  => Array a
  -- ^ Input array
  -> Array a
  -- ^ Result of calling 'atan'
cplx = flip op1 af_cplx

-- | Find the maximum of two 'Array's
--
-- @
-- >>> 'clamp' ('vector' \@'Int' 10 [1..])
-- @
real
  :: AFType a
  => Array a
  -- ^ Input array
  -> Array a
  -- ^ Result of calling 'real'
real = flip op1 af_real

-- | Find the maximum of two 'Array's
--
-- @
-- >>> 'clamp' ('vector' \@'Int' 10 [1..])
-- @
imag
  :: AFType a
  => Array a
  -- ^ Input array
  -> Array a
  -- ^ Result of calling 'imag'
imag = flip op1 af_imag

-- | Find the maximum of two 'Array's
--
-- @
-- >>> 'clamp' ('vector' \@'Int' 10 [1..])
-- @
conjg
  :: AFType a
  => Array a
  -- ^ Input array
  -> Array a
  -- ^ Result of calling 'conjg'
conjg = flip op1 af_conjg

-- | Find the maximum of two 'Array's
--
-- @
-- >>> 'clamp' ('vector' \@'Int' 10 [1..])
-- @
sinh
  :: AFType a
  => Array a
  -- ^ Input array
  -> Array a
  -- ^ Result of calling 'sinh'
sinh = flip op1 af_sinh

-- | Find the maximum of two 'Array's
--
-- @
-- >>> 'clamp' ('vector' \@'Int' 10 [1..])
-- @
cosh
  :: AFType a
  => Array a
  -- ^ Input array
  -> Array a
  -- ^ Result of calling 'cosh'
cosh = flip op1 af_cosh

-- | Find the maximum of two 'Array's
--
-- @
-- >>> 'clamp' ('vector' \@'Int' 10 [1..])
-- @
tanh
  :: AFType a
  => Array a
  -- ^ Input array
  -> Array a
  -- ^ Result of calling 'tanh'
tanh = flip op1 af_tanh

-- | Find the maximum of two 'Array's
--
-- @
-- >>> 'clamp' ('vector' \@'Int' 10 [1..])
-- @
root
  :: AFType a
  => Array a
  -- ^ First input
  -> Array a
  -- ^ Second input
  -> Bool
  -- ^ Use batch
  -> Array a
  -- ^ Result of root
root x y (fromIntegral . fromEnum -> batch) = do
  x `op2` y $ \arr arr1 arr2 ->
    af_root arr arr1 arr2 batch

-- | Find the maximum of two 'Array's
--
-- @
-- >>> 'clamp' ('vector' \@'Int' 10 [1..])
-- @
pow
  :: AFType a
  => Array a
  -- ^ First input
  -> Array a
  -- ^ Second input
  -> Bool
  -- ^ Use batch
  -> Array a
  -- ^ Result of pow
pow x y (fromIntegral . fromEnum -> batch) = do
  x `op2` y $ \arr arr1 arr2 ->
    af_pow arr arr1 arr2 batch

-- | Find the maximum of two 'Array's
--
-- @
-- >>> 'clamp' ('vector' \@'Int' 10 [1..])
-- @
pow2
  :: AFType a
  => Array a
  -- ^ Input array
  -> Array a
  -- ^ Result of calling 'pow2'
pow2 = flip op1 af_pow2

-- | Find the maximum of two 'Array's
--
-- @
-- >>> 'clamp' ('vector' \@'Int' 10 [1..])
-- @
exp
  :: AFType a
  => Array a
  -- ^ Input array
  -> Array a
  -- ^ Result of calling 'exp'
exp = flip op1 af_exp

-- | Find the maximum of two 'Array's
--
-- @
-- >>> 'clamp' ('vector' \@'Int' 10 [1..])
-- @
sigmoid
  :: AFType a
  => Array a
  -- ^ Input array
  -> Array a
  -- ^ Result of calling 'sigmoid'
sigmoid = flip op1 af_sigmoid

-- | Find the maximum of two 'Array's
--
-- @
-- >>> 'clamp' ('vector' \@'Int' 10 [1..])
-- @
expm1
  :: AFType a
  => Array a
  -- ^ Input array
  -> Array a
  -- ^ Result of calling 'expm1'
expm1 = flip op1 af_expm1

-- | Find the maximum of two 'Array's
--
-- @
-- >>> 'clamp' ('vector' \@'Int' 10 [1..])
-- @
erf
  :: AFType a
  => Array a
  -- ^ Input array
  -> Array a
  -- ^ Result of calling 'erf'
erf = flip op1 af_erf

-- | Find the maximum of two 'Array's
--
-- @
-- >>> 'clamp' ('vector' \@'Int' 10 [1..])
-- @
erfc
  :: AFType a
  => Array a
  -- ^ Input array
  -> Array a
  -- ^ Result of calling 'erfc'
erfc = flip op1 af_erfc

-- | Find the maximum of two 'Array's
--
-- @
-- >>> 'clamp' ('vector' \@'Int' 10 [1..])
-- @
log
  :: AFType a
  => Array a
  -- ^ Input array
  -> Array a
  -- ^ Result of calling 'log'
log = flip op1 af_log

-- | Find the maximum of two 'Array's
--
-- @
-- >>> 'clamp' ('vector' \@'Int' 10 [1..])
-- @
log1p
  :: AFType a
  => Array a
  -- ^ Input array
  -> Array a
  -- ^ Result of calling 'log1p'
log1p = flip op1 af_log1p

-- | Find the maximum of two 'Array's
--
-- @
-- >>> 'clamp' ('vector' \@'Int' 10 [1..])
-- @
log10
  :: AFType a
  => Array a
  -- ^ Input array
  -> Array a
  -- ^ Result of calling 'log10'
log10 = flip op1 af_log10

-- | Find the maximum of two 'Array's
--
-- @
-- >>> 'clamp' ('vector' \@'Int' 10 [1..])
-- @
log2
  :: AFType a
  => Array a
  -- ^ Input array
  -> Array a
  -- ^ Result of calling 'log2'
log2 = flip op1 af_log2

-- | Find the maximum of two 'Array's
--
-- @
-- >>> 'clamp' ('vector' \@'Int' 10 [1..])
-- @
sqrt
  :: AFType a
  => Array a
  -- ^ Input array
  -> Array a
  -- ^ Result of calling 'sqrt'
sqrt = flip op1 af_sqrt

-- | Find the maximum of two 'Array's
--
-- @
-- >>> 'clamp' ('vector' \@'Int' 10 [1..])
-- @
cbrt
  :: AFType a
  => Array a
  -- ^ Input array
  -> Array a
  -- ^ Result of calling 'cbrt'
cbrt = flip op1 af_cbrt

-- | Find the maximum of two 'Array's
--
-- @
-- >>> 'clamp' ('vector' \@'Int' 10 [1..])
-- @
factorial
  :: AFType a
  => Array a
  -- ^ Input array
  -> Array a
  -- ^ Result of calling 'factorial'
factorial = flip op1 af_factorial

-- | Find the maximum of two 'Array's
--
-- @
-- >>> 'clamp' ('vector' \@'Int' 10 [1..])
-- @
tgamma
  :: AFType a
  => Array a
  -- ^ Input array
  -> Array a
  -- ^ Result of calling 'tgamma'
tgamma = flip op1 af_tgamma

-- | Find the maximum of two 'Array's
--
-- @
-- >>> 'clamp' ('vector' \@'Int' 10 [1..])
-- @
lgamma
  :: AFType a
  => Array a
  -- ^ Input array
  -> Array a
  -- ^ Result of calling 'lgamma'
lgamma = flip op1 af_lgamma

-- | Find the maximum of two 'Array's
--
-- @
-- >>> 'clamp' ('vector' \@'Int' 10 [1..])
-- @
isZero
  :: AFType a
  => Array a
  -- ^ Input array
  -> Array a
  -- ^ Result of calling 'isZero'
isZero = flip op1 af_iszero

-- | Check if an 'Array' has any 'Infinity' values
--
-- @
-- >>> 'isInf' ('scalar' \@'Double' (1 / 0))
-- @
isInf
  :: (Real a, AFType a)
  => Array a
  -- ^ Input array
  -> Array a
  -- ^ will contain 1's where input is Inf or -Inf, and 0 otherwise.
isInf = (`op1` af_isinf)

-- | Check if an 'Array' has any 'NaN' values
--
-- @
-- >>> 'isNaN' ('scalar' \@'Double' ('acos' 2))
-- @
isNaN
  :: forall a. (AFType a, Real a)
  => Array a
  -- ^ Input array
  -> Array a
  -- ^ Will contain 1's where input is NaN, and 0 otherwise.
isNaN = (`op1` af_isnan)
