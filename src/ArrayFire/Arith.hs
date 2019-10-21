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
-- import qualified ArrayFire as A
--
-- main :: IO ()
-- main = print $ A.scalar \@Int 1 \`A.add\` A.scalar \@Int 1
-- -- 2
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
-- >>> A.scalar @Int 1 `A.add` A.scalar @Int 1
-- ArrayFire Array
-- [1 1 1 1]
--         2
add
  :: AFType a
  => Array a
  -- ^ First input
  -> Array a
  -- ^ Second input
  -> Array a
  -- ^ Result of add
add x y =
  x `op2` y $ \arr arr1 arr2 ->
    af_add arr arr1 arr2 1

-- | Adds two 'Array' objects
--
-- >>> (A.scalar @Int 1 `A.addBatched` A.scalar @Int 1) True
-- ArrayFire Array
-- [1 1 1 1]
--         2
addBatched
  :: AFType a
  => Array a
  -- ^ First input
  -> Array a
  -- ^ Second input
  -> Bool
  -- ^ Use batch
  -> Array a
v  -- ^ Result of add
addBatched x y (fromIntegral . fromEnum -> batch) =
  x `op2` y $ \arr arr1 arr2 ->
    af_add arr arr1 arr2 batch

-- | Subtracts two 'Array' objects
--
-- >>> A.scalar @Int 1 `A.sub` A.scalar @Int 1
-- ArrayFire Array
-- [1 1 1 1]
--         0
sub
  :: AFType a
  => Array a
  -- ^ First input
  -> Array a
  -- ^ Second input
  -> Array a
  -- ^ Result of sub
sub x y = do
  x `op2` y $ \arr arr1 arr2 ->
    af_sub arr arr1 arr2 1

-- | Subtracts two 'Array' objects
--
-- >>> (A.scalar @Int 1 `subBatched` A.scalar @Int 1) True
-- ArrayFire Array
-- [1 1 1 1]
--         0
subBatched
  :: AFType a
  => Array a
  -- ^ First input
  -> Array a
  -- ^ Second input
  -> Bool
  -- ^ Use batch
  -> Array a
  -- ^ Result of sub
subBatched x y (fromIntegral . fromEnum -> batch) = do
  x `op2` y $ \arr arr1 arr2 ->
    af_sub arr arr1 arr2 batch

-- | Multiply two 'Array' objects
--
-- >>> A.scalar @Int 2 `mul` A.scalar @Int 2
-- ArrayFire Array
-- [1 1 1 1]
--         4
mul
  :: AFType a
  => Array a
  -- ^ First input
  -> Array a
  -- ^ Second input
  -> Array a
  -- ^ Result of mul
mul x y = do
  x `op2` y $ \arr arr1 arr2 ->
    af_mul arr arr1 arr2 1

-- | Multiply two 'Array' objects
--
--
-- >>> (A.scalar @Int 2 `mulBatched` A.scalar @Int 2) True
-- ArrayFire Array
-- [1 1 1 1]
--         4
mulBatched
  :: AFType a
  => Array a
  -- ^ First input
  -> Array a
  -- ^ Second input
  -> Bool
  -- ^ Use batch
  -> Array a
  -- ^ Result of mul
mulBatched x y (fromIntegral . fromEnum -> batch) = do
  x `op2` y $ \arr arr1 arr2 ->
    af_mul arr arr1 arr2 batch

-- | Divide two 'Array' objects
--
-- >>> A.scalar @Int 6 `A.div` A.scalar @Int 3
-- ArrayFire Array
-- [1 1 1 1]
--         2
div
  :: AFType a
  => Array a
  -- ^ First input
  -> Array a
  -- ^ Second input
  -> Array a
  -- ^ Result of div
div x y = do
  x `op2` y $ \arr arr1 arr2 ->
    af_div arr arr1 arr2 1

-- | Divide two 'Array' objects
--
-- >>> (A.scalar @Int 6 `A.divBatched` A.scalar @Int 3) True
-- ArrayFire Array
-- [1 1 1 1]
--         2
divBatched
  :: AFType a
  => Array a
  -- ^ First input
  -> Array a
  -- ^ Second input
  -> Bool
  -- ^ Use batch
  -> Array a
  -- ^ Result of div
divBatched x y (fromIntegral . fromEnum -> batch) = do
  x `op2` y $ \arr arr1 arr2 ->
    af_div arr arr1 arr2 batch

-- | Test if on 'Array' is less than another 'Array'
--
-- >>> A.scalar @Int 1 `A.lt` A.scalar @Int 1
-- ArrayFire Array
-- [1 1 1 1]
--         0
-- >>> A.scalar @Int 1 < A.scalar @Int 1
-- False
lt
  :: AFType a
  => Array a
  -- ^ First input
  -> Array a
  -- ^ Second input
  -> Array a
  -- ^ Result of less than
lt x y = do
  x `op2` y $ \arr arr1 arr2 ->
    af_lt arr arr1 arr2 1

-- | Test if on 'Array' is less than another 'Array'
--
-- >>> (A.scalar @Int 1 `A.ltBatched` A.scalar @Int 1) True
-- ArrayFire Array
-- [1 1 1 1]
--         0
-- >>> A.scalar @Int 1 < A.scalar @Int 1
-- False
ltBatched
  :: AFType a
  => Array a
  -- ^ First input
  -> Array a
  -- ^ Second input
  -> Bool
  -- ^ Use batch
  -> Array a
  -- ^ Result of less than
ltBatched x y (fromIntegral . fromEnum -> batch) = do
  x `op2` y $ \arr arr1 arr2 ->
    af_lt arr arr1 arr2 batch

-- | Test if an 'Array' is greater than another 'Array'
--
-- >>> A.scalar @Int 1 `A.gt` A.scalar @Int 1
-- ArrayFire Array
-- [1 1 1 1]
--         0
-- >>> A.scalar @Int 1 > A.scalar @Int 2
-- False
gt
  :: AFType a
  => Array a
  -- ^ First input
  -> Array a
  -- ^ Second input
  -> Array a
  -- ^ Result of gt
gt x y = do
  x `op2` y $ \arr arr1 arr2 ->
    af_gt arr arr1 arr2 1

-- | Test if an 'Array' is greater than another 'Array'
--
-- >>> (A.scalar @Int 1 `gtBatched` A.scalar @Int 1) True
-- False
gtBatched
  :: AFType a
  => Array a
  -- ^ First input
  -> Array a
  -- ^ Second input
  -> Bool
  -- ^ Use batch
  -> Array a
  -- ^ Result of gt
gtBatched x y (fromIntegral . fromEnum -> batch) = do
  x `op2` y $ \arr arr1 arr2 ->
    af_gt arr arr1 arr2 batch

-- | Test if one 'Array' is less than or equal to another 'Array'
--
-- >>> A.scalar @Int 1 `A.le` A.scalar @Int 1
-- ArrayFire Array
-- [1 1 1 1]
--         1
-- >>> A.scalar @Int 1 < A.scalar @Int 1
-- False
le
  :: AFType a
  => Array a
  -- ^ First input
  -> Array a
  -- ^ Second input
  -> Array a
  -- ^ Result of less than or equal
le x y = do
  x `op2` y $ \arr arr1 arr2 ->
    af_le arr arr1 arr2 1

-- | Test if one 'Array' is less than or equal to another 'Array'
--
-- >>> (A.scalar @Int 1 `A.leBatched` A.scalar @Int 1) True
-- ArrayFire Array
-- [1 1 1 1]
--         1
-- >>> A.scalar @Int 1 <= A.scalar @Int 1
-- False
leBatched
  :: AFType a
  => Array a
  -- ^ First input
  -> Array a
  -- ^ Second input
  -> Bool
  -- ^ Use batch
  -> Array a
  -- ^ Result of less than or equal
leBatched x y (fromIntegral . fromEnum -> batch) = do
  x `op2` y $ \arr arr1 arr2 ->
    af_le arr arr1 arr2 batch

-- | Test if one 'Array' is greater than or equal to another 'Array'
--
--
-- >>> (A.scalar @Int 1 `A.ge` A.scalar @Int 1) True
--
ge
  :: AFType a
  => Array a
  -- ^ First input
  -> Array a
  -- ^ Second input
  -> Array a
  -- ^ Result of greater than or equal
ge x y = do
  x `op2` y $ \arr arr1 arr2 ->
    af_ge arr arr1 arr2 1

-- | Test if one 'Array' is greater than or equal to another 'Array'
--
--
-- >>> (A.scalar @Int 1 `A.geBatched` A.scalar @Int 1) True
--
geBatched
  :: AFType a
  => Array a
  -- ^ First input
  -> Array a
  -- ^ Second input
  -> Bool
  -- ^ Use batch
  -> Array a
  -- ^ Result of greater than or equal
geBatched x y (fromIntegral . fromEnum -> batch) = do
  x `op2` y $ \arr arr1 arr2 ->
    af_ge arr arr1 arr2 batch

-- | Test if one 'Array' is equal to another 'Array'
--
--
-- >>> A.scalar @Int 1 `A.eq` A.scalar @Int 1
--
eq
  :: AFType a
  => Array a
  -- ^ First input
  -> Array a
  -- ^ Second input
  -> Array a
  -- ^ Result of equal
eq x y = do
  x `op2` y $ \arr arr1 arr2 ->
    af_eq arr arr1 arr2 1

-- | Test if one 'Array' is equal to another 'Array'
--
--
-- >>> (A.scalar @Int 1 `A.eqBatched` A.scalar @Int 1) True
--
eqBatched
  :: AFType a
  => Array a
  -- ^ First input
  -> Array a
  -- ^ Second input
  -> Bool
  -- ^ Use batch
  -> Array a
  -- ^ Result of equal
eqBatched x y (fromIntegral . fromEnum -> batch) =
  x `op2` y $ \arr arr1 arr2 ->
    af_eq arr arr1 arr2 batch

-- | Test if one 'Array' is not equal to another 'Array'
--
-- >>> A.scalar @Int 1 `A.neq` A.scalar @Int 1
-- ArrayFire Array
--[1 1 1 1]
--         0
-- >>> A.scalar @Int 1 /= A.scalar @Int 1
-- False
neq
  :: AFType a
  => Array a
  -- ^ First input
  -> Array a
  -- ^ Second input
  -> Array a
  -- ^ Result of not equal
neq x y =
  x `op2` y $ \arr arr1 arr2 ->
    af_neq arr arr1 arr2 1

-- | Test if one 'Array' is not equal to another 'Array'
--
--
-- >>> (A.scalar @Int 1 `neqBatched` A.scalar @Int 1) True
--
neqBatched
  :: AFType a
  => Array a
  -- ^ First input
  -> Array a
  -- ^ Second input
  -> Bool
  -- ^ Use batch
  -> Array a
  -- ^ Result of not equal
neqBatched x y (fromIntegral . fromEnum -> batch) = do
  x `op2` y $ \arr arr1 arr2 ->
    af_neq arr arr1 arr2 batch

-- | Logical 'and' one 'Array' with another
--
--
-- >>> (A.scalar @Int 1 `and` A.scalar @Int 1)
--
and
  :: AFType a
  => Array a
  -- ^ First input
  -> Array a
  -- ^ Second input
  -> Array a
  -- ^ Result of and
and x y =
  x `op2` y $ \arr arr1 arr2 ->
    af_and arr arr1 arr2 1

-- | Logical 'and' one 'Array' with another
--
--
-- >>> (A.scalar @Int 1 `andBatched` A.scalar @Int 1) True
--
andBatched
  :: AFType a
  => Array a
  -- ^ First input
  -> Array a
  -- ^ Second input
  -> Bool
  -- ^ Use batch
  -> Array a
  -- ^ Result of and
andBatched x y (fromIntegral . fromEnum -> batch) = do
  x `op2` y $ \arr arr1 arr2 ->
    af_and arr arr1 arr2 batch

-- | Logical 'or' one 'Array' with another
--
--
-- >>> (A.scalar @Int 1 `or` A.scalar @Int 1)
--
or
  :: AFType a
  => Array a
  -- ^ First input
  -> Array a
  -- ^ Second input
  -> Array a
  -- ^ Result of or
or x y =
  x `op2` y $ \arr arr1 arr2 ->
    af_or arr arr1 arr2 1

-- | Logical 'or' one 'Array' with another
--
--
-- >>> (A.scalar @Int 1 `orBatched` A.scalar @Int 1) 'True'
--
orBatched
  :: AFType a
  => Array a
  -- ^ First input
  -> Array a
  -- ^ Second input
  -> Bool
  -- ^ Use batch
  -> Array a
  -- ^ Result of or
orBatched x y (fromIntegral . fromEnum -> batch) = do
  x `op2` y $ \arr arr1 arr2 ->
    af_or arr arr1 arr2 batch

-- | Not the values of an 'Array'
--
--
-- >>> not (A.scalar @Int 1)
--
not
  :: AFType a
  => Array a
  -- ^ Input 'Array'
  -> Array a
  -- ^ Result of 'not' on an 'Array'
not = flip op1 af_not

-- | Bitwise and the values in one 'Array' against another 'Array'
--
--
-- >>> 'bitAnd' (A.scalar @Int 1) (A.scalar @Int 1)
--
bitAnd
  :: AFType a
  => Array a
  -- ^ First input
  -> Array a
  -- ^ Second input
  -> Array a
  -- ^ Result of bitwise and
bitAnd x y =
  x `op2` y $ \arr arr1 arr2 ->
    af_bitand arr arr1 arr2 1

-- | Bitwise and the values in one 'Array' against another 'Array'
--
--
-- >>> 'bitAndBatched' (A.scalar @Int 1) (A.scalar @Int 1) 'False'
--
bitAndBatched
  :: AFType a
  => Array a
  -- ^ First input
  -> Array a
  -- ^ Second input
  -> Bool
  -- ^ Use batch
  -> Array a
  -- ^ Result of bitwise and
bitAndBatched x y (fromIntegral . fromEnum -> batch) = do
  x `op2` y $ \arr arr1 arr2 ->
    af_bitand arr arr1 arr2 batch

-- | Bitwise or the values in one 'Array' against another 'Array'
--
--
-- >>> 'bitOr' (A.scalar @Int 1) (A.scalar @Int 1)
--
bitOr
  :: AFType a
  => Array a
  -- ^ First input
  -> Array a
  -- ^ Second input
  -> Array a
  -- ^ Result of bit or
bitOr x y = do
  x `op2` y $ \arr arr1 arr2 ->
    af_bitor arr arr1 arr2 1

-- | Bitwise or the values in one 'Array' against another 'Array'
--
--
-- >>> 'bitOrBatched' (A.scalar @Int 1) (A.scalar @Int 1) 'False'
--
bitOrBatched
  :: AFType a
  => Array a
  -- ^ First input
  -> Array a
  -- ^ Second input
  -> Bool
  -- ^ Use batch
  -> Array a
  -- ^ Result of bit or
bitOrBatched x y (fromIntegral . fromEnum -> batch) = do
  x `op2` y $ \arr arr1 arr2 ->
    af_bitor arr arr1 arr2 batch

-- | Bitwise xor the values in one 'Array' against another 'Array'
--
--
-- >>> 'bitXor' (A.scalar @Int 1) (A.scalar @Int 1)
--
bitXor
  :: AFType a
  => Array a
  -- ^ First input
  -> Array a
  -- ^ Second input
  -> Array a
  -- ^ Result of bit xor
bitXor x y = do
  x `op2` y $ \arr arr1 arr2 ->
    af_bitxor arr arr1 arr2 1

-- | Bitwise xor the values in one 'Array' against another 'Array'
--
--
-- >>> 'bitXorBatched' (A.scalar @Int 1) (A.scalar @Int 1) 'False'
--
bitXorBatched
  :: AFType a
  => Array a
  -- ^ First input
  -> Array a
  -- ^ Second input
  -> Bool
  -- ^ Use batch
  -> Array a
  -- ^ Result of bit xor
bitXorBatched x y (fromIntegral . fromEnum -> batch) = do
  x `op2` y $ \arr arr1 arr2 ->
    af_bitxor arr arr1 arr2 batch

-- | Left bit shift the values in one 'Array' against another 'Array'
--
--
-- >>> 'bitShiftL' (A.scalar @Int 1) (A.scalar @Int 1)
--
bitShiftL
  :: AFType a
  => Array a
  -- ^ First input
  -> Array a
  -- ^ Second input
  -> Array a
  -- ^ Result of bit shift left
bitShiftL x y =
  x `op2` y $ \arr arr1 arr2 ->
    af_bitshiftl arr arr1 arr2 1

-- | Left bit shift the values in one 'Array' against another 'Array'
--
--
-- >>> 'bitShiftLBatched' (A.scalar @Int 1) (A.scalar @Int 1) 'False'
--
bitShiftLBatched
  :: AFType a
  => Array a
  -- ^ First input
  -> Array a
  -- ^ Second input
  -> Bool
  -- ^ Use batch
  -> Array a
  -- ^ Result of bit shift left
bitShiftLBatched x y (fromIntegral . fromEnum -> batch) = do
  x `op2` y $ \arr arr1 arr2 ->
    af_bitshiftl arr arr1 arr2 batch


-- | Right bit shift the values in one 'Array' against another 'Array'
--
--
-- >>> 'bitShiftR' (A.scalar @Int 1) (A.scalar @Int 1)
--
bitShiftR
  :: AFType a
  => Array a
  -- ^ First input
  -> Array a
  -- ^ Second input
  -> Array a
  -- ^ Result of bit shift right
bitShiftR x y =
  x `op2` y $ \arr arr1 arr2 ->
    af_bitshiftr arr arr1 arr2 1

-- | Right bit shift the values in one 'Array' against another 'Array'
--
--
-- >>> 'bitShiftRBatched' (A.scalar @Int 1) (A.scalar @Int 1) 'False'
--
bitShiftRBatched
  :: AFType a
  => Array a
  -- ^ First input
  -> Array a
  -- ^ Second input
  -> Bool
  -- ^ Use batch
  -> Array a
  -- ^ Result of bit shift left
bitShiftRBatched x y (fromIntegral . fromEnum -> batch) = do
  x `op2` y $ \arr arr1 arr2 ->
    af_bitshiftr arr arr1 arr2 batch

-- | Cast one 'Array' into another
--
--
-- >>> 'cast' (A.scalar @Int 1) :: 'Array' 'Double'
--
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
--
-- >>> 'minOf' (A.scalar @Int 1) (A.scalar @Int 0)
--
minOf
  :: AFType a
  => Array a
  -- ^ First input
  -> Array a
  -- ^ Second input
  -> Array a
  -- ^ Result of minimum of
minOf x y =
  x `op2` y $ \arr arr1 arr2 ->
    af_minof arr arr1 arr2 1

-- | Find the minimum of two 'Array's
--
--
-- >>> 'minOfBatched' (A.scalar @Int 1) (A.scalar @Int 0) 'False'
--
minOfBatched
  :: AFType a
  => Array a
  -- ^ First input
  -> Array a
  -- ^ Second input
  -> Bool
  -- ^ Use batch
  -> Array a
  -- ^ Result of minimum of
minOfBatched x y (fromIntegral . fromEnum -> batch) = do
  x `op2` y $ \arr arr1 arr2 ->
    af_minof arr arr1 arr2 batch

-- | Find the maximum of two 'Array's
--
--
-- >>> 'maxOf' (A.scalar @Int 1) (A.scalar @Int 0)
--
maxOf
  :: AFType a
  => Array a
  -- ^ First input
  -> Array a
  -- ^ Second input
  -> Array a
  -- ^ Result of maximum of
maxOf x y =
  x `op2` y $ \arr arr1 arr2 ->
    af_maxof arr arr1 arr2 1


-- | Find the maximum of two 'Array's
--
--
-- >>> 'maxOfBatched' (A.scalar @Int 1) (A.scalar @Int 0) 'False'
--
maxOfBatched
  :: AFType a
  => Array a
  -- ^ First input
  -> Array a
  -- ^ Second input
  -> Bool
  -- ^ Use batch
  -> Array a
  -- ^ Result of maximum of
maxOfBatched x y (fromIntegral . fromEnum -> batch) = do
  x `op2` y $ \arr arr1 arr2 ->
    af_maxof arr arr1 arr2 batch

-- | Should take the clamp
--
--
-- >>> 'clamp' (A.scalar @Int 2) (A.scalar @Int 1) (A.scalar @Int 3)
--
clamp
  :: Array a
  -- ^ First input
  -> Array a
  -- ^ Second input
  -> Array a
  -- ^ Third input
  -> Array a
  -- ^ Result of clamp
clamp a b c =
  op3 a b c $ \arr arr1 arr2 arr3 ->
    af_clamp arr arr1 arr2 arr3 1

-- | Should take the clamp
--
--
-- >>> 'clamp' (A.scalar @Int 2) (A.scalar @Int 1) (A.scalar @Int 3)
--
clampBatched
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
clampBatched a b c (fromIntegral . fromEnum -> batch) =
  op3 a b c $ \arr arr1 arr2 arr3 ->
    af_clamp arr arr1 arr2 arr3 batch

-- | Find the remainder of two 'Array's
--
--
-- >>> 'rem' (vector @Int 10 [1..])  (vector @Int 10 [1..])
--
rem
  :: AFType a
  => Array a
  -- ^ First input
  -> Array a
  -- ^ Second input
  -> Array a
  -- ^ Result of remainder
rem x y =
  x `op2` y $ \arr arr1 arr2 ->
    af_rem arr arr1 arr2 1

-- | Find the remainder of two 'Array's
--
--
-- >>> 'remBatched' (vector @Int 10 [1..])  (vector @Int 10 [1..]) True
--
remBatched
  :: AFType a
  => Array a
  -- ^ First input
  -> Array a
  -- ^ Second input
  -> Bool
  -- ^ Use batch
  -> Array a
  -- ^ Result of remainder
remBatched x y (fromIntegral . fromEnum -> batch) = do
  x `op2` y $ \arr arr1 arr2 ->
    af_rem arr arr1 arr2 batch

-- | Take the 'mod' of two 'Array's
--
--
-- >>> 'mod' (vector @Int 10 [1..]) (vector @Int 10 [1..])
--
mod
  :: AFType a
  => Array a
  -- ^ First input
  -> Array a
  -- ^ Second input
  -> Array a
  -- ^ Result of mod
mod x y = do
  x `op2` y $ \arr arr1 arr2 ->
    af_mod arr arr1 arr2 1

-- | Take the 'mod' of two 'Array's
--
--
-- >>> 'modBatched' (vector @Int 10 [1..]) (vector @Int 10 [1..]) True
--
modBatched
  :: AFType a
  => Array a
  -- ^ First input
  -> Array a
  -- ^ Second input
  -> Bool
  -- ^ Use batch
  -> Array a
  -- ^ Result of mod
modBatched x y (fromIntegral . fromEnum -> batch) = do
  x `op2` y $ \arr arr1 arr2 ->
    af_mod arr arr1 arr2 batch

-- | Take the absolute value of an array
--
--
-- >>> 'abs' (A.scalar @Int (-1))
--
abs
  :: AFType a
  => Array a
  -- ^ Input array
  -> Array a
  -- ^ Result of calling 'abs'
abs = flip op1 af_abs

-- | Find the arg of an array
--
--
-- >>> 'arg' (vector @Int 10 [1..])
--
arg
  :: AFType a
  => Array a
  -- ^ Input array
  -> Array a
  -- ^ Result of calling 'arg'
arg = flip op1 af_arg

-- | Find the sign of two 'Array's
--
--
-- >>> 'sign' (vector @Int 10 [1..])
--
sign
  :: AFType a
  => Array a
  -- ^ Input array
  -> Array a
  -- ^ Result of calling 'sign'
sign = flip op1 af_sign

-- | Round the values in an 'Array'
--
--
-- >>> 'round' (vector @Int 10 [1..])
--
round
  :: AFType a
  => Array a
  -- ^ Input array
  -> Array a
  -- ^ Result of calling 'round'
round = flip op1 af_round

-- | Truncate the values of an 'Array'
--
--
-- >>> 'trunc' (vector @Int 10 [1..])
--
trunc
  :: AFType a
  => Array a
  -- ^ Input array
  -> Array a
  -- ^ Result of calling 'trunc'
trunc = flip op1 af_trunc

-- | Take the floor of all values in an 'Array'
--
--
-- >>> 'floor' (vector @Int 10 [1..])
--
floor
  :: AFType a
  => Array a
  -- ^ Input array
  -> Array a
  -- ^ Result of calling 'floor'
floor = flip op1 af_floor

-- | Take the ceil of all values in an 'Array'
--
--
-- >>> 'ceil' (vector @Int 10 [1..])
--
ceil
  :: AFType a
  => Array a
  -- ^ Input array
  -> Array a
  -- ^ Result of calling 'ceil'
ceil = flip op1 af_ceil

-- | Take the sin of all values in an 'Array'
--
--
-- >>> 'sin' (vector @Int 10 [1..])
--
sin
  :: AFType a
  => Array a
  -- ^ Input array
  -> Array a
  -- ^ Result of calling 'sin'
sin = flip op1 af_sin

-- | Take the cos of all values in an 'Array'
--
--
-- >>> 'cos' (vector @Int 10 [1..])
--
cos
  :: AFType a
  => Array a
  -- ^ Input array
  -> Array a
  -- ^ Result of calling 'cos'
cos = flip op1 af_cos

-- | Take the tan of all values in an 'Array'
--
--
-- >>> 'tan' (vector @Int 10 [1..])
--
tan
  :: AFType a
  => Array a
  -- ^ Input array
  -> Array a
  -- ^ Result of calling 'tan'
tan = flip op1 af_tan

-- | Take the asin of all values in an 'Array'
--
--
-- >>> 'asin' (vector @Int 10 [1..])
--
asin
  :: AFType a
  => Array a
  -- ^ Input array
  -> Array a
  -- ^ Result of calling 'asin'
asin = flip op1 af_asin

-- | Take the acos of all values in an 'Array'
--
--
-- >>> 'acos' (vector @Int 10 [1..])
--
acos
  :: AFType a
  => Array a
  -- ^ Input array
  -> Array a
  -- ^ Result of calling 'acos'
acos = flip op1 af_acos

-- | Take the atan of all values in an 'Array'
--
--
-- >>> 'atan' (vector @Int 10 [1..])
--
atan
  :: AFType a
  => Array a
  -- ^ Input array
  -> Array a
  -- ^ Result of calling 'atan'
atan = flip op1 af_atan

-- | Take the atan2 of all values in an 'Array'
--
--
-- >>> 'atan2' (vector @Int 10 [1..]) (vector @Int 10 [1..])
--
atan2
  :: AFType a
  => Array a
  -- ^ First input
  -> Array a
  -- ^ Second input
  -> Array a
  -- ^ Result of atan2
atan2 x y =
  x `op2` y $ \arr arr1 arr2 ->
    af_atan2 arr arr1 arr2 1

-- | Take the atan2 of all values in an 'Array'
--
--
-- >>> 'atan2Batched' (vector @Int 10 [1..]) (vector @Int 10 [1..]) True
--
atan2Batched
  :: AFType a
  => Array a
  -- ^ First input
  -> Array a
  -- ^ Second input
  -> Bool
  -- ^ Use batch
  -> Array a
  -- ^ Result of atan2
atan2Batched x y (fromIntegral . fromEnum -> batch) = do
  x `op2` y $ \arr arr1 arr2 ->
    af_atan2 arr arr1 arr2 batch

-- | Take the cplx2 of all values in an 'Array'
--
--
-- >>> 'cplx2' (vector @Int 10 [1..]) (vector @Int 10 [1..])
--
cplx2
  :: AFType a
  => Array a
  -- ^ First input
  -> Array a
  -- ^ Second input
  -> Array a
  -- ^ Result of cplx2
cplx2 x y =
  x `op2` y $ \arr arr1 arr2 ->
    af_cplx2 arr arr1 arr2 1

-- | Take the cplx2Batched of all values in an 'Array'
--
--
-- >>> 'cplx2Batched' (vector @Int 10 [1..]) (vector @Int 10 [1..]) True
--
cplx2Batched
  :: AFType a
  => Array a
  -- ^ First input
  -> Array a
  -- ^ Second input
  -> Bool
  -- ^ Use batch
  -> Array a
  -- ^ Result of cplx2
cplx2Batched x y (fromIntegral . fromEnum -> batch) = do
  x `op2` y $ \arr arr1 arr2 ->
    af_cplx2 arr arr1 arr2 batch

-- | Execute cplx
--
--
-- >>> 'cplx' (vector @Int 10 [1..])
--
cplx
  :: AFType a
  => Array a
  -- ^ Input array
  -> Array a
  -- ^ Result of calling 'atan'
cplx = flip op1 af_cplx

-- | Execute real
--
--
-- >>> 'real' (vector @Int 10 [1..])
--
real
  :: AFType a
  => Array a
  -- ^ Input array
  -> Array a
  -- ^ Result of calling 'real'
real = flip op1 af_real

-- | Execute imag
--
--
-- >>> 'imag' (vector @Int 10 [1..])
--
imag
  :: AFType a
  => Array a
  -- ^ Input array
  -> Array a
  -- ^ Result of calling 'imag'
imag = flip op1 af_imag

-- | Execute conjg
--
--
-- >>> 'conjg' (vector @Int 10 [1..])
--
conjg
  :: AFType a
  => Array a
  -- ^ Input array
  -> Array a
  -- ^ Result of calling 'conjg'
conjg = flip op1 af_conjg

-- | Execute sinh
--
--
-- >>> 'sinh' (vector @Int 10 [1..])
--
sinh
  :: AFType a
  => Array a
  -- ^ Input array
  -> Array a
  -- ^ Result of calling 'sinh'
sinh = flip op1 af_sinh

-- | Execute cosh
--
--
-- >>> 'cosh' (vector @Int 10 [1..])
--
cosh
  :: AFType a
  => Array a
  -- ^ Input array
  -> Array a
  -- ^ Result of calling 'cosh'
cosh = flip op1 af_cosh

-- | Execute tanh
--
--
-- >>> 'tanh' (vector @Int 10 [1..])
--
tanh
  :: AFType a
  => Array a
  -- ^ Input array
  -> Array a
  -- ^ Result of calling 'tanh'
tanh = flip op1 af_tanh

-- | Execute root
--
--
-- >>> 'root' (vector @Int 10 [1..]) (vector @Int 10 [1..])
--
root
  :: AFType a
  => Array a
  -- ^ First input
  -> Array a
  -- ^ Second input
  -> Array a
  -- ^ Result of root
root x y =
  x `op2` y $ \arr arr1 arr2 ->
    af_root arr arr1 arr2 1

-- | Execute rootBatched
--
--
-- >>> 'rootBatched' (vector @Int 10 [1..]) (vector @Int 10 [1..]) True
--
rootBatched
  :: AFType a
  => Array a
  -- ^ First input
  -> Array a
  -- ^ Second input
  -> Bool
  -- ^ Use batch
  -> Array a
  -- ^ Result of root
rootBatched x y (fromIntegral . fromEnum -> batch) = do
  x `op2` y $ \arr arr1 arr2 ->
    af_root arr arr1 arr2 batch

-- | Execute pow
--
--
-- >>> 'pow' (vector @Int 10 [1..]) 2
--
pow
  :: AFType a
  => Array a
  -- ^ First input
  -> Array a
  -- ^ Second input
  -> Array a
  -- ^ Result of pow
pow x y =
  x `op2` y $ \arr arr1 arr2 ->
    af_pow arr arr1 arr2 1


-- | Execute powBatched
--
--
-- >>> 'powBatched' (vector @Int 10 [1..]) ('constant' @Int [1] 2) True
--
powBatched
  :: AFType a
  => Array a
  -- ^ First input
  -> Array a
  -- ^ Second input
  -> Bool
  -- ^ Use batch
  -> Array a
  -- ^ Result of powBatched
powBatched x y (fromIntegral . fromEnum -> batch) = do
  x `op2` y $ \arr arr1 arr2 ->
    af_pow arr arr1 arr2 batch

-- | Raise an 'Array' to the second power
--
--
-- >>> 'pow2' (vector @Int 10 [1..])
--
pow2
  :: AFType a
  => Array a
  -- ^ Input array
  -> Array a
  -- ^ Result of calling 'pow2'
pow2 = flip op1 af_pow2

-- | Execute exp on 'Array'
--
--
-- >>> 'exp' (vector @Int 10 [1..])
--
exp
  :: AFType a
  => Array a
  -- ^ Input array
  -> Array a
  -- ^ Result of calling 'exp'
exp = flip op1 af_exp

-- | Execute sigmoid on 'Array'
--
--
-- >>> 'sigmoid' (vector @Int 10 [1..])
--
sigmoid
  :: AFType a
  => Array a
  -- ^ Input array
  -> Array a
  -- ^ Result of calling 'sigmoid'
sigmoid = flip op1 af_sigmoid

-- | Execute expm1
--
--
-- >>> 'expm1' (vector @Int 10 [1..])
--
expm1
  :: AFType a
  => Array a
  -- ^ Input array
  -> Array a
  -- ^ Result of calling 'expm1'
expm1 = flip op1 af_expm1

-- | Execute erf
--
--
-- >>> 'erf' (vector @Int 10 [1..])
--
erf
  :: AFType a
  => Array a
  -- ^ Input array
  -> Array a
  -- ^ Result of calling 'erf'
erf = flip op1 af_erf

-- | Execute erfc
--
--
-- >>> 'erfc' (vector @Int 10 [1..])
--
erfc
  :: AFType a
  => Array a
  -- ^ Input array
  -> Array a
  -- ^ Result of calling 'erfc'
erfc = flip op1 af_erfc

-- | Execute log
--
--
-- >>> 'log' (vector @Int 10 [1..])
--
log
  :: AFType a
  => Array a
  -- ^ Input array
  -> Array a
  -- ^ Result of calling 'log'
log = flip op1 af_log

-- | Execute log1p
--
--
-- >>> 'log1p' (vector @Int 10 [1..])
--
log1p
  :: AFType a
  => Array a
  -- ^ Input array
  -> Array a
  -- ^ Result of calling 'log1p'
log1p = flip op1 af_log1p

-- | Execute log10
--
--
-- >>> 'log10' (vector @Int 10 [1..])
--
log10
  :: AFType a
  => Array a
  -- ^ Input array
  -> Array a
  -- ^ Result of calling 'log10'
log10 = flip op1 af_log10

-- | Execute log2
--
--
-- >>> 'log2' (vector @Int 10 [1..])
--
log2
  :: AFType a
  => Array a
  -- ^ Input array
  -> Array a
  -- ^ Result of calling 'log2'
log2 = flip op1 af_log2

-- | Execute sqrt
--
--
-- >>> 'sqrt' (vector @Int 10 [1..])
--
sqrt
  :: AFType a
  => Array a
  -- ^ Input array
  -> Array a
  -- ^ Result of calling 'sqrt'
sqrt = flip op1 af_sqrt

-- | Execute cbrt
--
--
-- >>> 'cbrt' (vector @Int 10 [1..])
--
cbrt
  :: AFType a
  => Array a
  -- ^ Input array
  -> Array a
  -- ^ Result of calling 'cbrt'
cbrt = flip op1 af_cbrt

-- | Execute factorial1
--
--
-- >>> 'factorial1' (vector @Int 10 [1..])
--
factorial
  :: AFType a
  => Array a
  -- ^ Input array
  -> Array a
  -- ^ Result of calling 'factorial'
factorial = flip op1 af_factorial

-- | Execute tgamma
--
--
-- >>> 'tgamma' (vector @Int 10 [1..])
--
tgamma
  :: AFType a
  => Array a
  -- ^ Input array
  -> Array a
  -- ^ Result of calling 'tgamma'
tgamma = flip op1 af_tgamma

-- | Execute lgamma
--
--
-- >>> 'lgamma' (vector @Int 10 [1..])
--
lgamma
  :: AFType a
  => Array a
  -- ^ Input array
  -> Array a
  -- ^ Result of calling 'lgamma'
lgamma = flip op1 af_lgamma

-- | Execute isZero
--
--
-- >>> 'isZero' (vector @Int 10 [1..])
--
isZero
  :: AFType a
  => Array a
  -- ^ Input array
  -> Array a
  -- ^ Result of calling 'isZero'
isZero = (`op1` af_iszero)

-- | Execute isInf
--
--
-- >>> 'isInf' (vector @Int 10 [1..])
--
isInf
  :: (Real a, AFType a)
  => Array a
  -- ^ Input array
  -> Array a
  -- ^ will contain 1's where input is Inf or -Inf, and 0 otherwise.
isInf = (`op1` af_isinf)

-- | Execute isNaN
--
--
-- >>> 'isNaN' (vector @Int 10 [1..])
--
isNaN
  :: forall a. (AFType a, Real a)
  => Array a
  -- ^ Input array
  -> Array a
  -- ^ Will contain 1's where input is NaN, and 0 otherwise.
isNaN = (`op1` af_isnan)
