{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE FlexibleContexts    #-}
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

import Prelude                  (Bool(..), ($), (.), flip, fromEnum, fromIntegral, Real, RealFrac)

import Data.Coerce
import Data.Proxy
import Data.Complex

import ArrayFire.FFI
import ArrayFire.Internal.Arith
import ArrayFire.Internal.Types
import Foreign.C.Types

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
  -- ^ Result of add
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
  -> Array CBool
  -- ^ Result of less than
lt x y = do
  x `op2bool` y $ \arr arr1 arr2 ->
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
  -> Array CBool
  -- ^ Result of less than
ltBatched x y (fromIntegral . fromEnum -> batch) = do
  x `op2bool` y $ \arr arr1 arr2 ->
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
  -> Array CBool
  -- ^ Result of gt
gt x y = do
  x `op2bool` y $ \arr arr1 arr2 ->
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
  -> Array CBool
  -- ^ Result of gt
gtBatched x y (fromIntegral . fromEnum -> batch) = do
  x `op2bool` y $ \arr arr1 arr2 ->
    af_gt arr arr1 arr2 batch

-- | Test if one 'Array' is less than or equal to another 'Array'
--
-- >>> A.scalar @Int 1 `A.le` A.scalar @Int 1
-- ArrayFire Array
-- [1 1 1 1]
--         1
-- >>> A.scalar @Int 1 <= A.scalar @Int 1
-- False
le
  :: AFType a
  => Array a
  -- ^ First input
  -> Array a
  -- ^ Second input
  -> Array CBool
  -- ^ Result of less than or equal
le x y = do
  x `op2bool` y $ \arr arr1 arr2 ->
    af_le arr arr1 arr2 1

-- | Test if one 'Array' is less than or equal to another 'Array'
--
-- >>> (A.scalar @Int 1 `A.leBatched` A.scalar @Int 1) True
-- ArrayFire Array
-- [1 1 1 1]
--         1
-- >>> A.scalar @Int 1 <= A.scalar @Int 1
-- True
leBatched
  :: AFType a
  => Array a
  -- ^ First input
  -> Array a
  -- ^ Second input
  -> Bool
  -- ^ Use batch
  -> Array CBool
  -- ^ Result of less than or equal
leBatched x y (fromIntegral . fromEnum -> batch) = do
  x `op2bool` y $ \arr arr1 arr2 ->
    af_le arr arr1 arr2 batch

-- | Test if one 'Array' is greater than or equal to another 'Array'
--
-- >>> A.scalar @Int 1 `A.ge` A.scalar @Int 1
-- ArrayFire Array
-- [1 1 1 1]
--         1
-- >>> A.scalar @Int 1 >= A.scalar @Int 1
-- True
ge
  :: AFType a
  => Array a
  -- ^ First input
  -> Array a
  -- ^ Second input
  -> Array CBool
  -- ^ Result of greater than or equal
ge x y = do
  x `op2bool` y $ \arr arr1 arr2 ->
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
  -> Array CBool
  -- ^ Result of greater than or equal
geBatched x y (fromIntegral . fromEnum -> batch) = do
  x `op2bool` y $ \arr arr1 arr2 ->
    af_ge arr arr1 arr2 batch

-- | Test if one 'Array' is equal to another 'Array'
--
-- >>> A.scalar @Int 1 `A.eq` A.scalar @Int 1
-- ArrayFire Array
-- [1 1 1 1]
--         1
--
-- >>> A.scalar @Int 1 == A.scalar @Int 1
-- True
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
-- >>> (A.scalar @Int 1 `A.neqBatched` A.scalar @Int 1) True
-- False
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
-- >>> A.scalar @Int 1 `A.and` A.scalar @Int 1
-- ArrayFire Array
-- [1 1 1 1]
--          1
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
-- >>> (A.scalar @Int 1 `andBatched` A.scalar @Int 1) True
-- ArrayFire Array
-- [1 1 1 1]
--         1
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
-- >>> A.scalar @Int 1 `A.or` A.scalar @Int 1
-- ArrayFire Array
-- [1 1 1 1]
--         1
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
-- >>> (A.scalar @Int 1 `A.orBatched` A.scalar @Int 1) True
-- ArrayFire Array
-- [1 1 1 1]
--         1
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
-- >>> A.not (A.scalar @Int 1)
-- ArrayFire Array
-- [1 1 1 1]
--         0
not
  :: AFType a
  => Array a
  -- ^ Input 'Array'
  -> Array a
  -- ^ Result of 'not' on an 'Array'
not = flip op1 af_not

-- | Bitwise and the values in one 'Array' against another 'Array'
--
-- >>> A.bitAnd (A.scalar @Int 1) (A.scalar @Int 1)
-- ArrayFire Array
-- [1 1 1 1]
--         1
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
--- >>> A.bitAndBatched (A.scalar @Int 1) (A.scalar @Int 1) True
-- ArrayFire Array
-- [1 1 1 1]
--         1
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
-- >>> A.bitOr (A.scalar @Int 1) (A.scalar @Int 1)
-- ArrayFire Array
-- [1 1 1 1]
--         1
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
-- >>> A.bitOrBatched (A.scalar @Int 1) (A.scalar @Int 1) False
-- ArrayFire Array
-- [1 1 1 1]
--         1
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
-- >>> A.bitXor (A.scalar @Int 1) (A.scalar @Int 1)
-- ArrayFire Array
-- [1 1 1 1]
--         0
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
-- >>> A.bitXorBatched (A.scalar @Int 1) (A.scalar @Int 1) False
-- ArrayFire Array
-- [1 1 1 1]
--         0
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
-- >>> A.bitShiftL (A.scalar @Int 1) (A.scalar @Int 1)
-- ArrayFire Array
-- [1 1 1 1]
--         2
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
-- >>> A.bitShiftLBatched (A.scalar @Int 1) (A.scalar @Int 1) False
-- ArrayFire Array
-- [1 1 1 1]
--         2
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
-- >>> A.bitShiftR (A.scalar @Int 1) (A.scalar @Int 1)
-- ArrayFire Array
-- [1 1 1 1]
--         0
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
-- >>> A.bitShiftRBatched (A.scalar @Int 1) (A.scalar @Int 1) False
-- ArrayFire Array
-- [1 1 1 1]
--         0
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
-- >>> A.cast (A.scalar @Int 1) :: Array Double
-- ArrayFire Array
-- [1 1 1 1]
--    1.0000
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
-- >>> A.minOf (A.scalar @Int 1) (A.scalar @Int 0)
-- ArrayFire Array
-- [1 1 1 1]
--         0
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
-- >>> A.minOfBatched (A.scalar @Int 1) (A.scalar @Int 0) False
-- ArrayFire Array
-- [1 1 1 1]
--         0
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
-- >>> A.maxOf (A.scalar @Int 1) (A.scalar @Int 0)
-- ArrayFire Array
-- [1 1 1 1]
--         1
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
-- >>> A.maxOfBatched (A.scalar @Int 1) (A.scalar @Int 0) False
-- ArrayFire Array
--[1 1 1 1]
--         1
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
-- >>> clamp (A.scalar @Int 2) (A.scalar @Int 1) (A.scalar @Int 3)
-- ArrayFire Array
-- [1 1 1 1]
--          2
--
clamp
  :: Array a
  -- ^ input
  -> Array a
  -- ^ lower bound
  -> Array a
  -- ^ upper bound
  -> Array a
  -- ^ Result of clamp
clamp a b c =
  op3 a b c $ \arr arr1 arr2 arr3 ->
    af_clamp arr arr1 arr2 arr3 1

-- | Should take the clamp
--
-- >>> (clampBatched (A.scalar @Int 2) (A.scalar @Int 1) (A.scalar @Int 3)) True
-- ArrayFire Array
-- [1 1 1 1]
--          2
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
-- >>> A.rem (A.vector @Int 10 [1..]) (A.vector @Int 10 [1..])
-- ArrayFire Array
-- [10 1 1 1]
--         0          0          0          0          0          0          0          0          0          0
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
-- >>> A.remBatched (A.vector @Int 10 [1..])  (vector @Int 10 [1..]) True
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
-- >>> A.mod (A.vector @Int 10 [1..]) (A.vector @Int 10 [1..])
-- ArrayFire Array
--[10 1 1 1]
--         0          0          0          0          0          0          0          0          0          0
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
-- >>> A.modBatched (vector @Int 10 [1..]) (vector @Int 10 [1..]) True
-- ArrayFire Array
-- [10 1 1 1]
--         0          0          0          0          0          0          0          0          0          0
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
-- >>> A.abs (A.scalar @Int (-1))
-- ArrayFire Array
-- [1 1 1 1]
--    1.0000
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
-- >>> A.arg (vector @Int 10 [1..])
-- ArrayFire Array
-- [10 1 1 1]
--         0          0          0          0          0          0          0          0          0          0
arg
  :: AFType a
  => Array a
  -- ^ Input array
  -> Array a
  -- ^ Result of calling 'arg'
arg = flip op1 af_arg

-- | Find the sign of two 'Array's
--
-- >>> A.sign (vector @Int 10 [1..])
-- ArrayFire Array
-- [10 1 1 1]
--   0.0000     0.0000     0.0000     0.0000     0.0000     0.0000     0.0000     0.0000     0.0000     0.0000
sign
  :: AFType a
  => Array a
  -- ^ Input array
  -> Array a
  -- ^ Result of calling 'sign'
sign = flip op1 af_sign

-- | Round the values in an 'Array'
--
-- >>> A.round (A.vector @Int 10 [1..])
-- ArrayFire Array
-- [10 1 1 1]
--    1.0000     2.0000     3.0000     4.0000     5.0000     6.0000     7.0000     8.0000     9.0000    10.0000
round
  :: AFType a
  => Array a
  -- ^ Input array
  -> Array a
  -- ^ Result of calling 'round'
round = flip op1 af_round

-- | Truncate the values of an 'Array'
--
-- >>> A.trunc (A.vector @Int 10 [1..])
-- ArrayFire Array
-- [10 1 1 1]
--    1.0000     2.0000     3.0000     4.0000     5.0000     6.0000     7.0000     8.0000     9.0000    10.0000
trunc
  :: AFType a
  => Array a
  -- ^ Input array
  -> Array a
  -- ^ Result of calling 'trunc'
trunc = flip op1 af_trunc

-- | Take the floor of all values in an 'Array'
--
-- >>> A.floor (A.vector @Int 10 [10,9..])
-- ArrayFire Array
-- [10 1 1 1]
--    1.0000     2.0000     3.0000     4.0000     5.0000     6.0000     7.0000     8.0000     9.0000    10.0000
floor
  :: AFType a
  => Array a
  -- ^ Input array
  -> Array a
  -- ^ Result of calling 'floor'
floor = flip op1 af_floor

-- | Take the ceil of all values in an 'Array'
--
-- >>> A.ceil (A.vector @Int 10 [1..])
-- ArrayFire Array
-- [10 1 1 1]
--    1.0000     2.0000     3.0000     4.0000     5.0000     6.0000     7.0000     8.0000     9.0000    10.0000
ceil
  :: AFType a
  => Array a
  -- ^ Input array
  -> Array a
  -- ^ Result of calling 'ceil'
ceil = flip op1 af_ceil

-- | Take the sin of all values in an 'Array'
--
-- >>> A.sin (A.vector @Int 10 [1..])
-- ArrayFire Array
-- [10 1 1 1]
--    0.8415     0.9093     0.1411    -0.7568    -0.9589    -0.2794     0.6570     0.9894     0.4121    -0.5440
sin
  :: AFType a
  => Array a
  -- ^ Input array
  -> Array a
  -- ^ Result of calling 'sin'
sin = flip op1 af_sin

-- | Take the cos of all values in an 'Array'
--
-- >>> A.cos (A.vector @Int 10 [1..])
-- ArrayFire Array
-- [10 1 1 1]
--    0.5403    -0.4161    -0.9900    -0.6536     0.2837     0.9602     0.7539    -0.1455    -0.9111    -0.8391
cos
  :: AFType a
  => Array a
  -- ^ Input array
  -> Array a
  -- ^ Result of calling 'cos'
cos = flip op1 af_cos

-- | Take the tan of all values in an 'Array'
--
-- >>> A.tan (A.vector @Int 10 [1..])
-- ArrayFire Array
-- [10 1 1 1]
--    1.5574    -2.1850    -0.1425     1.1578    -3.3805    -0.2910     0.8714    -6.7997    -0.4523     0.6484
tan
  :: AFType a
  => Array a
  -- ^ Input array
  -> Array a
  -- ^ Result of calling 'tan'
tan = flip op1 af_tan

-- | Take the asin of all values in an 'Array'
--
-- >>> A.asin (A.vector @Int 10 [1..])
-- ArrayFire Array
-- [10 1 1 1]
--    1.5708        nan        nan        nan        nan        nan        nan        nan        nan        nan
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
-- >>> A.acos (A.vector @Int 10 [1..])
-- ArrayFire Array
-- [10 1 1 1]
--    0.0000        nan        nan        nan        nan        nan        nan        nan        nan        nan
acos
  :: AFType a
  => Array a
  -- ^ Input array
  -> Array a
  -- ^ Result of calling 'acos'
acos = flip op1 af_acos

-- | Take the atan of all values in an 'Array'
--
-- >>> A.atan (A.vector @Int 10 [1..])
-- ArrayFire Array
-- [10 1 1 1]
--    0.7854     1.1071     1.2490     1.3258     1.3734     1.4056     1.4289     1.4464     1.4601     1.4711
atan
  :: AFType a
  => Array a
  -- ^ Input array
  -> Array a
  -- ^ Result of calling 'atan'
atan = flip op1 af_atan

-- | Take the atan2 of all values in an 'Array'
--
-- >>> A.atan2 (A.vector @Int 10 [1..]) (A.vector @Int 10 [1..])
-- ArrayFire Array
-- [10 1 1 1]
--    0.7854     0.7854     0.7854     0.7854     0.7854     0.7854     0.7854     0.7854     0.7854     0.7854
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
-- >>> A.atan2Batched (A.vector @Int 10 [1..]) (A.vector @Int 10 [1..]) True
-- ArrayFire Array
-- [10 1 1 1]
--    0.7854     0.7854     0.7854     0.7854     0.7854     0.7854     0.7854     0.7854     0.7854     0.7854
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
-- >>> A.cplx2 (A.vector @Int 10 [1..]) (A.vector @Int 10 [1..])
-- ArrayFire Array
-- [10 1 1 1]
--         (1.0000,1.0000)          (2.0000,2.0000)          (3.0000,3.0000)          (4.0000,4.0000)          (5.0000,5.0000)          (6.0000,6.0000)          (7.0000,7.0000)          (8.0000,8.0000)          (9.0000,9.0000)          (10.0000,10.0000)
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
-- >>> A.cplx2Batched (A..vector @Int 10 [1..]) (A.vector @Int 10 [1..]) True
-- ArrayFire Array
-- [10 1 1 1]
--         (1.0000,1.0000)          (2.0000,2.0000)          (3.0000,3.0000)          (4.0000,4.0000)          (5.0000,5.0000)          (6.0000,6.0000)          (7.0000,7.0000)          (8.0000,8.0000)          (9.0000,9.0000)          (10.0000,10.0000)
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
-- >>> A.cplx (A.vector @Int 10 [1..])
-- ArrayFire Array
-- [10 1 1 1]
--         (1.0000,0.0000)          (2.0000,0.0000)          (3.0000,0.0000)          (4.0000,0.0000)          (5.0000,0.0000)          (6.0000,0.0000)          (7.0000,0.0000)          (8.0000,0.0000)          (9.0000,0.0000)          (10.0000,0.0000)
cplx
  :: AFType a
  => Array a
  -- ^ Input array
  -> Array a
  -- ^ Result of calling 'atan'
cplx = flip op1 af_cplx

-- | Execute real
--
-- >>> A.real (A.scalar @(Complex Double) (10 :+ 11)) :: Array Double
-- ArrayFire Array
-- [10 1 1 1]
--    10.0000
real
  :: (AFType a, AFType (Complex b), RealFrac a, RealFrac b)
  => Array (Complex b)
  -- ^ Input array
  -> Array a
  -- ^ Result of calling 'real'
real = flip op1d af_real

-- | Execute imag
--
-- >>> A.imag (A.scalar @(Complex Double) (10 :+ 11)) :: Array Double
-- ArrayFire Array
-- [10 1 1 1]
--    11.0000
imag
  :: (AFType a, AFType (Complex b), RealFrac a, RealFrac b)
  => Array (Complex b)
  -- ^ Input array
  -> Array a
  -- ^ Result of calling 'imag'
imag = flip op1d af_imag

-- | Execute conjg
--
-- >>> A.conjg (A.vector @Double 10 [1..])
-- ArrayFire Array
-- [10 1 1 1]
--    1.0000     2.0000     3.0000     4.0000     5.0000     6.0000     7.0000     8.0000     9.0000    10.0000
conjg
  :: AFType a
  => Array a
  -- ^ Input array
  -> Array a
  -- ^ Result of calling 'conjg'
conjg = flip op1 af_conjg

-- | Execute sinh
--
-- >>> A.sinh (A.vector @Int 10 [1..])
-- ArrayFire Array
-- [10 1 1 1]
--    1.1752     3.6269    10.0179    27.2899    74.2032   201.7132   548.3161  1490.4788  4051.5419 11013.2329
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
-- >>> A.cosh (A.vector @Double 10 [1..])
-- ArrayFire Array
-- [10 1 1 1]
--    1.5431     3.7622    10.0677    27.3082    74.2099   201.7156   548.3170  1490.4792  4051.5420 11013.2329
cosh
  :: AFType a
  => Array a
  -- ^ Input array
  -> Array a
  -- ^ Result of calling 'cosh'
cosh = flip op1 af_cosh

-- | Execute tanh
--
-- >>> A.tanh (A.vector @Int 10 [1..])
-- ArrayFire Array
-- [10 1 1 1]
--    0.7616     0.9640     0.9951     0.9993     0.9999     1.0000     1.0000     1.0000     1.0000     1.0000
tanh
  :: AFType a
  => Array a
  -- ^ Input array
  -> Array a
  -- ^ Result of calling 'tanh'
tanh = flip op1 af_tanh

-- | Execute asinh
--
-- >>> A.asinh (A.vector @Int 10 [1..])
-- ArrayFire Array
-- [10 1 1 1]
--    0.8814     1.4436     1.8184     2.0947     2.3124     2.4918     2.6441     2.7765     2.8934     2.9982 
asinh
  :: AFType a
  => Array a
  -- ^ Input array
  -> Array a
  -- ^ Result of calling 'tanh'
asinh = flip op1 af_asinh

-- | Execute acosh
--
-- >>> A.acosh (A.vector @Double 10 [1..])
-- ArrayFire Array
-- [10 1 1 1]
--    0.0000     1.3170     1.7627     2.0634     2.2924     2.4779     2.6339     2.7687     2.8873     2.9932 
acosh
  :: AFType a
  => Array a
  -- ^ Input array
  -> Array a
  -- ^ Result of calling 'tanh'
acosh = flip op1 af_acosh

-- | Execute atanh
--
-- >>> A.atanh (A.vector @Double 10 [1..])
-- ArrayFire Array
-- [10 1 1 1]
--        inf        nan        nan        nan        nan        nan        nan        nan        nan        nan 
atanh
  :: AFType a
  => Array a
  -- ^ Input array
  -> Array a
  -- ^ Result of calling 'tanh'
atanh = flip op1 af_atanh

-- | Execute root
--
-- >>> A.root (A.vector @Double 10 [1..]) (A.vector @Double 10 [1..])
-- ArrayFire Array
-- [10 1 1 1]
--    1.0000     1.4142     1.4422     1.4142     1.3797     1.3480     1.3205     1.2968     1.2765     1.2589
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
-- >>> A.rootBatched (vector @Double 10 [1..]) (vector @Double 10 [1..]) True
-- ArrayFire Array
-- [10 1 1 1]
--    1.0000     1.4142     1.4422     1.4142     1.3797     1.3480     1.3205     1.2968     1.2765     1.2589
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
-- >>> A.pow (A.vector @Int 10 [1..]) 2
-- ArrayFire Array
-- [10 1 1 1]
--         1          4          9         16         25         36         49         64         81        100
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
-- >>> A.powBatched (A.vector @Int 10 [1..]) (A.constant @Int [1] 2) True
-- ArrayFire Array
-- [10 1 1 1]
--         1          4          9         16         25         36         49         64         81        100
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
-- >>> A.pow2 (A.vector @Int 10 [1..])
-- ArrayFire Array
-- [10 1 1 1]
--         2          4          8         16         32         64        128        256        512       1024
pow2
  :: AFType a
  => Array a
  -- ^ Input array
  -> Array a
  -- ^ Result of calling 'pow2'
pow2 = flip op1 af_pow2

-- | Execute exp on 'Array'
--
-- >>> A.exp (A.vector @Double 10 [1..])
-- ArrayFire Array
-- [10 1 1 1]
--    2.7183     7.3891    20.0855    54.5982   148.4132   403.4288  1096.6332  2980.9580  8103.0839 22026.4658 
exp
  :: AFType a
  => Array a
  -- ^ Input array
  -> Array a
  -- ^ Result of calling 'exp'
exp = flip op1 af_exp

-- | Execute sigmoid on 'Array'
--
-- >>> A.sigmoid (A.vector @Int 10 [1..])
-- ArrayFire Array
-- [10 1 1 1]
--    0.7311     0.8808     0.9526     0.9820     0.9933     0.9975     0.9991     0.9997     0.9999     1.0000
sigmoid
  :: AFType a
  => Array a
  -- ^ Input array
  -> Array a
  -- ^ Result of calling 'sigmoid'
sigmoid = flip op1 af_sigmoid

-- | Execute expm1
--
-- >>> A.expm1 (A.vector @Int 10 [1..])
-- ArrayFire Array
-- [10 1 1 1]
--    1.7183     6.3891    19.0855    53.5981   147.4132   402.4288  1095.6332  2979.9580  8102.0840 22025.4648
expm1
  :: AFType a
  => Array a
  -- ^ Input array
  -> Array a
  -- ^ Result of calling 'expm1'
expm1 = flip op1 af_expm1

-- | Execute erf
--
-- >>> A.erf (A.vector @Int 10 [1..])
-- ArrayFire Array
-- [10 1 1 1]
--    0.8427     0.9953     1.0000     1.0000     1.0000     1.0000     1.0000     1.0000     1.0000     1.0000
erf
  :: AFType a
  => Array a
  -- ^ Input array
  -> Array a
  -- ^ Result of calling 'erf'
erf = flip op1 af_erf

-- | Execute erfc
--
-- >>> A.erfc (A.vector @Int 10 [1..])
-- ArrayFire Array
-- [10 1 1 1]
--    0.1573     0.0047     0.0000     0.0000     0.0000     0.0000     0.0000     0.0000     0.0000     0.0000
erfc
  :: AFType a
  => Array a
  -- ^ Input array
  -> Array a
  -- ^ Result of calling 'erfc'
erfc = flip op1 af_erfc

-- | Execute log
--
-- >>> A.log (A.vector @Int 10 [1..])
-- ArrayFire Array
-- [10 1 1 1]
--    0.0000     0.6931     1.0986     1.3863     1.6094     1.7918     1.9459     2.0794     2.1972     2.3026
log
  :: AFType a
  => Array a
  -- ^ Input array
  -> Array a
  -- ^ Result of calling 'log'
log = flip op1 af_log

-- | Execute log1p
--
-- >>> A.log1p (A.vector @Int 10 [1..])
-- ArrayFire Array
-- [10 1 1 1]
--    0.6931     1.0986     1.3863     1.6094     1.7918     1.9459     2.0794     2.1972     2.3026     2.3979
log1p
  :: AFType a
  => Array a
  -- ^ Input array
  -> Array a
  -- ^ Result of calling 'log1p'
log1p = flip op1 af_log1p

-- | Execute log10
--
-- >>> A.log10 (A.vector @Int 10 [1..])
-- ArrayFire Array
-- [10 1 1 1]
--    0.0000     0.3010     0.4771     0.6021     0.6990     0.7782     0.8451     0.9031     0.9542     1.0000
log10
  :: AFType a
  => Array a
  -- ^ Input array
  -> Array a
  -- ^ Result of calling 'log10'
log10 = flip op1 af_log10

-- | Execute log2
--
-- >>> A.log2 (A.vector @Int 10 [1..])
-- ArrayFire Array
-- [10 1 1 1]
--    0.0000     1.0000     1.5850     2.0000     2.3219     2.5850     2.8074     3.0000     3.1699     3.3219
log2
  :: AFType a
  => Array a
  -- ^ Input array
  -> Array a
  -- ^ Result of calling 'log2'
log2 = flip op1 af_log2

-- | Execute sqrt
--
-- >>> A.sqrt (A.vector @Int 10 [1..])
-- ArrayFire Array
-- [10 1 1 1]
--    1.0000     1.4142     1.7321     2.0000     2.2361     2.4495     2.6458     2.8284     3.0000     3.1623
sqrt
  :: AFType a
  => Array a
  -- ^ Input array
  -> Array a
  -- ^ Result of calling 'sqrt'
sqrt = flip op1 af_sqrt

-- | Execute cbrt
--
-- >>> A.cbrt (A.vector @Int 10 [1..])
-- ArrayFire Array
-- [10 1 1 1]
--    1.0000     1.2599     1.4422     1.5874     1.7100     1.8171     1.9129     2.0000     2.0801     2.1544
cbrt
  :: AFType a
  => Array a
  -- ^ Input array
  -> Array a
  -- ^ Result of calling 'cbrt'
cbrt = flip op1 af_cbrt

-- | Execute factorial1
--
-- >>> A.factorial1 (A.vector @Int 10 [1..])
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
-- >>> A.lgamma (A.vector @Int 10 [1..])
-- ArrayFire Array
-- [10 1 1 1]
--    0.0000     0.0000     0.6931     1.7918     3.1781     4.7875     6.5793     8.5252    10.6046    12.8018
lgamma
  :: AFType a
  => Array a
  -- ^ Input array
  -> Array a
  -- ^ Result of calling 'lgamma'
lgamma = flip op1 af_lgamma

-- | Execute isZero
--
-- >>> A.isZero (A.vector @CBool 10 (repeat 0))
-- ArrayFire Array
-- [10 1 1 1]
--         1          1          1          1          1          1          1          1          1          1
isZero
  :: AFType a
  => Array a
  -- ^ Input array
  -> Array a
  -- ^ Result of calling 'isZero'
isZero = (`op1` af_iszero)

-- | Execute isInf
--
-- >>> A.isInf (A.vector @Double 10 [1..])
-- ArrayFire Array
-- [10 1 1 1]
--         0          0          0          0          0          0          0          0          0          0
isInf
  :: (Real a, AFType a)
  => Array a
  -- ^ Input array
  -> Array a
  -- ^ will contain 1's where input is Inf or -Inf, and 0 otherwise.
isInf = (`op1` af_isinf)

-- | Execute isNaN
--
-- >>> A.isNaN $ A.acos (A.vector @Int 10 [1..])
-- ArrayFire Array
-- [10 1 1 1]
--         0          1          1          1          1          1          1          1          1          1
isNaN
  :: forall a. (AFType a, Real a)
  => Array a
  -- ^ Input array
  -> Array a
  -- ^ Will contain 1's where input is NaN, and 0 otherwise.
isNaN = (`op1` af_isnan)
