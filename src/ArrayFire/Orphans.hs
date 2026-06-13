{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
--------------------------------------------------------------------------------
-- |
-- Module      : ArrayFire.Orphans
-- Copyright   : David Johnson (c) 2019-2026
-- License     : BSD 3
-- Maintainer  : David Johnson <code@dmj.io>
-- Stability   : Experimental
-- Portability : GHC
--
--------------------------------------------------------------------------------
module ArrayFire.Orphans where

import           Prelude hiding (pi)
import qualified Prelude

import           Control.DeepSeq (NFData(..))
import           Data.Proxy      (Proxy (..))

import qualified ArrayFire.Arith     as A
import qualified ArrayFire.Array     as A
import qualified ArrayFire.Algorithm as A
import qualified ArrayFire.Data      as A
import           ArrayFire.Internal.Defines (s16, s32, s64, u8, u16, u32, u64, b8)
import           ArrayFire.Types
import           ArrayFire.Util

instance NFData (Array a) where
  rnf x = x `seq` ()

-- | Structural equality on 'Array': equal shapes and elementwise-equal values.
--
-- Both inputs are 'A.eval'-ed before the comparison to flush each array's JIT
-- queue; skipping either eval can produce stale results. 'A.allTrueAll' reads
-- back a @(real, imaginary)@ pair; the imaginary component is reliably @0@ for
-- boolean reductions, so comparing only the real part against @1.0@ is safe.
--
-- /Caveat/: comparisons follow IEEE semantics elementwise, so an array
-- containing @NaN@ is not equal to itself (@x == x@ is 'False'), violating
-- 'Eq' reflexivity exactly as 'Double' itself does. @(\/=)@ remains the exact
-- negation of @(==)@ in all cases, including @NaN@.
instance (AFType a, Eq a) => Eq (Array a) where
  x == y = A.getDims x == A.getDims y
        && A.allTrueAll (A.eqBatched (A.eval x) (A.eval y) False) == 1.0

  x /= y = A.getDims x /= A.getDims y
        || A.anyTrueAll (A.neqBatched (A.eval x) (A.eval y) False) /= 0.0


-- | Elementwise 'Num' instance for 'Array'.
--
-- Note that 'signum' implements the real-valued, three-way sign
-- (@x > 0 -> 1@, @x < 0 -> -1@, otherwise @0@). This matches Haskell's
-- 'signum' for integral and real-floating arrays with finite values, but
-- diverges in a few cases:
--
--     * @NaN@ (for 'Float'\/'Double') yields @0@, whereas Haskell yields @NaN@.
--     * Negative zero @-0.0@ yields @+0.0@, losing the signed zero that
--       Haskell preserves.
--     * For complex arrays (e.g. @'Array' ('Data.Complex.Complex' Double)@)
--       it returns @1@\/@-1@\/@0@ from an order comparison rather than the unit
--       phasor @z / 'abs' z@ that Haskell's 'signum' produces, so the law
--       @'abs' x * 'signum' x == x@ does not hold for complex inputs.
instance (Num a, AFType a) => Num (Array a) where
  x + y       = A.add x y
  x * y       = A.mul x y
  -- af_abs promotes all integer inputs to f32 internally (see complex.cpp),
  -- losing precision for |x| > 2^24.  For integer types we implement abs
  -- entirely in integer arithmetic: signed types negate negative elements via
  -- select; unsigned types are already non-negative so abs is the identity.
  abs x
    | dt `elem` [s16, s32, s64] = A.select (A.lt x 0) (0 - x) x
    | dt `elem` [u8, u16, u32, u64, b8] = x
    | otherwise = A.abs x   -- float / complex: delegate to AF
    where dt = afType (Proxy @a)
  signum x    = A.select (A.gt x 0) 1 (A.select (A.lt x 0) (-1) 0)
  negate arr  = A.scalar @a (fromInteger (-1)) `A.mul` arr
  x - y       = A.sub x y
  fromInteger = A.scalar . fromIntegral

instance Show (Array a) where
  show = arrayString

instance forall a . (Fractional a, AFType a) => Fractional (Array a) where
  x / y  = A.div x y
  fromRational n = A.scalar @a (fromRational n)

instance forall a . (Ord a, AFType a, Fractional a) => Floating (Array a) where
  pi   = A.scalar @a (realToFrac (Prelude.pi :: Double))
  exp  = A.exp @a
  log  = A.log @a
  sqrt = A.sqrt @a
  (**) = A.pow @a
  sin  = A.sin @a
  cos  = A.cos @a
  tan = A.tan @a
  tanh = A.tanh @a
  asin = A.asin @a
  acos = A.acos @a
  atan = A.atan @a
  sinh = A.sinh @a
  cosh = A.cosh @a
  acosh = A.acosh @a
  atanh = A.atanh @a
  asinh = A.asinh @a
