{-# LANGUAGE ScopedTypeVariables #-}
module Test.Hspec.ApproxExpect where

import Data.CallStack (HasCallStack)
import Test.Hspec (shouldSatisfy, Expectation)

infix 1 `shouldBeApprox`

-- | Element-wise relative + absolute closeness for lists of 'Double'.
--
-- Tolerances: atol = 1e-9, rtol = 1e-6 (suitable for BLAS/FFT results).
closeList :: [Double] -> [Double] -> Bool
closeList as bs =
  length as == length bs &&
  and (zipWith (\a b -> abs (a - b) <= 1e-9 + 1e-6 * max (abs a) (abs b)) as bs)

-- | Assert two floating-point values are within relative + absolute tolerance.
--
-- Uses the same formula as numpy.testing.assert_allclose:
--   |a - b| <= atol + rtol * max(|a|, |b|)
-- with rtol = 1e-5 and atol = 1e-8, matching numpy defaults.
shouldBeApprox
  :: (HasCallStack, Show a, Ord a, Fractional a)
  => a -> a -> Expectation
shouldBeApprox actual expected =
  actual `shouldSatisfy` \x ->
    abs (x - expected) <= atol + rtol * max (abs x) (abs expected)
  where
    rtol = 1e-5
    atol = 1e-8
