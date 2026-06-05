{-# LANGUAGE ScopedTypeVariables #-}
module Test.Hspec.ApproxExpect where

import Data.CallStack (HasCallStack)
import Test.Hspec (shouldSatisfy, Expectation)

infix 1 `shouldBeApprox`

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
