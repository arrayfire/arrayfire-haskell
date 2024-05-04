{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Hspec.ApproxExpect where

import Data.CallStack (HasCallStack)

import Test.Hspec (shouldSatisfy, Expectation)

infix 1 `shouldBeApprox`

shouldBeApprox :: (HasCallStack, Show a, Fractional a, Eq a)
    => a -> a -> Expectation
shouldBeApprox actual tgt
       -- This is a hackish way of checking, without requiring a specific
       -- type or an 'Ord' instance, whether two floating-point values
       -- are only some epsilons apart: when the difference is small enough
       -- so scaling it down some more makes it a no-op for addition.
   = actual `shouldSatisfy` \x -> (x-tgt) * 1e-4 + tgt == tgt

