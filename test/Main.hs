{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Control.Monad

import           Data.Proxy
import           Spec                    (spec)
import           Test.Hspec              (hspec)
import           Test.QuickCheck
import           Test.QuickCheck.Classes

import qualified ArrayFire               as A
import           ArrayFire               (Array)

import           System.IO.Unsafe

instance (A.AFType a, Arbitrary a) => Arbitrary (Array a) where
  arbitrary = pure $ unsafePerformIO (A.randu [2,2])

main :: IO ()
main = do
  lawsCheck $ eqLaws (Proxy @(A.Complex Double))
  hspec spec

p :: Proxy (Array a)
p = Proxy

checks proxy = do
-- lawsCheck (numLaws proxy)
  lawsCheck (eqLaws proxy)
  lawsCheck (ordLaws proxy)
-- lawsCheck (semigroupLaws proxy)
