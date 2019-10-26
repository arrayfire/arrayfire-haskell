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
--  checks (Proxy :: Proxy (A.Array (A.Complex Float)))
--  checks (Proxy :: Proxy (A.Array (A.Complex Double)))
--  checks (Proxy :: Proxy (A.Array Double))
--  checks (Proxy :: Proxy (A.Array Float))
--  checks (Proxy :: Proxy (A.Array Double))
--  checks (Proxy :: Proxy (A.Array A.Int16))
--  checks (Proxy :: Proxy (A.Array A.Int32))
  -- checks (Proxy :: Proxy (A.Array A.CBool))
  -- checks (Proxy :: Proxy (A.Array Word))
  -- checks (Proxy :: Proxy (A.Array A.Word8))
  -- checks (Proxy :: Proxy (A.Array A.Word16))
  -- checks (Proxy :: Proxy (A.Array A.Word32))
--  lawsCheck $ semigroupLaws (Proxy :: Proxy (A.Array Double))
--  lawsCheck $ semigroupLaws (Proxy :: Proxy (A.Array Float))
  hspec spec

checks proxy = do
  lawsCheck (numLaws proxy)
  lawsCheck (eqLaws proxy)
  lawsCheck (ordLaws proxy)
--  lawsCheck (semigroupLaws proxy)
