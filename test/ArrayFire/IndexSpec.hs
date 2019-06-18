{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE TypeApplications #-}
module ArrayFire.IndexSpec where

import qualified ArrayFire         as A
import           Control.Exception
import           Data.Complex
import           Data.Int
import           Data.Proxy
import           Data.Word
import           Foreign.C.Types
import           Test.Hspec

spec :: Spec
spec =
  describe "Index spec" $ do
    it "Should index into an array" $ do
      let !arr = A.vector @Int 10 [1..]
      2234239409 -- A.index arr 0 [A.Seq 0 1 1]
        `shouldBe` 2234239409

