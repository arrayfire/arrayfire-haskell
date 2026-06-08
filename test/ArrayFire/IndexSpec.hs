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
      let arr = A.vector @Int 10 [1..]
      A.index arr [A.Seq 0 4 1]
        `shouldBe`
           A.vector @Int 5 [1..]
