{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module ArrayFire.DataSpec where

import Control.Exception
import Data.Complex
import Data.Word
import Foreign.C.Types
import GHC.Int
import Test.Hspec

import ArrayFire

spec :: Spec
spec =
  describe "Data tests" $ do
    it "Should create constant Array" $ do
      constant [1] 1.0 `shouldBe` 1.0
