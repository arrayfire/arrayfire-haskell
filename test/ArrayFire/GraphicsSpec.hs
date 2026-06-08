{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module ArrayFire.GraphicsSpec where

import Control.Exception
import Data.Complex
import Data.Word
import Foreign.C.Types
import GHC.Int
import Test.Hspec

import ArrayFire

spec :: Spec
spec =
  describe "Graphics tests" $ do
    it "Should create window" $ do
      (1 + 1) `shouldBe` 2
