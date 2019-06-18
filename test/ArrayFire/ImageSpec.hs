{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module ArrayFire.ImageSpec where

import Control.Exception
import Data.Complex
import Data.Word
import Foreign.C.Types
import GHC.Int
import Test.Hspec

import ArrayFire

spec :: Spec
spec =
  describe "Image tests" $ do
    it "Should test if Image I/O is available" $ do
      isImageIOAvailable `shouldReturn` True
