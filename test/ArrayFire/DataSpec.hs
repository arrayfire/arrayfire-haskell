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
      constant @Float [1] 1 `shouldBe` 1
      constant @Double [1] 1 `shouldBe` 1
      constant @Int16 [1] 1 `shouldBe` 1
      constant @Int32 [1] 1 `shouldBe` 1
      constant @Int64 [1] 1 `shouldBe` 1
      constant @Int [1] 1 `shouldBe` 1
      constant @Word16 [1] 1 `shouldBe` 1
      constant @Word32 [1] 1 `shouldBe` 1
      constant @Word64 [1] 1 `shouldBe` 1
      constant @Word [1] 1 `shouldBe` 1
      constant @CBool [1] 1 `shouldBe` 1
      constant @(Complex Double) [1] (1.0 :+ 1.0)
        `shouldBe`
          constant @(Complex Double) [1] (1.0 :+ 1.0)
      constant @(Complex Float) [1] (1.0 :+ 1.0)
        `shouldBe`
          constant @(Complex Float) [1] (1.0 :+ 1.0)
