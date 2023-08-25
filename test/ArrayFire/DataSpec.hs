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
    it "Should join Arrays along the specified dimension" $ do
      join 0 (constant @Int [1, 3] 1) (constant @Int [1, 3] 2) `shouldBe` mkArray @Int [2, 3] [1, 2, 1, 2, 1, 2]
      join 1 (constant @Int [1, 2] 1) (constant @Int [1, 2] 2) `shouldBe` mkArray @Int [1, 4] [1, 1, 2, 2]
      joinMany 0 [constant @Int [1, 3] 1, constant @Int [1, 3] 2] `shouldBe` mkArray @Int [2, 3] [1, 2, 1, 2, 1, 2]
      joinMany 1 [constant @Int [1, 2] 1, constant @Int [1, 1] 2, constant @Int [1, 3] 3] `shouldBe` mkArray @Int [1, 6] [1, 1, 2, 3, 3, 3]
