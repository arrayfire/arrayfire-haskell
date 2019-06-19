{-# LANGUAGE TypeApplications #-}
module ArrayFire.UtilSpec where

import qualified ArrayFire       as A
import           Data.Int
import           Data.Word
import           Data.Complex
import           Data.Proxy
import           Foreign.C.Types
import           Test.Hspec

spec :: Spec
spec =
  describe "Util spec" $ do
    it "Should get size of" $ do
      A.getSizeOf (Proxy @ Int) `shouldBe` 8
      A.getSizeOf (Proxy @ Int64) `shouldBe` 8
      A.getSizeOf (Proxy @ Int32) `shouldBe` 4
      A.getSizeOf (Proxy @ Int16) `shouldBe` 2
      A.getSizeOf (Proxy @ Word) `shouldBe` 8
      A.getSizeOf (Proxy @ Word64) `shouldBe` 8
      A.getSizeOf (Proxy @ Word32) `shouldBe` 4
      A.getSizeOf (Proxy @ Word16) `shouldBe` 2
      A.getSizeOf (Proxy @ Word8) `shouldBe` 1
      A.getSizeOf (Proxy @ Bool) `shouldBe` 1
      A.getSizeOf (Proxy @ Double) `shouldBe` 8
      A.getSizeOf (Proxy @ Float) `shouldBe` 4
      A.getSizeOf (Proxy @ (Complex Float)) `shouldBe` 8
      A.getSizeOf (Proxy @ (Complex Double)) `shouldBe` 16
    it "Should get version" $ do
      x <- A.getVersion
      x `shouldBe` (3,6,4)
