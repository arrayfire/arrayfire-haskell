{-# LANGUAGE TypeApplications #-}
module ArrayFire.UtilSpec where

import qualified ArrayFire        as A

import           Data.Complex
import           Data.Int
import           Data.Proxy
import           Data.Word
import           Foreign.C.Types
import           System.Directory
import           Test.Hspec

spec :: Spec
spec =
  describe "Util spec" $ do
    it "Should get size of" $ do
      A.getSizeOf (Proxy @Int) `shouldBe` 8
      A.getSizeOf (Proxy @Int64) `shouldBe` 8
      A.getSizeOf (Proxy @Int32) `shouldBe` 4
      A.getSizeOf (Proxy @Int16) `shouldBe` 2
      A.getSizeOf (Proxy @Word) `shouldBe` 8
      A.getSizeOf (Proxy @Word64) `shouldBe` 8
      A.getSizeOf (Proxy @Word32) `shouldBe` 4
      A.getSizeOf (Proxy @Word16) `shouldBe` 2
      A.getSizeOf (Proxy @Word8) `shouldBe` 1
      A.getSizeOf (Proxy @CBool) `shouldBe` 1
      A.getSizeOf (Proxy @Double) `shouldBe` 8
      A.getSizeOf (Proxy @Float) `shouldBe` 4
      A.getSizeOf (Proxy @(Complex Float)) `shouldBe` 8
      A.getSizeOf (Proxy @(Complex Double)) `shouldBe` 16
    it "Should get version" $ do
      (major, minor, patch) <- A.getVersion
      major `shouldBe` 3
      minor `shouldBe` 8
      patch `shouldSatisfy` (>= 0)
    it "Should get revision" $ do
      x <- A.getRevision
      x `shouldSatisfy` (not . null)
    it "Should save / read array" $ do
      let arr = A.constant @Int [1,1,1,1] 10
      idx <- A.saveArray "key" arr "file.array" False
      doesFileExist "file.array" `shouldReturn` True
      (`shouldBe` idx) =<< A.readArrayKeyCheck "file.array" "key"
      (`shouldBe` arr) =<< A.readArrayIndex "file.array" idx
      (`shouldBe` arr) =<< A.readArrayKey "file.array" "key"
      removeFile "file.array"

