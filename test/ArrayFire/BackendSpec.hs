{-# LANGUAGE TypeApplications #-}
module ArrayFire.BackendSpec where

import ArrayFire hiding (not)

import Test.Hspec

spec :: Spec
spec =
  describe "Backend spec" $ do
    it "Should get backend count" $ do
      (`shouldSatisfy` (>0)) =<< getBackendCount
    it "Should get available backends" $ do
      backends <- getAvailableBackends
      backends `shouldSatisfy` (CPU `elem`)
    it "Should set backend to CPU" $ do
      setBackend OpenCL
      (`shouldBe` OpenCL) =<< getActiveBackend
      let arr = matrix @Int (2,2) [1..]
      getBackendID arr `shouldBe` OpenCL
