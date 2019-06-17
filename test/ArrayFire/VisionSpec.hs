{-# LANGUAGE TypeApplications #-}
module ArrayFire.VisionSpec where

import qualified ArrayFire  as A
import           Test.Hspec

spec :: Spec
spec =
  describe "Vision spec" $ do
    it "Should get difference of gaussians" $ do
      (1 + 1) `shouldBe` 2
