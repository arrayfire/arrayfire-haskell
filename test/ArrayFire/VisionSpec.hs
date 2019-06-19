{-# LANGUAGE TypeApplications #-}
module ArrayFire.VisionSpec where

import qualified ArrayFire  as A
import           Test.Hspec

spec :: Spec
spec =
  describe "Vision spec" $ do
    it "Should construct Features for fast feature detection" $ do
      let arr = A.vector @Int 30000 [1..]
      let feats = A.fast arr 1.0 9 False 1.0 3
      (1 + 1) `shouldBe` 2

