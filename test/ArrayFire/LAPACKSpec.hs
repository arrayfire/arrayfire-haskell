{-# LANGUAGE TypeApplications #-}
module ArrayFire.LAPACKSpec where

import qualified ArrayFire       as A
import           Prelude
import           Test.Hspec

spec :: Spec
spec =
  describe "LAPACK spec" $ do
    it "Should have LAPACK available" $ do
      A.isLAPACKAvailable `shouldBe` True
