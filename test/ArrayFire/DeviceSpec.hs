{-# LANGUAGE TypeApplications #-}
module ArrayFire.DeviceSpec where

import qualified ArrayFire       as A
import           Foreign.C.Types
import           Test.Hspec

spec :: Spec
spec =
  describe "Algorithm tests" $ do
    it "Should show device info" $ do
      A.info `shouldReturn` ()
    it "Should show device init" $ do
      A.afInit `shouldReturn` ()
    it "Should get info string" $ do
      A.getInfoString >>= (`shouldSatisfy` (not . null))
    it "Should get device" $ do
      A.getDevice >>= (`shouldSatisfy` (>= 0))
    it "Should get and set device" $ do
      (A.getDevice >>= A.setDevice) `shouldReturn` ()

