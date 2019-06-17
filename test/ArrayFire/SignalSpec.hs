{-# LANGUAGE TypeApplications #-}
module ArrayFire.SignalSpec where

import qualified ArrayFire       as A
import           Data.Int
import           Data.Word
import           Data.Complex
import           Data.Proxy
import           Foreign.C.Types
import           Test.Hspec

spec :: Spec
spec =
  describe "Signal spec" $ do
    it "Should do FFT in place" $ do
      A.fftInPlace (A.matrix @(Complex Double) (10,10) (repeat (1 :+ 1))) 10.2
        `shouldReturn` ()
    it "Should do FFT" $ do
      A.fft (A.matrix @(Complex Float) (1,1) (repeat (1 :+ 1))) 1 1
        `shouldBe` A.matrix @(Complex Float) (1,1) (repeat (1 :+ 1))
