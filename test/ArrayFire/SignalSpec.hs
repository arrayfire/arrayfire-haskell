{-# LANGUAGE TypeApplications #-}
module ArrayFire.SignalSpec where

import qualified ArrayFire       as A
import           Data.Complex
import           Test.Hspec

-- | Check all elements of two Complex Double arrays are within tolerance.
shouldBeApproxC
  :: A.Array (Complex Double)
  -> A.Array (Complex Double)
  -> Expectation
shouldBeApproxC actual expected =
  zipWith (\a e -> magnitude (a - e))
    (A.toList @(Complex Double) actual)
    (A.toList @(Complex Double) expected)
  `shouldSatisfy` all (< 1e-10)

spec :: Spec
spec =
  describe "Signal" $ do

    describe "fft" $ do
      it "fftInPlace runs without error" $ do
        A.fftInPlace (A.scalar @(Complex Double) (1 :+ 0)) 1.0
          `shouldReturn` ()

      it "transform of a Dirac delta is a flat spectrum" $ do
        A.fft (A.mkArray @(Complex Double) [4] [1,0,0,0]) 1.0 4
          `shouldBeApproxC`
          A.mkArray @(Complex Double) [4] [1,1,1,1]

      it "transform of all-ones concentrates all energy at DC" $ do
        A.fft (A.mkArray @(Complex Double) [4] [1,1,1,1]) 1.0 4
          `shouldBeApproxC`
          A.mkArray @(Complex Double) [4] [4,0,0,0]

      it "normalization factor scales the output" $ do
        A.fft (A.mkArray @(Complex Double) [4] [1,0,0,0]) 2.0 4
          `shouldBeApproxC`
          A.mkArray @(Complex Double) [4] [2,2,2,2]

      it "ifft . fft is the identity" $ do
        let n     = 8
            input = A.mkArray @(Complex Double) [n] (map (:+ 0) [1..8])
        A.ifft (A.fft input 1.0 n) (1.0 / fromIntegral n) n
          `shouldBeApproxC` input

      it "fft output_size pads with zeros when larger than input" $ do
        -- 4-point FFT of a 2-point signal padded to 4: input [1,1,0,0]
        A.fft (A.mkArray @(Complex Double) [2] [1,1]) 1.0 4
          `shouldBeApproxC`
          A.fft (A.mkArray @(Complex Double) [4] [1,1,0,0]) 1.0 4

    describe "fft2" $ do
      it "2D transform of a Dirac delta is a flat spectrum" $ do
        A.fft2 (A.mkArray @(Complex Double) [4,4] (1 : replicate 15 0)) 1.0 4 4
          `shouldBeApproxC`
          A.mkArray @(Complex Double) [4,4] (replicate 16 1)

      it "ifft2 . fft2 is the identity" $ do
        let input = A.mkArray @(Complex Double) [4,4] (map (:+ 0) [1..16])
        A.ifft2 (A.fft2 input 1.0 4 4) (1.0 / 16) 4 4
          `shouldBeApproxC` input

      it "2D transform of all-ones concentrates all energy at DC" $ do
        A.fft2 (A.mkArray @(Complex Double) [4,4] (replicate 16 1)) 1.0 4 4
          `shouldBeApproxC`
          A.mkArray @(Complex Double) [4,4] (16 : replicate 15 0)
