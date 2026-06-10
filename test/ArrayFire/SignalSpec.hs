{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module ArrayFire.SignalSpec where

import qualified ArrayFire       as A
import           Data.Complex
import           Test.Hspec
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck       (NonEmptyList (..), choose, forAll, vectorOf)

-- | Check all elements of two Double arrays are within tolerance.
shouldBeApproxD
  :: A.Array Double
  -> A.Array Double
  -> Expectation
shouldBeApproxD actual expected =
  zipWith (\a e -> abs (a - e))
    (A.toList @Double actual)
    (A.toList @Double expected)
  `shouldSatisfy` all (< 1e-6)

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

    describe "fft2_inplace" $ do
      it "runs without error" $ do
        A.fft2_inplace (A.mkArray @(Complex Double) [4,4] (map (:+ 0) [1..16])) 1.0
          `shouldReturn` ()

    describe "fft3" $ do
      it "3D transform of a Dirac delta is a flat spectrum" $ do
        A.fft3 (A.mkArray @(Complex Double) [4,4,4] (1 : replicate 63 0)) 1.0 4 4 4
          `shouldBeApproxC`
          A.mkArray @(Complex Double) [4,4,4] (replicate 64 1)

      it "ifft3 . fft3 is the identity" $ do
        let input = A.mkArray @(Complex Double) [4,4,4] (map (:+ 0) [1..64])
        A.ifft3 (A.fft3 input 1.0 4 4 4) (1.0 / 64) 4 4 4
          `shouldBeApproxC` input

    describe "fft3_inplace" $ do
      it "runs without error" $ do
        A.fft3_inplace (A.mkArray @(Complex Double) [4,4,4] (map (:+ 0) [1..64])) 1.0
          `shouldReturn` ()

    describe "ifft_inplace" $ do
      it "runs without error" $ do
        A.ifft_inplace (A.mkArray @(Complex Double) [4] (map (:+ 0) [1..4])) 1.0
          `shouldReturn` ()

    describe "ifft2_inplace" $ do
      it "runs without error" $ do
        A.ifft2_inplace (A.mkArray @(Complex Double) [4,4] (map (:+ 0) [1..16])) 1.0
          `shouldReturn` ()

    describe "ifft3_inplace" $ do
      it "runs without error" $ do
        A.ifft3_inplace (A.mkArray @(Complex Double) [4,4,4] (map (:+ 0) [1..64])) 1.0
          `shouldReturn` ()

    describe "fftr2c / fftc2r" $ do
      it "fftr2c output has (n/2+1) complex elements" $ do
        let n = 8
            out = A.fftr2c (A.mkArray @Double [n] [1..8]) 1.0 n
        A.getElements out `shouldBe` (n `div` 2 + 1)

      it "fftc2r recovers even-length real signal" $ do
        let n    = 8
            inp  = A.mkArray @Double [n] [1..8]
            spec' = A.fftr2c inp 1.0 n
            -- norm = 1/n so that r2c * c2r = identity
            out  = A.fftc2r spec' (1.0 / fromIntegral n) False
        out `shouldBeApproxD` inp

      it "fft2r2c output first dim is (n/2+1)" $ do
        let n = 8
            out = A.fft2r2c (A.mkArray @Double [n,n] (replicate (n*n) 1.0)) 1.0 n n
            (d0, _, _, _) = A.getDims out
        d0 `shouldBe` (n `div` 2 + 1)

      it "fft3r2c runs without error" $ do
        let n = 4
            out = A.fft3r2c (A.mkArray @Double [n,n,n] (replicate (n*n*n) 1.0)) 1.0 n n n
        A.getElements out `shouldSatisfy` (> 0)

    describe "approx1" $ do
      it "matches docstring example with Cubic interpolation" $ do
        let input     = A.vector @Float 3 [10,20,30]
            positions = A.vector @Float 5 [0.0, 0.5, 1.0, 1.5, 2.0]
            result    = A.approx1 input positions A.Cubic 0.0
        zipWith (\a e -> abs (a - e))
          (A.toList @Float result)
          (A.toList @Float (A.mkArray @Float [5] [10.0, 13.75, 20.0, 26.25, 30.0]))
          `shouldSatisfy` all (< 1e-4)

      it "Nearest interpolation returns nearest sample value" $ do
        let input     = A.vector @Float 3 [10,20,30]
            positions = A.vector @Float 3 [0.0, 1.0, 2.0]
        zipWith (\a e -> abs (a - e))
          (A.toList @Float (A.approx1 input positions A.Nearest 0.0))
          (A.toList @Float (A.mkArray @Float [3] [10.0, 20.0, 30.0]))
          `shouldSatisfy` all (< 1e-4)

      it "out-of-bounds positions use the fill value" $ do
        let input     = A.vector @Double 3 [10,20,30]
            positions = A.vector @Double 1 [-1.0]
        A.approx1 input positions A.Linear 0.0
          `shouldBeApproxD` A.mkArray @Double [1] [0.0]

    describe "approx2" $ do
      it "matches docstring example with Cubic interpolation" $ do
        let input  = A.matrix @Float (3,3) [[1,1,1],[2,2,2],[3,3,3]]
            pos1   = A.matrix @Float (2,2) [[0.5,1.5],[0.5,1.5]]
            pos2   = A.matrix @Float (2,2) [[0.5,0.5],[1.5,1.5]]
            result = A.approx2 input pos1 pos2 A.Cubic 0.0
        zipWith (\a e -> abs (a - e))
          (A.toList @Float result)
          (A.toList @Float (A.mkArray @Float [2,2] [1.375, 2.625, 1.375, 2.625]))
          `shouldSatisfy` all (< 1e-4)

    describe "convolve1" $ do
      it "convolving with unit delta is identity" $ do
        let sig    = A.mkArray @Double [5] [1,2,3,4,5]
            delta  = A.mkArray @Double [1] [1]
        A.convolve1 sig delta A.ConvDefault A.ConvDomainSpatial
          `shouldBeApproxD` sig

      it "ConvExpand output length is signal_len + filter_len - 1" $ do
        let sig = A.mkArray @Double [5] [1,2,3,4,5]
            flt = A.mkArray @Double [3] [1,0,0]
            out = A.convolve1 sig flt A.ConvExpand A.ConvDomainSpatial
        A.getElements out `shouldBe` 7

      it "ConvDomainAuto matches ConvDomainSpatial result" $ do
        let sig = A.mkArray @Double [8] [1,2,3,4,5,6,7,8]
            flt = A.mkArray @Double [3] [1,2,1]
        A.convolve1 sig flt A.ConvDefault A.ConvDomainAuto
          `shouldBeApproxD`
          A.convolve1 sig flt A.ConvDefault A.ConvDomainSpatial

    describe "convolve2" $ do
      it "convolving with unit 2D delta is identity" $ do
        let img   = A.mkArray @Double [4,4] [1..16]
            delta = A.mkArray @Double [1,1] [1]
        A.convolve2 img delta A.ConvDefault A.ConvDomainSpatial
          `shouldBeApproxD` img

    describe "convolve2Sep" $ do
      it "separable convolution matches full 2D convolution with outer-product kernel" $ do
        let img  = A.mkArray @Double [4,4] [1..16]
            colF = A.mkArray @Double [1] [1]
            rowF = A.mkArray @Double [1] [1]
        A.convolve2Sep colF rowF img A.ConvDefault
          `shouldBeApproxD` img

    describe "fftConvolve2" $ do
      it "result matches spatial convolve2 for a simple kernel" $ do
        let img = A.mkArray @Double [8,8] [1..64]
            flt = A.mkArray @Double [3,3] [0,0,0, 0,1,0, 0,0,0]
        A.fftConvolve2 img flt A.ConvDefault
          `shouldBeApproxD`
          A.convolve2 img flt A.ConvDefault A.ConvDomainSpatial

    describe "fir" $ do
      it "passthrough filter (b=[1]) returns input unchanged" $ do
        let sig = A.mkArray @Double [5] [1,2,3,4,5]
            b   = A.mkArray @Double [1] [1]
        A.fir b sig `shouldBeApproxD` sig

    describe "iir" $ do
      it "all-feedforward / no-feedback is equivalent to FIR" $ do
        let sig = A.mkArray @Double [5] [1,2,3,4,5]
            b   = A.mkArray @Double [1] [1]
            a   = A.mkArray @Double [1] [1]
        A.iir b a sig `shouldBeApproxD` sig

    describe "medFilt1" $ do
      it "constant signal is unchanged by any kernel" $ do
        let sig = A.mkArray @Double [7] (replicate 7 3.0)
        A.medFilt1 sig 3 A.PadZero `shouldBeApproxD` sig

    describe "medFilt2" $ do
      it "constant image is unchanged by any kernel" $ do
        let img = A.mkArray @Double [5,5] (replicate 25 7.0)
        A.medFilt2 img 3 3 A.PadSym `shouldBeApproxD` img

    describe "convolve3" $ do
      it "convolving with unit 3D delta is identity" $ do
        let vol   = A.mkArray @Double [4,4,4] [1..64]
            delta = A.mkArray @Double [1,1,1] [1]
        A.convolve3 vol delta A.ConvDefault A.ConvDomainSpatial
          `shouldBeApproxD` vol

    describe "fft2C2r" $ do
      it "fft2r2c . fft2C2r is the identity for an even-size 2D signal" $ do
        let n   = 8
            inp = A.mkArray @Double [n,n] [1..fromIntegral (n*n)]
            c2r = A.fft2C2r (A.fft2r2c inp 1.0 n n) (1.0 / fromIntegral (n*n)) False
        c2r `shouldBeApproxD` inp

    describe "fft3C2r" $ do
      it "fft3r2c . fft3C2r is the identity for an even-size 3D signal" $ do
        let n   = 4
            inp = A.mkArray @Double [n,n,n] [1..fromIntegral (n*n*n)]
            c2r = A.fft3C2r (A.fft3r2c inp 1.0 n n n) (1.0 / fromIntegral (n*n*n)) False
        c2r `shouldBeApproxD` inp

    describe "setFFTPlanCacheSize" $ do
      it "runs without error" $ do
        A.setFFTPlanCacheSize 4 `shouldReturn` ()

    describe "FFT properties" $ do
      -- ifft . fft = id for arbitrary complex signals of power-of-2 length
      prop "ifft . fft = id (arbitrary complex signal)" $
        forAll (choose (1 :: Int, 6)) $ \k ->
          forAll (vectorOf (2^k) (choose (-10, 10 :: Double))) $ \xs ->
            let n     = 2^k
                input = A.mkArray @(A.Complex Double) [n] (map (:+ 0) xs)
                out   = A.ifft (A.fft input 1.0 n) (1.0 / fromIntegral n) n
            in zipWith (\a e -> magnitude (a - e))
                 (A.toList @(A.Complex Double) out)
                 (A.toList @(A.Complex Double) input)
               `shouldSatisfy` all (< 1e-9)

      -- FFT linearity: fft(a + b) = fft(a) + fft(b)
      prop "fft is linear: fft(a+b) = fft(a) + fft(b)" $
        forAll (choose (1 :: Int, 5)) $ \k ->
          forAll (vectorOf (2^k) (choose (-5, 5 :: Double))) $ \as_ ->
          forAll (vectorOf (2^k) (choose (-5, 5 :: Double))) $ \bs_ ->
            let n   = 2^k
                a   = A.mkArray @(A.Complex Double) [n] (map (:+ 0) as_)
                b   = A.mkArray @(A.Complex Double) [n] (map (:+ 0) bs_)
                lhs = A.toList @(A.Complex Double) (A.fft (a + b) 1.0 n)
                rhs = zipWith (+)
                        (A.toList @(A.Complex Double) (A.fft a 1.0 n))
                        (A.toList @(A.Complex Double) (A.fft b 1.0 n))
            in zipWith (\l r -> magnitude (l - r)) lhs rhs
               `shouldSatisfy` all (< 1e-9)

      -- Parseval's theorem: ||x||^2 = (1/N) * ||X||^2
      prop "Parseval's theorem holds for arbitrary signals" $
        forAll (choose (1 :: Int, 6)) $ \k ->
          forAll (vectorOf (2^k) (choose (-10, 10 :: Double))) $ \xs ->
            let n       = 2^k
                input   = A.mkArray @(A.Complex Double) [n] (map (:+ 0) xs)
                tEnergy = sum (map (\x -> x*x) xs)
                xf      = A.fft input 1.0 n
                fEnergy = (1.0 / fromIntegral n) *
                            sum (map (\c -> realPart c * realPart c + imagPart c * imagPart c)
                                     (A.toList @(A.Complex Double) xf))
            in abs (tEnergy - fEnergy) < 1e-6 + 1e-6 * abs tEnergy

      -- convolve1 with unit delta is identity for arbitrary signals
      prop "convolve1 with unit delta is identity" $ \(NonEmpty xs) ->
        let sig   = A.mkArray @Double [length xs] xs
            delta = A.mkArray @Double [1] [1]
            out   = A.convolve1 sig delta A.ConvDefault A.ConvDomainSpatial
        in zipWith (\a e -> abs (a - e))
             (A.toList @Double out)
             (A.toList @Double sig)
           `shouldSatisfy` all (< 1e-9)

      -- fftr2c . fftc2r round-trip for arbitrary even-length real signals
      prop "fftc2r . fftr2c = id for even-length real signals" $
        forAll (choose (1 :: Int, 5)) $ \k ->
          forAll (vectorOf (2^k) (choose (-10, 10 :: Double))) $ \xs ->
            let n   = 2^k
                inp = A.mkArray @Double [n] xs
                out = A.fftc2r (A.fftr2c inp 1.0 n) (1.0 / fromIntegral n) False
            in zipWith (\a e -> abs (a - e))
                 (A.toList @Double out)
                 xs
               `shouldSatisfy` all (< 1e-9)
