{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
-- | Numerical algorithm tests that exercise broad API surface area.
-- Each test has a known exact answer derived from mathematics, so failures
-- indicate either a bug in the library or a precision regression.
module ArrayFire.NumericalSpec where

import qualified ArrayFire as A
import           Data.Function ((&))
import           Test.Hspec
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck       (NonEmptyList (..))

tol :: Double
tol = 1e-4

shouldBeApprox :: Double -> Double -> Expectation
shouldBeApprox x y = abs (x - y) < tol `shouldBe` True

spec :: Spec
spec = describe "Numerical algorithms" $ do

  -- ∫₀^π sin(x) dx = 2  (midpoint rectangle rule)
  -- Exercises: arange, sin, sumAll, scalar, *, +
  describe "Rectangle-rule integration" $ do
    it "approximates integral of sin over [0,pi] = 2" $ do
      let n      = 10000 :: Int
          h      = pi / fromIntegral n
          is     = A.arange @Double [n] (-1)         -- [0,1,...,n-1]
          xs     = (is + A.scalar 0.5) * A.scalar h  -- midpoints
          result = h * A.sumAll (sin xs)
      result `shouldBeApprox` 2.0

  -- Power iteration on A = [[2,1],[1,2]]
  -- Exact dominant eigenvalue = 3, eigenvector = [1,1]/√2
  -- Exercises: matrix, matmul, sumAll, *, /, scalar, sqrt, Haskell iterate
  describe "Power iteration" $ do
    it "converges to dominant eigenvalue 3 of [[2,1],[1,2]]" $ do
      let a       = A.matrix @Double (2,2) [[2,1],[1,2]]
          v0      = A.matrix @Double (2,1) [[1,1]]
          norm2 v = sqrt @Double (A.sumAll (v * v))
          norm v  = v / A.scalar (norm2 v)
          step v  = norm (A.matmul a v A.None A.None)
          vFinal  = iterate step (norm v0) !! 30
          av      = A.matmul a vFinal A.None A.None
          -- Rayleigh quotient: v^T A v
          lambda  = A.sumAll (vFinal * av)
      lambda `shouldBeApprox` 3.0

  -- Geometric series: Σ(k=0..19) 0.5^k = (1 - 0.5^20)/(1 - 0.5)
  -- Exercises: arange, (**), sumAll, scalar
  describe "Geometric series" $ do
    it "sum of 0.5^k for k=0..19 matches closed form" $ do
      let n        = 20 :: Int
          ks       = A.arange @Double [n] (-1)
          terms    = A.scalar 0.5 ** ks
          result   = A.sumAll terms
          expected = (1.0 - 0.5 ^ n) / (1.0 - 0.5)
      result `shouldBeApprox` expected

  -- Centered-difference moving average on u = [1..10]:
  --   avg_i = (u[i-1] + u[i+1]) / 2  for i = 1..8
  -- For an arithmetic sequence, this equals u[i] exactly.
  -- Exercises: vector, (!), range, +, /, scalar
  describe "Slice-based centered differences" $ do
    it "moving average of arithmetic sequence equals interior values" $ do
      let u    = A.vector @Double 10 [1..10]
          avg  = (u A.! A.range 0 7 + u A.! A.range 2 9) / A.scalar 2.0
      avg `shouldBe` u A.! A.range 1 8

  -- Slice assignment: overwrite interior of a zero vector.
  -- Exercises: vector, &, (.~), !, range, toList
  describe "Slice assignment" $ do
    it "(.~) writes src into interior slice, leaves boundaries unchanged" $ do
      let u      = A.vector @Double 6 (repeat 0.0)
          src    = A.vector @Double 4 [1,2,3,4]
          result = u & A.range 1 4 A..~ src
      A.toList result `shouldBe` [0,1,2,3,4,0]

  -- Sample statistics of [1..100].
  -- mean([1..100]) = 50.5  (exact by Gauss's formula)
  -- sum = n * mean must hold exactly.
  -- Exercises: vector, meanAll, sumAll
  describe "Statistical identities" $ do
    it "mean of [1..100] = 50.5" $ do
      A.meanAll (A.vector @Double 100 [1..100]) `shouldBeApprox` 50.5
    it "sumAll = n * meanAll" $ do
      let arr   = A.vector @Double 100 [1..100]
          m     = A.meanAll arr
          s     = A.sumAll  arr
      s `shouldBeApprox` (100 * m)
    it "variance of a constant array is 0" $ do
      A.varAll (A.vector @Double 50 (repeat 7.0)) A.Population `shouldBeApprox` 0.0

  -- Sum of first n squares: Σ(k=1..n) k² = n(n+1)(2n+1)/6
  -- Exercises: iota, *, +, scalar, sumAll
  describe "Sum of squares" $ do
    it "Sigma k^2 for k=1..100 matches closed form n(n+1)(2n+1)/6" $ do
      let n        = 100 :: Int
          ks       = A.iota @Double [n] [] + A.scalar 1.0  -- [1,2,...,n]
          result   = A.sumAll (ks * ks)
          expected = fromIntegral (n * (n+1) * (2*n+1)) / 6.0
      result `shouldBeApprox` expected

  -- Parseval's theorem: ||x||² = (1/N)||X||²  where X = FFT(x)
  -- Uses a complex Dirac delta: |x|² = 1, FFT is a flat spectrum |X[k]|² = 1 each.
  -- Exercises: mkArray, fft, conjg, real, sumAll, *
  describe "Parseval's theorem" $ do
    it "time-domain and frequency-domain energies agree" $ do
      let n       = 64 :: Int
          -- Dirac delta: all energy in first sample
          xs      = A.mkArray @(A.Complex Double) [n] (1 : repeat 0)
          -- time-domain energy: Σ |x[k]|² = 1
          tEnergy = A.sumAll (A.real (xs * A.conjg xs) :: A.Array Double)
          -- frequency-domain energy: (1/N) Σ |X[k]|² = (1/N)*N = 1
          xf      = A.fft xs 1.0 n
          fEnergy = (1.0 / fromIntegral n) * (A.sumAll (A.real (xf * A.conjg xf) :: A.Array Double))
      tEnergy `shouldBeApprox` 1.0
      tEnergy `shouldBeApprox` fEnergy

  describe "sumAll = n * meanAll (property)" $ do
    prop "sumAll = n * meanAll for any non-empty list of Double" $ \(NonEmpty xs) ->
      let n   = length xs
          arr = A.vector @Double n xs
          s   = A.sumAll arr
          m   = A.meanAll arr
      in abs (s - fromIntegral n * m) < 1e-9 + 1e-6 * abs s
