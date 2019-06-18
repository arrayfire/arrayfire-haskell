{-# LANGUAGE TypeApplications #-}
module ArrayFire.RandomSpec where

import ArrayFire
import Control.Monad

import Test.Hspec

spec :: Spec
spec =
  describe "Random engine spec" $ do
    it "Should create random engine" $ do
      (`shouldBe` Philox)
         =<< getRandomEngine
         =<< createRandomEngine 5000 Philox
      (`shouldBe` Mersenne)
         =<< getRandomEngine
         =<< createRandomEngine 5000 Mersenne
      (`shouldBe` ThreeFry)
         =<< getRandomEngine
         =<< createRandomEngine 5000 ThreeFry
    it "Should set random engine" $ do
       r <- createRandomEngine 5000 ThreeFry
       setRandomEngine r Philox
       (`shouldBe` Philox) =<< getRandomEngine r
    it "Should set and get seed" $ do
       setSeed 100
       (`shouldBe` 100) =<< getSeed


