{-# LANGUAGE TypeApplications #-}
module ArrayFire.RandomSpec where

import ArrayFire

import Test.Hspec

spec :: Spec
spec =
  describe "Random engine spec" $ do
    it "Should create random engine" $ do
      (`shouldBe` Philox)
         =<< getRandomEngine
         =<< createRandomEngine 10 Philox
      (`shouldBe` Mersenne)
         =<< getRandomEngine
         =<< createRandomEngine 10 Mersenne
      (`shouldBe` ThreeFry)
         =<< getRandomEngine
         =<< createRandomEngine 10 ThreeFry
    it "Should set random engine" $ do
       r <- createRandomEngine 10 ThreeFry
       setRandomEngine r Philox
       (`shouldBe` Philox) =<< getRandomEngine r
    it "Should set and get seed" $ do
       r <- setSeed 100
       (`shouldBe` 100) =<< getSeed


