{-# LANGUAGE TypeApplications #-}
module ArrayFire.RandomSpec where

import ArrayFire

import Test.Hspec

spec :: Spec
spec = describe "Random spec" $ do

  describe "random engine" $ do
    it "Should create random engine" $ do
      (`shouldBe` Philox)
         =<< getRandomEngineType
         =<< createRandomEngine 5000 Philox
      (`shouldBe` Mersenne)
         =<< getRandomEngineType
         =<< createRandomEngine 5000 Mersenne
      (`shouldBe` ThreeFry)
         =<< getRandomEngineType
         =<< createRandomEngine 5000 ThreeFry
    it "Should set random engine" $ do
       r <- createRandomEngine 5000 ThreeFry
       setRandomEngine r Philox
       (`shouldBe` Philox) =<< getRandomEngineType r
    it "Should set and get seed" $ do
       setSeed 100
       (`shouldBe` 100) =<< getSeed

  -- Reproducibility is the contract that makes randomness usable in tests and
  -- science: a fixed seed must yield a fixed stream.
  describe "seed reproducibility" $ do

    it "global setSeed makes randu reproducible" $ do
      setSeed 1234
      a1 <- toList <$> randu @Float [256]
      setSeed 1234
      a2 <- toList <$> randu @Float [256]
      a2 `shouldBe` a1

    it "global setSeed makes randn reproducible" $ do
      setSeed 9876
      a1 <- toList <$> randn @Double [256]
      setSeed 9876
      a2 <- toList <$> randn @Double [256]
      a2 `shouldBe` a1

    it "two engines with the same seed + type draw the same stream" $ do
      e1 <- createRandomEngine 42 Philox
      e2 <- createRandomEngine 42 Philox
      a1 <- toList <$> randomUniform @Float [256] e1
      a2 <- toList <$> randomUniform @Float [256] e2
      a2 `shouldBe` a1

    it "engines with different seeds draw different streams" $ do
      e1 <- createRandomEngine 1 Philox
      e2 <- createRandomEngine 2 Philox
      a1 <- toList <$> randomUniform @Float [256] e1
      a2 <- toList <$> randomUniform @Float [256] e2
      a2 `shouldNotBe` a1

  describe "distribution shape & range" $ do

    it "randu produces the requested dimensions" $ do
      a <- randu @Float [3,4]
      getDims a `shouldBe` (3,4,1,1)

    it "randn produces the requested dimensions" $ do
      a <- randn @Double [5,2,3]
      getDims a `shouldBe` (5,2,3,1)

    it "uniform draws lie in [0,1)" $ do
      setSeed 7
      xs <- toList <$> randu @Float [4096]
      xs `shouldSatisfy` all (\x -> x >= 0 && x < 1)

  describe "randomNormal" $ do
    it "produces the requested dimensions" $ do
      e <- getDefaultRandomEngine
      a <- randomNormal @Double [3,4] e
      getDims a `shouldBe` (3,4,1,1)
    it "produces the right number of elements" $ do
      e <- getDefaultRandomEngine
      a <- randomNormal @Float [5,2] e
      getElements a `shouldBe` 10

    it "two engines with the same seed produce the same normal stream" $ do
      e1 <- createRandomEngine 42 Philox
      e2 <- createRandomEngine 42 Philox
      a1 <- toList <$> randomNormal @Double [256] e1
      a2 <- toList <$> randomNormal @Double [256] e2
      a2 `shouldBe` a1
    it "engines with different seeds produce different normal streams" $ do
      e1 <- createRandomEngine 1 Philox
      e2 <- createRandomEngine 2 Philox
      a1 <- toList <$> randomNormal @Double [256] e1
      a2 <- toList <$> randomNormal @Double [256] e2
      a2 `shouldNotBe` a1

  describe "randomEngineSetSeed / randomEngineGetSeed" $ do
    it "getSeed returns the seed supplied to createRandomEngine" $ do
      e <- createRandomEngine 9999 Philox
      s <- randomEngineGetSeed e
      s `shouldBe` 9999
    it "setAndGet round-trip: seed is updated after randomEngineSetSeed" $ do
      e <- createRandomEngine 1 Philox
      randomEngineSetSeed e 12345
      s <- randomEngineGetSeed e
      s `shouldBe` 12345
    it "different seeds produce different streams after randomEngineSetSeed" $ do
      e <- createRandomEngine 1 Philox
      randomEngineSetSeed e 100
      a1 <- toList <$> randomUniform @Float [64] e
      randomEngineSetSeed e 200
      a2 <- toList <$> randomUniform @Float [64] e
      a2 `shouldNotBe` a1
    it "same seed after reset produces the same stream" $ do
      e <- createRandomEngine 1 Philox
      randomEngineSetSeed e 777
      a1 <- toList <$> randomUniform @Float [64] e
      randomEngineSetSeed e 777
      a2 <- toList <$> randomUniform @Float [64] e
      a2 `shouldBe` a1

  describe "retainRandomEngine" $ do
    it "retained engine has the same type as the original" $ do
      e  <- createRandomEngine 42 Philox
      e' <- retainRandomEngine e
      getRandomEngineType e' `shouldReturn` Philox
    it "retained handle shares state with original (both advance the same stream)" $ do
      e  <- createRandomEngine 42 Philox
      e' <- retainRandomEngine e
      a1 <- toList <$> randomUniform @Double [4] e
      a2 <- toList <$> randomUniform @Double [4] e'
      a2 `shouldNotBe` a1

  describe "setDefaultRandomEngineType" $ do
    it "default engine type reflects the type that was set" $ do
      setDefaultRandomEngineType ThreeFry
      e <- getDefaultRandomEngine
      getRandomEngineType e `shouldReturn` ThreeFry
    it "switching type changes what getDefaultRandomEngine reports" $ do
      setDefaultRandomEngineType Philox
      e1 <- getDefaultRandomEngine
      t1 <- getRandomEngineType e1
      setDefaultRandomEngineType Mersenne
      e2 <- getDefaultRandomEngine
      t2 <- getRandomEngineType e2
      t1 `shouldBe` Philox
      t2 `shouldBe` Mersenne
