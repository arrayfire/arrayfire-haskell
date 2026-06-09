{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module ArrayFire.GraphicsSpec where

import           ArrayFire (Cell(..), ColorMap(..))
import           Test.Hspec

spec :: Spec
spec = describe "Graphics spec" $ do

  -- The 'Cell' render-descriptor is a pure record and is always testable,
  -- with or without a display.
  --
  -- The window operations (createWindow, setTitle, ...) are intentionally
  -- not exercised here: they require a live OpenGL/forge context and abort
  -- the process with a SIGSEGV on headless machines. A segfault is not a
  -- catchable Haskell exception, so there is no safe way to probe them in an
  -- automated suite.
  describe "Cell" $ do
    let cell = Cell 1 2 "chart" ColorMapSpectrum

    it "exposes its fields" $ do
      cellRow cell      `shouldBe` 1
      cellCol cell      `shouldBe` 2
      cellTitle cell    `shouldBe` "chart"
      cellColorMap cell `shouldBe` ColorMapSpectrum

    it "has a lawful Eq instance" $ do
      cell `shouldBe` Cell 1 2 "chart" ColorMapSpectrum
      cell `shouldNotBe` Cell 1 2 "chart" ColorMapHeat

    it "carries each ColorMap through a record update" $
      -- ColorMap derives Enum (not Bounded); enumFrom runs to the last ctor
      map (cellColorMap . \c -> cell { cellColorMap = c }) [ColorMapDefault ..]
        `shouldBe` ([ColorMapDefault ..] :: [ColorMap])
