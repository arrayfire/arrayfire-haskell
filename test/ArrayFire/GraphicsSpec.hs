{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module ArrayFire.GraphicsSpec where

import           Control.Exception (SomeException, try)
import qualified ArrayFire as A
import           ArrayFire (Cell(..), ColorMap(..))
import           Test.Hspec

-- | Run a window-dependent action, marking the example pending (rather than
-- failing) when no display / forge backend is available — as is the case on
-- headless CI. A genuine window action that throws still surfaces here.
withWindowOr :: IO a -> (a -> Expectation) -> Expectation
withWindowOr acquire k = do
  r <- try @SomeException acquire
  case r of
    Left _  -> pendingWith "no display / forge backend available"
    Right a -> k a

spec :: Spec
spec = describe "Graphics spec" $ do

  -- The 'Cell' render-descriptor is a pure record and is always testable,
  -- with or without a display.
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

  -- Window operations require an OpenGL context; guarded so headless runs
  -- report 'pending' instead of failing.
  describe "Window (requires a display)" $ do
    it "creates a window" $
      withWindowOr (A.createWindow 320 240 "test window") $ \_ ->
        pure ()  -- reaching here without an exception is success

    it "is not reported closed immediately after creation" $
      withWindowOr (A.createWindow 320 240 "test window") $ \w ->
        A.isWindowClosed w `shouldReturn` False

    it "accepts title / size / position / visibility updates" $
      withWindowOr (A.createWindow 320 240 "test window") $ \w -> do
        A.setTitle w "renamed"
        A.setSize w 640 480
        A.setPosition w 10 10
        A.setVisibility w False
        -- the window is still live (operations did not throw)
        A.isWindowClosed w `shouldReturn` False
