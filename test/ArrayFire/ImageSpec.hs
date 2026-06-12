{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module ArrayFire.ImageSpec where

import qualified ArrayFire as A
import           ArrayFire.Exception       (AFException (..), AFExceptionType (..))
import           Control.Exception        (bracket, finally, try, throwIO)
import           System.Directory         (getTemporaryDirectory, removeFile)
import           System.IO                (openTempFile, hClose)
import           Test.Hspec
import           Test.Hspec.ApproxExpect

-- | A 4×4 single-channel constant image.
gray :: A.Array Float
gray = A.constant @Float [4,4] 1.0

-- | A 4×4×3 three-channel (RGB) constant image.
rgb :: A.Array Float
rgb = A.constant @Float [4,4,3] 1.0

spec :: Spec
spec = describe "Image spec" $ do
  describe "isImageIOAvailable" $
    it "reports whether FreeImage support was compiled in" $
      -- value is build-dependent; we only assert the call succeeds & is Bool
      (A.isImageIOAvailable >>= (`shouldSatisfy` (`elem` [True, False])))

  describe "gaussianKernel" $ do
    it "produces a kernel of the requested dimensions" $
      A.getDims (A.gaussianKernel @Float 3 5 0 0) `shouldBe` (3,5,1,1)

    it "is normalized to sum ~1" $
      sum (A.toList (A.gaussianKernel @Float 5 5 0 0)) `shouldBeApprox` (1.0 :: Float)

    it "has only non-negative weights" $
      A.toList (A.gaussianKernel @Float 5 5 0 0) `shouldSatisfy` all (>= 0)

  describe "resize" $ do
    it "upsamples to the requested dimensions" $
      A.getDims (A.resize gray 8 8 A.Nearest) `shouldBe` (8,8,1,1)

    it "downsamples to the requested dimensions" $
      A.getDims (A.resize gray 2 2 A.Bilinear) `shouldBe` (2,2,1,1)

    it "preserves a constant image under bilinear resize" $
      A.toList (A.resize gray 8 8 A.Bilinear) `shouldSatisfy` all (`approx` 1.0)

  describe "colorspace conversion" $ do
    it "rgb2gray collapses the channel dimension" $
      A.getDims (A.rgb2gray rgb 0.3 0.59 0.11) `shouldBe` (4,4,1,1)

    it "rgb2gray of a constant image yields the weighted intensity" $
      A.toList (A.rgb2gray rgb 0.3 0.59 0.11) `shouldSatisfy` all (`approx` 1.0)

    it "gray2rgb expands to three channels" $
      A.getDims (A.gray2rgb gray 1 1 1) `shouldBe` (4,4,3,1)

    it "rgb2ycbcr / ycbcr2rgb preserve image dimensions" $ do
      let ycbcr = A.rgb2ycbcr rgb A.Ycc601
      A.getDims ycbcr `shouldBe` (4,4,3,1)
      A.getDims (A.ycbcr2rgb ycbcr A.Ycc601) `shouldBe` (4,4,3,1)

  describe "morphology" $ do
    it "dilation with an all-ones mask leaves a constant image unchanged" $ do
      let mask = A.constant @Float [3,3] 1.0
      A.toList (A.dilate gray mask) `shouldSatisfy` all (`approx` 1.0)

    it "erosion with an all-ones mask leaves a constant image unchanged" $ do
      let mask = A.constant @Float [3,3] 1.0
      A.toList (A.erode gray mask) `shouldSatisfy` all (`approx` 1.0)

  describe "histogram" $ do
    it "has one element per requested bin" $
      A.getElements (A.histogram gray 16 0 1) `shouldBe` 16

    it "produces a u32 array" $
      A.getType (A.histogram gray 16 0 1) `shouldBe` A.U32

    it "accumulates every pixel across all bins" $
      sum (map fromIntegral (A.toList (A.histogram gray 16 0 1)))
        `shouldBe` (16 :: Int)  -- 4×4 pixels

  describe "gradient" $
    it "of a constant image is zero in both directions" $ do
      let (gx, gy) = A.gradient gray
      A.toList gx `shouldSatisfy` all (`approx` 0.0)
      A.toList gy `shouldSatisfy` all (`approx` 0.0)

  describe "summed area table (sat)" $ do
    it "preserves the image dimensions" $
      A.getDims (A.sat gray) `shouldBe` (4,4,1,1)

    it "bottom-right cell holds the total sum" $
      -- column-major: last element is the integral over the whole image
      last (A.toList (A.sat gray)) `shouldBeApprox` (16.0 :: Float)

  describe "moments" $ do
    it "M00 of a constant image equals its total intensity (area)" $
      case A.momentsAll gray A.M00 of
        [m00] -> m00 `shouldBeApprox` (16.0 :: Double)
        ms    -> expectationFailure ("expected one moment, got " <> show ms)
    it "FirstOrder returns all four moments without corrupting memory" $
      length (A.momentsAll gray A.FirstOrder) `shouldBe` 4

  describe "Image I/O" $ do
    it "saveImage/loadImage round-trips a grayscale image" $ do
      avail <- A.isImageIOAvailable
      if not avail then pending else do
        res <- try $ withTempPng $ \path -> do
          A.saveImage gray path
          img <- A.loadImage @Float path False
          A.getDims img `shouldBe` (4,4,1,1)
          A.toList img `shouldSatisfy` all (`approx` 1.0)
        case res of
          Left (AFException LoadLibError _ _) -> pending
          Left e                              -> throwIO e
          Right ()                            -> return ()

    it "saveImage/loadImage round-trips a colour image" $ do
      avail <- A.isImageIOAvailable
      if not avail then pending else do
        res <- try $ withTempPng $ \path -> do
          A.saveImage rgb path
          img <- A.loadImage @Float path True
          A.getDims img `shouldBe` (4,4,3,1)
          A.toList img `shouldSatisfy` all (`approx` 1.0)
        case res of
          Left (AFException LoadLibError _ _) -> pending
          Left e                              -> throwIO e
          Right ()                            -> return ()

    it "saveImageNative/loadImageNative round-trips dims" $ do
      avail <- A.isImageIOAvailable
      if not avail then pending else do
        res <- try $ withTempPng $ \path -> do
          A.saveImageNative gray path
          img <- A.loadImageNative @Float path
          let (r, c, _, _) = A.getDims img
          (r, c) `shouldBe` (4, 4)
        case res of
          Left (AFException LoadLibError _ _) -> pending
          Left e                              -> throwIO e
          Right ()                            -> return ()

  where
    -- relative+absolute tolerance check, returning Bool for use with `all`
    approx :: Float -> Float -> Bool
    approx x e = abs (x - e) <= 1e-8 + 1e-5 * max (abs x) (abs e)

    withTempPng :: (FilePath -> IO a) -> IO a
    withTempPng action =
      bracket
        (do tmp <- getTemporaryDirectory
            (path, h) <- openTempFile tmp "af_test.png"
            hClose h
            pure path)
        removeFile
        action
