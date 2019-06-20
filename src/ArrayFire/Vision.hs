{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}
--------------------------------------------------------------------------------
-- |
-- Module      : ArrayFire.Vision
-- Copyright   : David Johnson (c) 2019-2020
-- License     : BSD 3
-- Maintainer  : David Johnson <djohnson.m@gmail.com>
-- Stability   : Experimental
-- Portability : GHC
--
--------------------------------------------------------------------------------
module ArrayFire.Vision where

import Control.Exception           hiding (TypeError)
import Data.Typeable
import Foreign.ForeignPtr
import Foreign.Marshal
import Foreign.Storable
import System.IO.Unsafe

import ArrayFire.Exception
import ArrayFire.FFI
import ArrayFire.Internal.Features
import ArrayFire.Internal.Vision
import ArrayFire.Types

-- | FAST feature detectors
--
-- A circle of radius 3 pixels, translating into a total of 16 pixels, is checked for sequential segments of pixels much brighter or much darker than the central one.
-- For a pixel p to be considered a feature, there must exist a sequential segment of arc_length pixels in the circle around it such that all are greather than (p + thr) or smaller than (p - thr).
-- After all features in the image are detected, if nonmax is true, the non-maximal suppression is applied, checking all detected features and the features detected in its 8-neighborhood and discard it if its score is non maximal.
fast
  :: Array a
  -- ^ Array containing a grayscale image (color images are not supported)
  -> Float
  -- ^ FAST threshold for which a pixel of the circle around the central pixel is considered to be greater or smaller
  -> Int
  -- ^ Length of arc (or sequential segment) to be tested, must be within range [9-16]
  -> Bool
  -- ^ Performs non-maximal suppression if true
  -> Float
  -- ^ Maximum ratio of features to detect, the maximum number of features is calculated by feature_ratio * in.elements(). The maximum number of features is not based on the score, instead, features detected after the limit is reached are discarded
  -> Int
  -- ^ Is the length of the edges in the image to be discarded by FAST (minimum is 3, as the radius of the circle)
  -> Features
  -- ^ Struct containing arrays for x and y coordinates and score, while array orientation is set to 0 as FAST does not compute orientation, and size is set to 1 as FAST does not compute multiple scales
fast (Array fptr) thr (fromIntegral -> arc) (fromIntegral . fromEnum -> non) ratio (fromIntegral -> edge)
  = unsafePerformIO . mask_ . withForeignPtr fptr $ \aptr ->
      do feat <- alloca $ \ptr -> do
           throwAFError =<< af_fast ptr aptr thr arc non ratio edge
           peek ptr
         Features <$>
           newForeignPtr af_release_features feat

harris
  :: Array a
  -> Int
  -> Float
  -> Float
  -> Int
  -> Float
  -> Features
harris (Array fptr) (fromIntegral -> maxc) minresp sigma (fromIntegral -> bs) thr
  = unsafePerformIO . mask_ . withForeignPtr fptr $ \aptr ->
      do feat <- alloca $ \ptr -> do
           throwAFError =<< af_harris ptr aptr maxc minresp sigma bs thr
           peek ptr
         Features <$>
           newForeignPtr af_release_features feat

orb
  :: Array a
  -> Float
  -> Int
  -> Float
  -> Int
  -> Bool
  -> (Features, Array a)
orb (Array fptr) thr (fromIntegral -> feat) scl (fromIntegral -> levels) (fromIntegral . fromEnum -> blur)
  = unsafePerformIO . mask_ . withForeignPtr fptr $ \inptr ->
      do (feature, arr) <-
           alloca $ \aptr -> do
             alloca $ \bptr -> do
               throwAFError =<< af_orb aptr bptr inptr thr feat scl levels blur
               (,) <$> peek aptr <*> peek bptr
         feats <- Features <$> newForeignPtr af_release_features feature
         array <- Array <$> newForeignPtr af_release_array_finalizer arr
         pure (feats, array)

sift
  :: Array a
  -> Int
  -> Float
  -> Float
  -> Float
  -> Bool
  -> Float
  -> Float
  -> (Features, Array a)
sift (Array fptr) (fromIntegral -> a) b c d (fromIntegral . fromEnum -> e) f g
  = unsafePerformIO . mask_ . withForeignPtr fptr $ \inptr ->
      do (feat, arr) <-
           alloca $ \aptr -> do
             alloca $ \bptr -> do
               throwAFError =<< af_sift aptr bptr inptr a b c d e f g
               (,) <$> peek aptr <*> peek bptr
         feats <- Features <$> newForeignPtr af_release_features feat
         array <- Array <$> newForeignPtr af_release_array_finalizer arr
         pure (feats, array)

gloh
  :: Array a
  -> Int
  -> Float
  -> Float
  -> Float
  -> Bool
  -> Float
  -> Float
  -> (Features, Array a)
gloh (Array fptr) (fromIntegral -> a) b c d (fromIntegral . fromEnum -> e) f g
  = unsafePerformIO . mask_ . withForeignPtr fptr $ \inptr ->
      do (feat, arr) <-
           alloca $ \aptr -> do
             alloca $ \bptr -> do
               throwAFError =<< af_gloh aptr bptr inptr a b c d e f g
               (,) <$> peek aptr <*> peek bptr
         feats <- Features <$> newForeignPtr af_release_features feat
         array <- Array <$> newForeignPtr af_release_array_finalizer arr
         pure (feats, array)

hammingMatcher
 :: Array a
 -> Array a
 -> Int
 -> Int
 -> (Array a, Array a)
hammingMatcher a b (fromIntegral -> x) (fromIntegral -> y)
  = op2p2 a b (\p c d e -> af_hamming_matcher p c d e x y)

nearestNeighbor
 :: Array a
 -> Array a
 -> Int
 -> Int
 -> MatchType
 -> (Array a, Array a)
nearestNeighbor a b (fromIntegral -> x) (fromIntegral -> y) (fromMatchType -> match)
  = op2p2 a b (\p c d e -> af_nearest_neighbour p c d e x y match)

matchTemplate
 :: Array a
 -> Array a
 -> MatchType
 -> Array a
matchTemplate a b (fromMatchType -> match)
  = op2 a b (\p c d -> af_match_template p c d match)

-- | SUSAN corner detector.
--
-- SUSAN is an acronym standing for Smallest Univalue Segment Assimilating Nucleus. This method places a circular disc over the pixel to be tested (a.k.a nucleus) to compute the corner measure of that corresponding pixel. The region covered by the circular disc is M, and a pixel in this region is represented by m   M where m  0 is the nucleus. Every pixel in the region is compared to the nucleus using the following comparison function:
--
-- c(m  )=e ((I(m  ) I(m  0))/t)6
-- where t is radius of the region, I is the brightness of the pixel.
--
-- Response of SUSAN operator is given by the following equation:
--
-- R(M)={g n(M)if n(M)<g0otherwise,
-- where n(M)= m   Mc(m  ), g is named the geometric threshold and n is the number of pixels in the mask which are within t of the nucleus.
--
-- Importance of the parameters, t and g is explained below:
--
-- t determines how similar points have to be to the nucleusbefore they are considered to be a part of the univalue segment
--
-- g determines the minimum size of the univalue segment. For a large enough g, SUSAN operator becomes an edge dectector.
susan
  :: Array a
  -- ^ is input grayscale/intensity image
  -> Int
  -- ^ nucleus radius for each pixel neighborhood
  -> Float
  -- ^ intensity difference threshold a.k.a t from equations in description
  -> Float
  -- ^ geometric threshold
  -> Float
  -- ^ is maximum number of features that will be returned by the function
  -> Int
  -- ^ indicates how many pixels width area should be skipped for corner detection
  -> Features
susan (Array fptr) (fromIntegral -> a) b c d (fromIntegral -> e)
  = unsafePerformIO . mask_ . withForeignPtr fptr $ \inptr ->
      do feat <-
           alloca $ \aptr -> do
             throwAFError =<< af_susan aptr inptr a b c d e
             peek aptr
         Features <$> newForeignPtr af_release_features feat

-- | Difference of Gaussians.
--
-- Given an image, this function computes two different versions of smoothed input image using the difference smoothing parameters and subtracts one from the other and returns the result.
--
dog
 :: Array a
 -- ^ is input image
 -> Int
 -- ^ is the radius of first gaussian kernel
 -> Int
 -- ^ is the radius of second gaussian kernel
 -> Array a
 -- ^ is difference of smoothed inputs
dog a (fromIntegral -> x) (fromIntegral -> y) =
  op1 a (\p c -> af_dog p c x y)

-- | Homography Estimation.
--
-- Homography estimation find a perspective transform between two sets of 2D points.
-- Currently, two methods are supported for the estimation, RANSAC (RANdom SAmple Consensus)
-- and LMedS (Least Median of Squares). Both methods work by randomly selecting a subset of 4 points
-- of the set of source points, computing the eigenvectors of that set and finding the perspective transform.
-- The process is repeated several times, a maximum of times given by the value passed to the iterations arguments
-- for RANSAC (for the CPU backend, usually less than that, depending on the quality of the dataset,
-- but for CUDA and OpenCL backends the transformation will be computed exactly the amount of times passed via
-- the iterations parameter), the returned value is the one that matches the best number of inliers, which are
-- all of the points that fall within a maximum L2 distance from the value passed to the inlier_thr argument.
-- For the LMedS case, the number of iterations is currently hardcoded to meet the following equation:
--
-- m=log(1−P)log[1−(1−ϵ)p],
--
-- where P=0.99, ϵ=40% and p=4.
homography
 :: forall a . AFType a
 => Array a
 -- ^ x coordinates of the source points.
 -> Array a
 -- ^ y coordinates of the source points.
 -> Array a
 -- ^ x coordinates of the destination points.
 -> Array a
 -- ^ y coordinates of the destination points.
 -> HomographyType
 -- ^ htype, can be AF_HOMOGRAPHY_RANSAC, for which a
 -- RANdom SAmple Consensus will be used to evaluate
 -- the homography quality (e.g., number of inliers),
 -- or AF_HOMOGRAPHY_LMEDS, which will use
 -- Least Median of Squares method to evaluate homography quality.
 -> Float
  -- ^ If htype is AF_HOMOGRAPHY_RANSAC, this parameter will five the maximum L2-distance for a point to be considered an inlier.
 -> Int
  -- ^ maximum number of iterations when htype is AF_HOMOGRAPHY_RANSAC and backend is CPU, if backend is CUDA or OpenCL, iterations is the total number of iterations, an iteration is a selection of 4 random points for which the homography is estimated and evaluated for number of inliers.
 -> (Int, Array a)
homography
  (Array a)
  (Array b)
  (Array c)
  (Array d)
  (fromHomographyType -> homo)
  inlier
  (fromIntegral -> iterations) = do
    unsafePerformIO . mask_ $ do
      withForeignPtr a $ \aptr ->
        withForeignPtr b $ \bptr ->
          withForeignPtr c $ \cptr ->
            withForeignPtr d $ \dptr -> do
              alloca $ \outPtrA ->
                alloca $ \outPtrI -> do
                  throwAFError =<<
                    af_homography
                      outPtrA
                      outPtrI
                      aptr
                      bptr
                      cptr
                      dptr
                      homo
                      inlier
                      iterations
                      dtype
                  arrayPtr <- peek outPtrA
                  (,) <$> do fromIntegral <$> peek outPtrI
                      <*> do Array <$> newForeignPtr af_release_array_finalizer arrayPtr
    where
      dtype = afType (Proxy @ a)
