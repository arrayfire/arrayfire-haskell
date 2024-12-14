{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}
--------------------------------------------------------------------------------
-- |
-- Module      : ArrayFire.Vision
-- Copyright   : David Johnson (c) 2019-2020
-- License     : BSD 3
-- Maintainer  : David Johnson <code@dmj.io>
-- Stability   : Experimental
-- Portability : GHC
--
-- Functions pertaining to Computer Vision.
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
import ArrayFire.Internal.Types

-- | FAST feature detectors
--
-- [ArrayFire Docs](http://arrayfire.org/docs/group__cv__func__fast.htm)
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

-- | Harris corner detection
--
-- [ArrayFire Docs](http://arrayfire.org/docs/group__cv__func__harris.htm)
--
-- Harris corner detector.
--
harris
  :: Array a
  -- ^ array containing a grayscale image (color images are not supported)
  -> Int
  -- ^ maximum number of corners to keep, only retains those with highest Harris responses
  -> Float
  -- ^ minimum response in order for a corner to be retained, only used if max_corners = 0
  -> Float
  -- ^ the standard deviation of a circular window (its dimensions will be calculated according to the standard deviation), the covariation matrix will be calculated to a circular neighborhood of this standard deviation (only used when block_size == 0, must be >= 0.5f and <= 5.0f)
  -> Int
  -- ^ square window size, the covariation matrix will be calculated to a square neighborhood of this size (must be >= 3 and <= 31)
  -> Float
  -- ^ struct containing arrays for x and y coordinates and score (Harris response), while arrays orientation and size are set to 0 and 1, respectively, because Harris does not compute that information
  -> Features
harris (Array fptr) (fromIntegral -> maxc) minresp sigma (fromIntegral -> bs) thr
  = unsafePerformIO . mask_ . withForeignPtr fptr $ \aptr ->
      do feat <- alloca $ \ptr -> do
           throwAFError =<< af_harris ptr aptr maxc minresp sigma bs thr
           peek ptr
         Features <$>
           newForeignPtr af_release_features feat

-- | ORB Feature descriptor
--
-- [ArrayFire Docs](http://arrayfire.org/docs/group__cv__func__orb.htm)
--
-- Extract ORB descriptors from FAST features that hold higher Harris responses. FAST does not compute orientation, thus, orientation of features is calculated using the intensity centroid. As FAST is also not multi-scale enabled, a multi-scale pyramid is calculated by downsampling the input image multiple times followed by FAST feature detection on each scale.
-- 
orb
  :: Array a
  -- ^ 'Array' containing a grayscale image (color images are not supported)
  -> Float
  -- ^ FAST threshold for which a pixel of the circle around the central pixel is considered to be brighter or darker
  -> Int
  -- ^ maximum number of features to hold (will only keep the max_feat features with higher Harris responses)
  -> Float
  -- ^ factor to downsample the input image, meaning that each level will hold prior level dimensions divided by scl_fctr
  -> Int
  -- ^ number of levels to be computed for the image pyramid
  -> Bool
  -- ^ blur image with a Gaussian filter with sigma=2 before computing descriptors to increase robustness against noise if true
  -> (Features, Array a)
  -- ^ 'Features' struct composed of arrays for x and y coordinates, score, orientation and size of selected features
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

-- | SIFT feature detector and descriptor extractor.
--
-- [ArrayFire Docs](http://arrayfire.org/docs/group__cv__func__sift.htm)
--
-- C Interface for SIFT feature detector and descriptor.
-- 
sift
  :: Array a
  -- ^ Array containing a grayscale image (color images are not supported)
  -> Int
  -- ^ number of layers per octave, the number of octaves is computed automatically according to the input image dimensions, the original SIFT paper suggests 3
  -> Float
  -- ^ threshold used to filter out features that have low contrast, the original SIFT paper suggests 0.04
  -> Float
  -- ^ threshold used to filter out features that are too edge-like, the original SIFT paper suggests 10.0
  -> Float
  -- ^ the sigma value used to filter the input image at the first octave, the original SIFT paper suggests 1.6
  -> Bool
  -- ^ if true, the input image dimensions will be doubled and the doubled image will be used for the first octave
  -> Float
  -- ^ the inverse of the difference between the minimum and maximum grayscale intensity value, e.g.: if the ranges are 0-256, the proper intensity_scale value is 1/256, if the ranges are 0-1, the proper intensity-scale value is 1/1
  -> Float
  -- ^ maximum ratio of features to detect, the maximum number of features is calculated by feature_ratio * in.elements(). The maximum number of features is not based on the score, instead, features detected after the limit is reached are discarded
  -> (Features, Array a)
  -- ^ Features object composed of arrays for x and y coordinates, score, orientation and size of selected features
  -- Nx128 array containing extracted descriptors, where N is the number of features found by SIFT
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

-- | SIFT feature detector and descriptor extractor.
--
-- [ArrayFire Docs](http://arrayfire.org/docs/group__cv__func__gloh.htm)
--
-- C Interface for SIFT feature detector and descriptor.
-- 
gloh
  :: Array a
  -- ^ 'Array' containing a grayscale image (color images are not supported)
  -> Int
  -- ^ number of layers per octave, the number of octaves is computed automatically according to the input image dimensions, the original SIFT paper suggests 3
  -> Float
  -- ^ threshold used to filter out features that have low contrast, the original SIFT paper suggests 0.04
  -> Float
  -- ^ threshold used to filter out features that are too edge-like, the original SIFT paper suggests 10.0
  -> Float
  -- ^ the sigma value used to filter the input image at the first octave, the original SIFT paper suggests 1.6
  -> Bool
  -- ^ if true, the input image dimensions will be doubled and the doubled image will be used for the first octave
  -> Float
  -- ^ the inverse of the difference between the minimum and maximum grayscale intensity value, e.g.: if the ranges are 0-256, the proper intensity_scale value is 1/256, if the ranges are 0-1, the proper intensity-scale value is 1/1
  -> Float
  -- ^ maximum ratio of features to detect, the maximum number of features is calculated by feature_ratio * in.elements(). The maximum number of features is not based on the score, instead, features detected after the limit is reached are discarded
  -> (Features, Array a)
  -- ^ 'Features' object composed of arrays for x and y coordinates, score, orientation and size of selected features
  -- ^ Nx272 array containing extracted GLOH descriptors, where N is the number of features found by SIFT
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

-- | Hamming Matcher
--
-- [ArrayFire Docs](http://arrayfire.org/docs/group__cv__func__hamming__matcher.htm)
--
-- Calculates Hamming distances between two 2-dimensional arrays containing features, one of the arrays containing the training data and the other the query data. One of the dimensions of the both arrays must be equal among them, identifying the length of each feature. The other dimension indicates the total number of features in each of the training and query arrays. Two 1-dimensional arrays are created as results, one containg the smallest N distances of the query array and another containing the indices of these distances in the training array. The resulting 1-dimensional arrays have length equal to the number of features contained in the query array.
-- 
hammingMatcher
 :: Array a
 -- ^ is the 'Array' containing the data to be queried
 -> Array a
 -- ^ is the 'Array' containing the data used as training data
 -> Int
 -- ^ indicates the dimension to analyze for distance (the dimension indicated here must be of equal length for both query and train arrays)
 -> Int
 -- ^ is the number of smallest distances to return (currently, only 1 is supported)
 -> (Array a, Array a)
 -- ^ is an array of MxN size, where M is equal to the number of query features and N is equal to n_dist. The value at position IxJ indicates the index of the Jth smallest distance to the Ith query value in the train data array. the index of the Ith smallest distance of the Mth query.
 -- is an array of MxN size, where M is equal to the number of query features and N is equal to n_dist. The value at position IxJ indicates the Hamming distance of the Jth smallest distance to the Ith query value in the train data array.
hammingMatcher a b (fromIntegral -> x) (fromIntegral -> y)
  = op2p2 a b (\p c d e -> af_hamming_matcher p c d e x y)

-- | Nearest Neighbor
--
-- [ArrayFire Docs](http://arrayfire.org/docs/group__cv__func__nearest__neighbour.htm)
--
-- Calculates nearest distances between two 2-dimensional arrays containing features based on the type of distance computation chosen. Currently, AF_SAD (sum of absolute differences), AF_SSD (sum of squared differences) and AF_SHD (hamming distance) are supported. One of the arrays containing the training data and the other the query data. One of the dimensions of the both arrays must be equal among them, identifying the length of each feature. The other dimension indicates the total number of features in each of the training and query arrays. Two 1-dimensional arrays are created as results, one containg the smallest N distances of the query array and another containing the indices of these distances in the training array. The resulting 1-dimensional arrays have length equal to the number of features contained in the query array.
-- 
nearestNeighbor
 :: Array a
 -- ^ is the array containing the data to be queried
 -> Array a
 -- ^ is the array containing the data used as training data
 -> Int
 -- ^ indicates the dimension to analyze for distance (the dimension indicated here must be of equal length for both query and train arrays)
 -> Int
 -- ^ is the number of smallest distances to return (currently, only values <= 256 are supported)
 -> MatchType
 -- ^ is the distance computation type. Currently AF_SAD (sum of absolute differences), AF_SSD (sum of squared differences), and AF_SHD (hamming distances) are supported.
 -> (Array a, Array a)
 -- ^ is an array of MxN size, where M is equal to the number of query features and N is equal to n_dist. The value at position IxJ indicates the index of the Jth smallest distance to the Ith query value in the train data array. the index of the Ith smallest distance of the Mth query.
 -- is an array of MxN size, where M is equal to the number of query features and N is equal to n_dist. The value at position IxJ indicates the distance of the Jth smallest distance to the Ith query value in the train data array based on the dist_type chosen.
nearestNeighbor a b (fromIntegral -> x) (fromIntegral -> y) (fromMatchType -> match)
  = op2p2 a b (\p c d e -> af_nearest_neighbour p c d e x y match)

-- | Nearest Neighbor
--
-- [ArrayFire Docs](http://arrayfire.org/docs/group__cv__func__match__template.htm)
--
-- C Interface for image template matching.
-- 
matchTemplate
 :: Array a
 -- ^ is an 'Array' with image data
 -> Array a
 -- ^ is the template we are looking for in the image
 -> MatchType
 -- ^ is metric that should be used to calculate the disparity between window in the image and the template image. It can be one of the values defined by the enum af_match_type
 -> Array a
 -- ^ will have disparity values for the window starting at corresponding pixel position
matchTemplate a b (fromMatchType -> match)
  = op2 a b (\p c d -> af_match_template p c d match)

-- | SUSAN corner detector.
--
-- [ArrayFire Docs](http://arrayfire.org/docs/group__cv__func__susan.htm)
--
-- SUSAN is an acronym standing for Smallest Univalue Segment Assimilating Nucleus. This method places a circular disc over the pixel to be tested (a.k.a nucleus) to compute the corner measure of that corresponding pixel. The region covered by the circular disc is M, and a pixel in this region is represented by m   M where m  0 is the nucleus. Every pixel in the region is compared to the nucleus using the following comparison function:
--
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
-- [ArrayFire Docs](http://arrayfire.org/docs/group__cv__func__dog.htm)
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
-- [ArrayFire Docs](http://arrayfire.org/docs/group__cv__func__homography.htm)
--
-- Homography estimation find a perspective transform between two sets of 2D points.
--
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
 -- ^ is a 3x3 array containing the estimated homography.
 -- is the number of inliers that the homography was estimated to comprise, in the case that htype is AF_HOMOGRAPHY_RANSAC, a higher inlier_thr value will increase the estimated inliers. Note that if the number of inliers is too low, it is likely that a bad homography will be returned.
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
      dtype = afType (Proxy @a)
