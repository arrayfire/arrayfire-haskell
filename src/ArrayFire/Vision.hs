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

susan
  :: Array a
  -> Int
  -> Float
  -> Float
  -> Float
  -> Int
  -> Features
susan (Array fptr) (fromIntegral -> a) b c d (fromIntegral -> e)
  = unsafePerformIO . mask_ . withForeignPtr fptr $ \inptr ->
      do feat <-
           alloca $ \aptr -> do
             throwAFError =<< af_susan aptr inptr a b c d e
             peek aptr
         Features <$> newForeignPtr af_release_features feat

dog
 :: Array a
 -> Int
 -> Int
 -> Array a
dog a (fromIntegral -> x) (fromIntegral -> y) = 
  op1 a (\p c -> af_dog p c x y)

homography
 :: forall a . AFType a
 => Array a
 -> Array a
 -> Array a
 -> Array a
 -> HomographyType
 -> Float
 -> Int
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
