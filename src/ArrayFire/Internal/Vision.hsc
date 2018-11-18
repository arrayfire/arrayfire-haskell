module ArrayFire.Internal.Vision where

import ArrayFire.Internal.Defines

import Data.Word

import Data.Int

#include "vision.h"

#include "extra.h"

import Foreign.Ptr

foreign import ccall unsafe "af_harris"
    af_harris :: Ptr AFFeatures -> AFArray -> Word32 -> Float -> Float -> Word32 -> Float -> IO AFErr
foreign import ccall unsafe "af_orb"
    af_orb :: Ptr AFFeatures -> Ptr AFArray -> AFArray -> Float -> Word32 -> Float -> Word32 -> Bool -> IO AFErr
foreign import ccall unsafe "af_sift"
    af_sift :: Ptr AFFeatures -> Ptr AFArray -> AFArray -> Word32 -> Float -> Float -> Float -> Bool -> Float -> Float -> IO AFErr
foreign import ccall unsafe "af_gloh"
    af_gloh :: Ptr AFFeatures -> Ptr AFArray -> AFArray -> Word32 -> Float -> Float -> Float -> Bool -> Float -> Float -> IO AFErr
foreign import ccall unsafe "af_hamming_matcher"
    af_hamming_matcher :: Ptr AFArray -> Ptr AFArray -> AFArray -> AFArray -> Word64 -> Word32 -> IO AFErr
foreign import ccall unsafe "af_nearest_neighbour"
    af_nearest_neighbour :: Ptr AFArray -> Ptr AFArray -> AFArray -> AFArray -> Word64 -> Word32 -> AFMatchType -> IO AFErr
foreign import ccall unsafe "af_match_template"
    af_match_template :: Ptr AFArray -> AFArray -> AFArray -> AFMatchType -> IO AFErr
foreign import ccall unsafe "af_susan"
    af_susan :: Ptr AFFeatures -> AFArray -> Word32 -> Float -> Float -> Float -> Word32 -> IO AFErr
foreign import ccall unsafe "af_dog"
    af_dog :: Ptr AFArray -> AFArray -> Int -> Int -> IO AFErr
foreign import ccall unsafe "af_homography"
    af_homography :: Ptr AFArray -> Ptr Int -> AFArray -> AFArray -> AFArray -> AFArray -> AFHomographyType -> Float -> Word32 -> AFDtype -> IO AFErr