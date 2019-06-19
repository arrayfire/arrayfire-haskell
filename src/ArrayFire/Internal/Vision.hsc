{-# LANGUAGE CPP #-}
module ArrayFire.Internal.Vision where

import ArrayFire.Internal.Defines
import ArrayFire.Internal.Types
import Data.Word
import Data.Int
import Foreign.Ptr
import Foreign.C.Types

#include "af/vision.h"
foreign import ccall unsafe "af_fast"
    af_fast :: Ptr AFFeatures -> AFArray -> Float -> CUInt -> CBool -> Float -> CUInt -> IO AFErr
foreign import ccall unsafe "af_harris"
    af_harris :: Ptr AFFeatures -> AFArray -> CUInt -> Float -> Float -> CUInt -> Float -> IO AFErr
foreign import ccall unsafe "af_orb"
    af_orb :: Ptr AFFeatures -> Ptr AFArray -> AFArray -> Float -> CUInt -> Float -> CUInt -> CBool -> IO AFErr
foreign import ccall unsafe "af_sift"
    af_sift :: Ptr AFFeatures -> Ptr AFArray -> AFArray -> CUInt -> Float -> Float -> Float -> CBool -> Float -> Float -> IO AFErr
foreign import ccall unsafe "af_gloh"
    af_gloh :: Ptr AFFeatures -> Ptr AFArray -> AFArray -> CUInt -> Float -> Float -> Float -> CBool -> Float -> Float -> IO AFErr
foreign import ccall unsafe "af_hamming_matcher"
    af_hamming_matcher :: Ptr AFArray -> Ptr AFArray -> AFArray -> AFArray -> DimT -> CUInt -> IO AFErr
foreign import ccall unsafe "af_nearest_neighbour"
    af_nearest_neighbour :: Ptr AFArray -> Ptr AFArray -> AFArray -> AFArray -> DimT -> CUInt -> AFMatchType -> IO AFErr
foreign import ccall unsafe "af_match_template"
    af_match_template :: Ptr AFArray -> AFArray -> AFArray -> AFMatchType -> IO AFErr
foreign import ccall unsafe "af_susan"
    af_susan :: Ptr AFFeatures -> AFArray -> CUInt -> Float -> Float -> Float -> CUInt -> IO AFErr
foreign import ccall unsafe "af_dog"
    af_dog :: Ptr AFArray -> AFArray -> CInt -> CInt -> IO AFErr
foreign import ccall unsafe "af_homography"
    af_homography :: Ptr AFArray -> Ptr CInt -> AFArray -> AFArray -> AFArray -> AFArray -> AFHomographyType -> Float -> CUInt -> AFDtype -> IO AFErr