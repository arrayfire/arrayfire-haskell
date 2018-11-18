module ArrayFire.Internal.Features where

import ArrayFire.Internal.Defines

import Data.Word

import Data.Int

#include "features.h"

#include "extra.h"

import Foreign.Ptr

foreign import ccall unsafe "af_retain_features"
    af_retain_features :: Ptr AFFeatures -> AFFeatures -> IO AFErr
foreign import ccall unsafe "af_get_features_num"
    af_get_features_num :: Ptr Word64 -> AFFeatures -> IO AFErr
foreign import ccall unsafe "af_get_features_xpos"
    af_get_features_xpos :: Ptr AFArray -> AFFeatures -> IO AFErr
foreign import ccall unsafe "af_get_features_ypos"
    af_get_features_ypos :: Ptr AFArray -> AFFeatures -> IO AFErr
foreign import ccall unsafe "af_get_features_score"
    af_get_features_score :: Ptr AFArray -> AFFeatures -> IO AFErr
foreign import ccall unsafe "af_get_features_orientation"
    af_get_features_orientation :: Ptr AFArray -> AFFeatures -> IO AFErr
foreign import ccall unsafe "af_get_features_size"
    af_get_features_size :: Ptr AFArray -> AFFeatures -> IO AFErr
foreign import ccall unsafe "af_release_features"
    af_release_features :: AFFeatures -> IO AFErr