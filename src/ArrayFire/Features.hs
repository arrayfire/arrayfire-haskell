{-# LANGUAGE ViewPatterns #-}
--------------------------------------------------------------------------------
-- |
-- Module      : ArrayFire.Features
-- Copyright   : David Johnson (c) 2019-2020
-- License     : BSD 3
-- Maintainer  : David Johnson <djohnson.m@gmail.com>
-- Stability   : Experimental
-- Portability : GHC
--
-- Features API for ArrayFire
--
--------------------------------------------------------------------------------
module ArrayFire.Features where

import Foreign.Marshal
import Foreign.Storable
import Foreign.ForeignPtr
import System.IO.Unsafe

import ArrayFire.Internal.Features
import ArrayFire.Types
import ArrayFire.FFI
import ArrayFire.Exception

-- | Construct 'Features'
createFeatures
  :: Int
  -> Features
createFeatures (fromIntegral -> n) =
  unsafePerformIO $ do
    ptr <-
      alloca $ \ptrInput -> do
        throwAFError =<< ptrInput `af_create_features` n
        peek ptrInput
    fptr <- newForeignPtr af_release_features ptr
    pure (Features fptr)

-- | Retain 'Features'
retainFeatures
  :: Features
  -> Features
retainFeatures = (`op1f` af_retain_features)

-- | Get number of 'Feature's
getFeaturesNum
  :: Features
  -> Int
getFeaturesNum = fromIntegral . (`infoFromFeatures` af_get_features_num)

-- | Get 'Feature' X-position
getFeaturesXPos
  :: Features
  -> Array a
getFeaturesXPos = (`featuresToArray` af_get_features_xpos)

-- | Get 'Feature' Y-position
getFeaturesYPos
  :: Features
  -> Array a
getFeaturesYPos = (`featuresToArray` af_get_features_ypos)

-- | Get 'Feature' Score
getFeaturesScore
  :: Features
  -> Array a
getFeaturesScore = (`featuresToArray` af_get_features_score)

-- | Get 'Feature' orientation
getFeaturesOrientation
  :: Features
  -> Array a
getFeaturesOrientation = (`featuresToArray` af_get_features_orientation)

-- | Get 'Feature' size
getFeaturesSize
  :: Features
  -> Array a
getFeaturesSize = (`featuresToArray` af_get_features_size)
