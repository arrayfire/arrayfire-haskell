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
-- Functions for constructing and querying 'Features'
--
-- @
-- >>> createFeatures 10
-- @
--
--------------------------------------------------------------------------------
module ArrayFire.Features where

import Foreign.Marshal
import Foreign.Storable
import Foreign.ForeignPtr
import System.IO.Unsafe

import ArrayFire.Internal.Features
import ArrayFire.Internal.Types
import ArrayFire.FFI
import ArrayFire.Exception

-- | Construct Features
--
-- >>> features = createFeatures 10
--
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

-- | Retain Features
--
-- >>> features = retainFeatures (createFeatures 10)
--
retainFeatures
  :: Features
  -> Features
retainFeatures = (`op1f` af_retain_features)

-- | Get number of Features
--
-- link
--
-- >>> getFeaturesNum (createFeatures 10)
-- 10
--
getFeaturesNum
  :: Features
  -> Int
getFeaturesNum = fromIntegral . (`infoFromFeatures` af_get_features_num)

-- | Get Feature X-position
--
-- >>> getFeaturesXPos (createFeatures 10)
-- ArrayFire Array
-- [10 1 1 1]
--     0.0000
--     1.8750
--     0.0000
--     2.3750
--     0.0000
--     2.5938
--     0.0000
--     2.0000
--     0.0000
--     2.4375
getFeaturesXPos
  :: Features
  -> Array a
getFeaturesXPos = (`featuresToArray` af_get_features_xpos)

-- | Get Feature Y-position
--
-- >>> getFeaturesYPos (createFeatures 10)
-- ArrayFire Array
-- [10 1 1 1]
--        nan
--        nan
--        nan
--        nan
--        nan
--        nan
--        nan
--        nan
--        nan
--        nan
getFeaturesYPos
  :: Features
  -> Array a
getFeaturesYPos = (`featuresToArray` af_get_features_ypos)

-- | Get Feature Score
--
-- >>> getFeaturesScore (createFeatures 10)
-- ArrayFire Array
-- [10 1 1 1]
--        nan
--        nan
--        nan
--        nan
--        nan
--        nan
--        nan
--        nan
--        nan
--        nan
getFeaturesScore
  :: Features
  -> Array a
getFeaturesScore = (`featuresToArray` af_get_features_score)

-- | Get Feature orientation
--
-- >>> getFeaturesOrientation (createFeatures 10)
-- ArrayFire Array
-- [10 1 1 1]
--        nan
--        nan
--        nan
--        nan
--        nan
--        nan
--        nan
--        nan
--        nan
--        nan
getFeaturesOrientation
  :: Features
  -> Array a
getFeaturesOrientation = (`featuresToArray` af_get_features_orientation)

-- | Get Feature size
--
-- >>> getFeaturesSize (createFeatures 10)
-- ArrayFire Array
-- [10 1 1 1]
--        nan
--        nan
--        nan
--        nan
--        nan
--        nan
--        nan
--        nan
--        nan
--        nan
getFeaturesSize
  :: Features
  -> Array a
getFeaturesSize = (`featuresToArray` af_get_features_size)
