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
-- @
-- module Main where
--
-- import ArrayFire
--
-- main :: IO ()
-- main = print =<< getAvailableBackends
-- @
--
-- @
-- [nix-shell:~\/arrayfire]$ .\/main
-- [CPU,OpenCL]
-- @
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

-- | Construct 'Features'
--
-- @
-- >>> print $ mean 0 ( vector @Int 10 [1..] )
-- @
-- @
-- ArrayFire Array
--   [1 1 1 1]
--      5.5000
-- @
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
--
-- @
-- >>> print $ mean 0 ( vector @Int 10 [1..] )
-- @
-- @
-- ArrayFire Array
--   [1 1 1 1]
--      5.5000
-- @
retainFeatures
  :: Features
  -> Features
retainFeatures = (`op1f` af_retain_features)

-- | Get number of 'Feature's
--
-- @
-- >>> print $ mean 0 ( vector @Int 10 [1..] )
-- @
-- @
-- ArrayFire Array
--   [1 1 1 1]
--      5.5000
-- @
getFeaturesNum
  :: Features
  -> Int
getFeaturesNum = fromIntegral . (`infoFromFeatures` af_get_features_num)

-- | Get 'Feature' X-position
--
-- @
-- >>> print $ mean 0 ( vector @Int 10 [1..] )
-- @
-- @
-- ArrayFire Array
--   [1 1 1 1]
--      5.5000
-- @
getFeaturesXPos
  :: Features
  -> Array a
getFeaturesXPos = (`featuresToArray` af_get_features_xpos)

-- | Get 'Feature' Y-position
--
-- @
-- >>> print $ mean 0 ( vector @Int 10 [1..] )
-- @
-- @
-- ArrayFire Array
--   [1 1 1 1]
--      5.5000
-- @
getFeaturesYPos
  :: Features
  -> Array a
getFeaturesYPos = (`featuresToArray` af_get_features_ypos)

-- | Get 'Feature' Score
--
-- @
-- >>> print $ mean 0 ( vector @Int 10 [1..] )
-- @
-- @
-- ArrayFire Array
--   [1 1 1 1]
--      5.5000
-- @
getFeaturesScore
  :: Features
  -> Array a
getFeaturesScore = (`featuresToArray` af_get_features_score)

-- | Get 'Feature' orientation
--
-- @
-- >>> print $ mean 0 ( vector @Int 10 [1..] )
-- @
-- @
-- ArrayFire Array
--   [1 1 1 1]
--      5.5000
-- @
getFeaturesOrientation
  :: Features
  -> Array a
getFeaturesOrientation = (`featuresToArray` af_get_features_orientation)

-- | Get 'Feature' size
--
-- @
-- >>> print $ mean 0 ( vector @Int 10 [1..] )
-- @
-- @
-- ArrayFire Array
--   [1 1 1 1]
--      5.5000
-- @
getFeaturesSize
  :: Features
  -> Array a
getFeaturesSize = (`featuresToArray` af_get_features_size)
