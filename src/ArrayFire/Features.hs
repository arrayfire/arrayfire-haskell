{-# LANGUAGE ViewPatterns #-}
module ArrayFire.Features where

import Foreign.Marshal
import Foreign.Storable
import Foreign.ForeignPtr
import System.IO.Unsafe

import ArrayFire.Internal.Features
import ArrayFire.Types
import ArrayFire.FFI
import ArrayFire.Exception

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

retainFeatures
  :: Features
  -> Features
retainFeatures = (`op1f` af_retain_features)

getFeaturesNum
  :: Features
  -> Int
getFeaturesNum = fromIntegral . (`infoFromFeatures` af_get_features_num)

getFeaturesXPos
  :: Features
  -> Array a
getFeaturesXPos = (`featuresToArray` af_get_features_xpos)

getFeaturesYPos
  :: Features
  -> Array a
getFeaturesYPos = (`featuresToArray` af_get_features_ypos)

getFeaturesScore
  :: Features
  -> Array a
getFeaturesScore = (`featuresToArray` af_get_features_score)

getFeaturesOrientation
  :: Features
  -> Array a
getFeaturesOrientation = (`featuresToArray` af_get_features_orientation)

getFeaturesSize
  :: Features
  -> Array a
getFeaturesSize = (`featuresToArray` af_get_features_size)
