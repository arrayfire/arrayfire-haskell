{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
--------------------------------------------------------------------------------
-- |
-- Module      : ArrayFire.Image
-- Copyright   : David Johnson (c) 2019-2020
-- License     : BSD 3
-- Maintainer  : David Johnson <djohnson.m@gmail.com>
-- Stability   : Experimental
-- Portability : GHC
--
-- Image API
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
module ArrayFire.Image where

import Data.Proxy

import ArrayFire.Internal.Types
import ArrayFire.FFI
import ArrayFire.Internal.Image

-- | Calculates the gradient of an image
--
-- @
-- >>> print (gradient image)
-- @
gradient :: Array a -> (Array a, Array a)
gradient a = a `op2p` af_gradient

-- | Loads an image from disk
--
-- @
-- >>> image <- loadImage "image.png" True
-- @
loadImage 
  :: String 
  -- ^ File path
  -> Bool
  -- ^ Is color image (boolean denoting if the image should be loaded as 1 channel or 3 channel)
   -> IO (Array a)
loadImage s b = loadAFImage s b af_load_image

-- | Saves an image to disk
--
-- @
-- >>> saveImage image "image.png" 
-- @
saveImage :: Array a -> String -> IO ()
saveImage a s = afSaveImage a s af_save_image

-- af_err af_load_image_memory(af_array *out, const void* ptr);
-- af_err af_save_image_memory(void** ptr, const af_array in, const af_image_format format);
-- af_err af_delete_image_memory(void* ptr);

-- | Loads an image natively
--
-- @
-- >>> image <- loadImageNative "image.png" 
-- @
loadImageNative :: String -> IO (Array a)
loadImageNative s = loadAFImageNative s af_load_image_native

-- | Saves an image natively
--
-- @
-- >>> saveImageNative image "image.png" 
-- @
saveImageNative :: Array a -> String -> IO ()
saveImageNative a s = afSaveImage a s af_save_image_native

-- | Returns true if ArrayFire was compiled with ImageIO (FreeImage) support
--
-- @
-- >>> print =<< isImageIOAvailable
-- @
isImageIOAvailable :: IO Bool
isImageIOAvailable = 
  toEnum . fromIntegral <$> afCall1 af_is_image_io_available

-- | Calculates 'resize' of 'Array' along user-specified dimension.
--
-- @
-- >>> print $ mean 0 ( vector @Int 10 [1..] )
-- @
-- @
-- ArrayFire Array
--   [1 1 1 1]
--      5.5000
-- @
resize
  :: Array a
  -> Int
  -> Int
  -> InterpType
  -> Array a
resize a (fromIntegral -> d0) (fromIntegral -> d1) (fromInterpType -> interp) =
  a `op1` (\ptr x -> af_resize ptr x d0 d1 interp)

-- | Calculates 'mean' of 'Array' along user-specified dimension.
--
-- @
-- >>> print $ mean 0 ( vector @Int 10 [1..] )
-- @
-- @
-- ArrayFire Array
--   [1 1 1 1]
--      5.5000
-- @
transform
  :: Array a
  -> Array a
  -> Int
  -> Int
  -> InterpType
  -> Bool
  -> Array a
transform inp trans (fromIntegral -> d0) (fromIntegral -> d1) (fromInterpType -> interp) (fromIntegral . fromEnum -> inverse) =
  op2 inp trans (\ptr a1 a2 -> af_transform ptr a1 a2 d0 d1 interp inverse)

-- | Calculates 'mean' of 'Array' along user-specified dimension.
--
-- @
-- >>> print $ mean 0 ( vector @Int 10 [1..] )
-- @
-- @
-- ArrayFire Array
--   [1 1 1 1]
--      5.5000
-- @
transformCoordinates
  :: Array a
  -> Float
  -> Float
  -> Array a
transformCoordinates a d0 d1 =
  a `op1` (\ptr x -> af_transform_coordinates ptr x d0 d1)

-- | Calculates 'mean' of 'Array' along user-specified dimension.
--
-- @
-- >>> print $ mean 0 ( vector @Int 10 [1..] )
-- @
-- @
-- ArrayFire Array
--   [1 1 1 1]
--      5.5000
-- @
rotate
  :: Array a
  -> Float
  -> Bool
  -> InterpType
  -> Array a
rotate a theta (fromIntegral . fromEnum -> crop) (fromInterpType -> interp) =
  a `op1` (\ptr x -> af_rotate ptr x theta crop interp)

-- | Calculates 'mean' of 'Array' along user-specified dimension.
--
-- @
-- >>> print $ mean 0 ( vector @Int 10 [1..] )
-- @
-- @
-- ArrayFire Array
--   [1 1 1 1]
--      5.5000
-- @
translate
  :: Array a
  -> Float
  -> Float
  -> Int
  -> Int
  -> InterpType
  -> Array a
translate a trans0 trans1 (fromIntegral -> odim0) (fromIntegral -> odim1) (fromInterpType -> interp) =
  a `op1` (\ptr x -> af_translate ptr x trans0 trans1 odim0 odim1 interp)

-- | Calculates 'mean' of 'Array' along user-specified dimension.
--
-- @
-- >>> print $ mean 0 ( vector @Int 10 [1..] )
-- @
-- @
-- ArrayFire Array
--   [1 1 1 1]
--      5.5000
-- @
scale
  :: Array a
  -> Float
  -> Float
  -> Int
  -> Int
  -> InterpType
  -> Array a
scale a trans0 trans1 (fromIntegral -> odim0) (fromIntegral -> odim1) (fromInterpType -> interp) =
  a `op1` (\ptr x -> af_scale ptr x trans0 trans1 odim0 odim1 interp)

-- | Calculates 'mean' of 'Array' along user-specified dimension.
--
-- @
-- >>> print $ mean 0 ( vector @Int 10 [1..] )
-- @
-- @
-- ArrayFire Array
--   [1 1 1 1]
--      5.5000
-- @
skew
  :: Array a
  -> Float
  -> Float
  -> Int
  -> Int
  -> InterpType
  -> Bool
  -> Array a
skew a trans0 trans1 (fromIntegral -> odim0) (fromIntegral -> odim1) (fromInterpType -> interp) (fromIntegral . fromEnum -> b) =
  a `op1` (\ptr x -> af_skew ptr x trans0 trans1 odim0 odim1 interp b)

-- | Calculates 'mean' of 'Array' along user-specified dimension.
--
-- @
-- >>> print $ mean 0 ( vector @Int 10 [1..] )
-- @
-- @
-- ArrayFire Array
--   [1 1 1 1]
--      5.5000
-- @
histogram
  :: Array a
  -> Int
  -> Double
  -> Double
  -> Array a
histogram a (fromIntegral -> b) c d =
  a `op1` (\ptr x -> af_histogram ptr x b c d)

-- | Calculates 'mean' of 'Array' along user-specified dimension.
--
-- @
-- >>> print $ mean 0 ( vector @Int 10 [1..] )
-- @
-- @
-- ArrayFire Array
--   [1 1 1 1]
--      5.5000
-- @
dilate
  :: Array a
  -> Array a
  -> Array a
dilate in' mask = op2 in' mask af_dilate

-- | Calculates 'mean' of 'Array' along user-specified dimension.
--
-- @
-- >>> print $ mean 0 ( vector @Int 10 [1..] )
-- @
-- @
-- ArrayFire Array
--   [1 1 1 1]
--      5.5000
-- @
dilate3
  :: Array a
  -> Array a
  -> Array a
dilate3 in' mask = op2 in' mask af_dilate3

-- | Calculates 'mean' of 'Array' along user-specified dimension.
--
-- @
-- >>> print $ mean 0 ( vector @Int 10 [1..] )
-- @
-- @
-- ArrayFire Array
--   [1 1 1 1]
--      5.5000
-- @
erode
  :: Array a
  -> Array a
  -> Array a
erode in' mask = op2 in' mask af_erode

-- | Calculates 'mean' of 'Array' along user-specified dimension.
--
-- @
-- >>> print $ mean 0 ( vector @Int 10 [1..] )
-- @
-- @
-- ArrayFire Array
--   [1 1 1 1]
--      5.5000
-- @
erode3
  :: Array a
  -> Array a
  -> Array a
erode3 in' mask = op2 in' mask af_erode3

-- | Calculates 'mean' of 'Array' along user-specified dimension.
--
-- @
-- >>> print $ mean 0 ( vector @Int 10 [1..] )
-- @
-- @
-- ArrayFire Array
--   [1 1 1 1]
--      5.5000
-- @
bilateral
  :: Array a
  -> Float
  -> Float
  -> Bool
  -> Array a
bilateral in' a b (fromIntegral . fromEnum -> c) = in' `op1` (\ptr k -> af_bilateral ptr k a b c)

-- | Calculates 'mean' of 'Array' along user-specified dimension.
--
-- @
-- >>> print $ mean 0 ( vector @Int 10 [1..] )
-- @
-- @
-- ArrayFire Array
--   [1 1 1 1]
--      5.5000
-- @
meanShift
  :: Array a
  -> Float
  -> Float
  -> Int
  -> Bool
  -> Array a
meanShift in' a b (fromIntegral -> c) (fromIntegral . fromEnum -> d) = in' `op1` (\ptr k -> af_mean_shift ptr k a b c d)

-- | Calculates 'mean' of 'Array' along user-specified dimension.
--
-- @
-- >>> print $ mean 0 ( vector @Int 10 [1..] )
-- @
-- @
-- ArrayFire Array
--   [1 1 1 1]
--      5.5000
-- @
minFilt
  :: Array a
  -> Int
  -> Int
  -> BorderType
  -> Array a
minFilt in' (fromIntegral -> a) (fromIntegral -> b) (fromBorderType -> c) =
  in' `op1` (\ptr k -> af_minfilt ptr k a b c)

-- | Calculates 'mean' of 'Array' along user-specified dimension.
--
-- @
-- >>> print $ mean 0 ( vector @Int 10 [1..] )
-- @
-- @
-- ArrayFire Array
--   [1 1 1 1]
--      5.5000
-- @
maxFilt
  :: Array a
  -> Int
  -> Int
  -> BorderType
  -> Array a
maxFilt in' (fromIntegral -> a) (fromIntegral -> b) (fromBorderType -> c) =
  in' `op1` (\ptr k -> af_maxfilt ptr k a b c)

-- | Calculates 'mean' of 'Array' along user-specified dimension.
--
-- @
-- >>> print $ mean 0 ( vector @Int 10 [1..] )
-- @
-- @
-- ArrayFire Array
--   [1 1 1 1]
--      5.5000
-- @
regions
  :: forall a . AFType a
  => Array a
  -> Connectivity
  -> Array a
regions in' (fromConnectivity -> conn) =
  in' `op1` (\ptr k -> af_regions ptr k conn dtype)
    where
      dtype = afType (Proxy @ a)

-- | Calculates 'mean' of 'Array' along user-specified dimension.
--
-- @
-- >>> print $ mean 0 ( vector @Int 10 [1..] )
-- @
-- @
-- ArrayFire Array
--   [1 1 1 1]
--      5.5000
-- @
sobel_operator
  :: Array a
  -> Int
  -> (Array a, Array a)
sobel_operator in' (fromIntegral -> kerSize) =
  in' `op2p` (\ptrA ptrB k -> af_sobel_operator ptrA ptrB k kerSize)

-- | Calculates 'mean' of 'Array' along user-specified dimension.
--
-- @
-- >>> print $ mean 0 ( vector @Int 10 [1..] )
-- @
-- @
-- ArrayFire Array
--   [1 1 1 1]
--      5.5000
-- @
rgb2gray
  :: Array a
  -> Float
  -> Float
  -> Float
  -> Array a
rgb2gray in' a b c =
  in' `op1` (\ptr k -> af_rgb2gray ptr k a b c)

-- | Calculates 'mean' of 'Array' along user-specified dimension.
--
-- @
-- >>> print $ mean 0 ( vector @Int 10 [1..] )
-- @
-- @
-- ArrayFire Array
--   [1 1 1 1]
--      5.5000
-- @
gray2rgb
  :: Array a
  -> Float
  -> Float
  -> Float
  -> Array a
gray2rgb in' a b c = in' `op1` (\ptr k -> af_gray2rgb ptr k a b c)

-- | Calculates 'mean' of 'Array' along user-specified dimension.
--
-- @
-- >>> print $ mean 0 ( vector @Int 10 [1..] )
-- @
-- @
-- ArrayFire Array
--   [1 1 1 1]
--      5.5000
-- @
histEqual
  :: Array a
  -> Array a
  -> Array a
histEqual in' mask = op2 in' mask af_hist_equal

-- | Calculates 'mean' of 'Array' along user-specified dimension.
--
-- @
-- >>> print $ mean 0 ( vector @Int 10 [1..] )
-- @
-- @
-- ArrayFire Array
--   [1 1 1 1]
--      5.5000
-- @
gaussianKernel
  :: Int
  -> Int
  -> Double
  -> Double
  -> Array a
gaussianKernel (fromIntegral -> i1) (fromIntegral -> i2) d1 d2 =
  createArray (\ptr -> af_gaussian_kernel ptr i1 i2 d1 d2)

-- | Calculates 'mean' of 'Array' along user-specified dimension.
--
-- @
-- >>> print $ mean 0 ( vector @Int 10 [1..] )
-- @
-- @
-- ArrayFire Array
--   [1 1 1 1]
--      5.5000
-- @
hsv2rgb
  :: Array a
  -> Array a
hsv2rgb = (`op1` af_hsv2rgb)

-- | Calculates 'mean' of 'Array' along user-specified dimension.
--
-- @
-- >>> print $ mean 0 ( vector @Int 10 [1..] )
-- @
-- @
-- ArrayFire Array
--   [1 1 1 1]
--      5.5000
-- @
rgb2hsv
  :: Array a
  -> Array a
rgb2hsv = (`op1` af_rgb2hsv)

-- | Calculates 'mean' of 'Array' along user-specified dimension.
--
-- @
-- >>> print $ mean 0 ( vector @Int 10 [1..] )
-- @
-- @
-- ArrayFire Array
--   [1 1 1 1]
--      5.5000
-- @
colorSpace
  :: Array a
  -> CSpace
  -> CSpace
  -> Array a
colorSpace in' (fromCSpace -> to) (fromCSpace -> from) =
  in' `op1` (\p a -> af_color_space p a to from)

-- | Calculates 'mean' of 'Array' along user-specified dimension.
--
-- @
-- >>> print $ mean 0 ( vector @Int 10 [1..] )
-- @
-- @
-- ArrayFire Array
--   [1 1 1 1]
--      5.5000
-- @
unwrap
  :: Array a
  -> Int
  -> Int
  -> Int
  -> Int
  -> Int
  -> Int
  -> Bool
  -> Array a
unwrap in' (fromIntegral -> wx) (fromIntegral -> wy) (fromIntegral -> sx) (fromIntegral ->  sy) (fromIntegral -> px) (fromIntegral -> py) (fromIntegral . fromEnum -> b)
  = in' `op1` (\ptr a -> af_unwrap ptr a wx wy sx sy px py b)

-- | Calculates 'mean' of 'Array' along user-specified dimension.
--
-- @
-- >>> print $ mean 0 ( vector @Int 10 [1..] )
-- @
-- @
-- ArrayFire Array
--   [1 1 1 1]
--      5.5000
-- @
wrap
  :: Array a
  -> Int
  -> Int
  -> Int
  -> Int
  -> Int
  -> Int
  -> Int
  -> Int
  -> Bool
  -> Array a
wrap in' (fromIntegral -> ox) (fromIntegral -> oy) (fromIntegral -> wx) (fromIntegral -> wy)
         (fromIntegral -> sx) (fromIntegral -> sy) (fromIntegral -> px) (fromIntegral -> py) (fromIntegral . fromEnum -> b)
  = in' `op1` (\ptr a -> af_wrap ptr a ox oy wx wy sx sy px py b)

-- | Calculates 'mean' of 'Array' along user-specified dimension.
--
-- @
-- >>> print $ mean 0 ( vector @Int 10 [1..] )
-- @
-- @
-- ArrayFire Array
--   [1 1 1 1]
--      5.5000
-- @
sat
  :: Array a
  -> Array a
sat = (`op1` af_sat)

-- | Calculates 'mean' of 'Array' along user-specified dimension.
--
-- @
-- >>> print $ mean 0 ( vector @Int 10 [1..] )
-- @
-- @
-- ArrayFire Array
--   [1 1 1 1]
--      5.5000
-- @
ycbcr2rgb
  :: Array a
  -> YccStd
  -> Array a
ycbcr2rgb a y = a `op1` (\p k -> af_ycbcr2rgb p k (fromAFYccStd y))

-- | Calculates 'mean' of 'Array' along user-specified dimension.
--
-- @
-- >>> print $ mean 0 ( vector @Int 10 [1..] )
-- @
-- @
-- ArrayFire Array
--   [1 1 1 1]
--      5.5000
-- @
rgb2ycbcr
  :: Array a
  -> YccStd
  -> Array a
rgb2ycbcr a y = a `op1` (\p k -> af_rgb2ycbcr p k (fromAFYccStd y))

-- | Calculates 'mean' of 'Array' along user-specified dimension.
--
-- @
-- >>> print $ mean 0 ( vector @Int 10 [1..] )
-- @
-- @
-- ArrayFire Array
--   [1 1 1 1]
--      5.5000
-- @
moments
  :: Array a
  -> MomentType
  -> Array a
moments in' m =
  in' `op1` (\p k -> af_moments p k (fromMomentType m))

-- | Calculates 'mean' of 'Array' along user-specified dimension.
--
-- @
-- >>> print $ mean 0 ( vector @Int 10 [1..] )
-- @
-- @
-- ArrayFire Array
--   [1 1 1 1]
--      5.5000
-- @
momentsAll
  :: Array a
  -> MomentType
  -> Double
momentsAll in' m =
  in' `infoFromArray` (\p a -> af_moments_all p a (fromMomentType m))

-- | Calculates 'mean' of 'Array' along user-specified dimension.
--
-- @
-- >>> print $ mean 0 ( vector @Int 10 [1..] )
-- @
-- @
-- ArrayFire Array
--   [1 1 1 1]
--      5.5000
-- @
canny
  :: Array a
  -> CannyThreshold
  -> Float
  -> Float
  -> Int
  -> Bool
  -> Array a
canny in' (fromCannyThreshold -> canny') low high (fromIntegral -> window) (fromIntegral . fromEnum -> fast) =
  in' `op1` (\p a -> af_canny p a canny' low high window fast)

-- | Calculates 'mean' of 'Array' along user-specified dimension.
--
-- @
-- >>> print $ mean 0 ( vector @Int 10 [1..] )
-- @
-- @
-- ArrayFire Array
--   [1 1 1 1]
--      5.5000
-- @
anisotropicDiffusion
  :: Array a
  -> Float
  -> Float
  -> Int
  -> FluxFunction
  -> DiffusionEq
  -> Array a
anisotropicDiffusion in' ts con (fromIntegral -> iter) (fromFluxFunction -> flux) (fromDiffusionEq -> diff) =
  in' `op1` (\p a -> af_anisotropic_diffusion p a ts con iter flux diff)

-- | Calculates 'mean' of 'Array' along user-specified dimension.
--
-- @
-- >>> print $ mean 0 ( vector @Int 10 [1..] )
-- @
-- @
-- ArrayFire Array
--   [1 1 1 1]
--      5.5000
-- @
iterativeDeconv
  :: Array a
  -> Array a
  -> Int
  -> Float
  -> IterativeDeconvAlgo
  -> Array a
iterativeDeconv in1 in2 (fromIntegral -> i) f1 (fromIterativeDeconvAlgo -> algo) =
  op2 in1 in2 (\p a k -> af_iterative_deconv p a k i f1 algo)

-- | Calculates 'mean' of 'Array' along user-specified dimension.
--
-- @
-- >>> print $ mean 0 ( vector @Int 10 [1..] )
-- @
-- @
-- ArrayFire Array
--   [1 1 1 1]
--      5.5000
-- @
inverseDeconv
  :: Array a
  -> Array a
  -> Float
  -> InverseDeconvAlgo
  -> Array a
inverseDeconv in1 in2 f1 (fromInverseDeconvAlgo -> algo) =
  op2 in1 in2 (\p a k -> af_inverse_deconv p a k f1 algo)
