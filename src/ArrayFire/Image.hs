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
-- Functions for loading and manipulating images with 'Array'
--
-- @
-- >>> image <- 'loadImage' "image.png" True
-- @
--
--------------------------------------------------------------------------------
module ArrayFire.Image where

import Data.Proxy
import Data.Word

import ArrayFire.Internal.Types
import ArrayFire.Internal.Image
import ArrayFire.FFI
import ArrayFire.Arith

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

-- | Resize an input image.
--
-- [ArrayFire Docs](http://arrayfire.org/docs/group__transform__func__resize.htm)
--
-- Resizing an input image can be done using either AF_INTERP_NEAREST, AF_INTERP_BILINEAR or AF_INTERP_LOWER, interpolations. Nearest interpolation will pick the nearest value to the location, bilinear interpolation will do a weighted interpolation for calculate the new size and lower interpolation is similar to the nearest, except it will use the floor function to get the lower neighbor.
--
resize
  :: Array a
  -- ^ input image
  -> Int
  -- ^ is the size for the first output dimension
  -> Int
  -- ^ is the size for the second output dimension
  -> InterpType
  -- ^ is the interpolation type (Nearest by default)
  -> Array a
  -- ^ will contain the resized image of specified by odim0 and odim1
resize a (fromIntegral -> d0) (fromIntegral -> d1) (fromInterpType -> interp) =
  a `op1` (\ptr x -> af_resize ptr x d0 d1 interp)

-- | Transform an input image.
--
-- [ArrayFire Docs](http://arrayfire.org/docs/group__transform__func__transform.htm)
--
-- The transform function uses an affine or perspective transform matrix to tranform an input image into a new one.
--
transform
  :: Array a
  -- ^ is input image
  -> Array a
  -- ^ is transformation matrix
  -> Int
  -- ^ is the first output dimension
  -> Int
  -- ^ is the second output dimension
  -> InterpType
  -- ^ is the interpolation type (Nearest by default)
  -> Bool
  -- ^ if true applies inverse transform, if false applies forward transoform
  -> Array a
  -- ^ will contain the transformed image
transform inp trans (fromIntegral -> d0) (fromIntegral -> d1) (fromInterpType -> interp) (fromIntegral . fromEnum -> inverse) =
  op2 inp trans (\ptr a1 a2 -> af_transform ptr a1 a2 d0 d1 interp inverse)

-- | Transform input coordinates.
--
-- [ArrayFire Docs](http://arrayfire.org/docs/group__transform__func__coordinates.htm)
--
-- C Interface for transforming an image C++ Interface for transforming coordinates.
--
transformCoordinates
  :: Array a
  -- ^ is transformation matrix
  -> Float
  -- ^ is the first input dimension
  -> Float
  -- ^ is the second input dimension
  -> Array a
  -- ^ the transformed coordinates
transformCoordinates a d0 d1 =
  a `op1` (\ptr x -> af_transform_coordinates ptr x d0 d1)

-- | Rotate an input image.
--
-- [ArrayFire Docs](http://arrayfire.org/docs/group__transform__func__rotate.htm)
--
-- Rotating an input image can be done using AF_INTERP_NEAREST, AF_INTERP_BILINEAR or AF_INTERP_LOWER interpolations. Nearest interpolation will pick the nearest value to the location, whereas bilinear interpolation will do a weighted interpolation for calculate the new size.
--
rotate
  :: Array a
  -- ^ is input image
  -> Float
  -- ^ is the degree (in radians) by which the input is rotated
  -> Bool
  -- ^ if true the output is cropped original dimensions. If false the output dimensions scale based on theta
  -> InterpType
  -- ^ is the interpolation type (Nearest by default)
  -> Array a
  -- ^ will contain the image in rotated by theta
rotate a theta (fromIntegral . fromEnum -> crop) (fromInterpType -> interp) =
  a `op1` (\ptr x -> af_rotate ptr x theta crop interp)

-- | Translate an input image.
--
-- [ArrayFire Docs](http://arrayfire.org/docs/group__transform__func__translate.htm)
--
-- Translating an image is moving it along 1st and 2nd dimensions by trans0 and trans1. Positive values of these will move the data towards negative x and negative y whereas negative values of these will move the positive right and positive down. See the example below for more.
--
translate
  :: Array a
  -- ^ is input image
  -> Float
  -- ^ is amount by which the first dimension is translated
  -> Float
  -- ^ is amount by which the second dimension is translated
  -> Int
  -- ^ is the first output dimension
  -> Int
  -- ^ is the second output dimension
  -> InterpType
  -- ^ is the interpolation type (Nearest by default)
  -> Array a
  -- ^ will contain the translated image
translate a trans0 trans1 (fromIntegral -> odim0) (fromIntegral -> odim1) (fromInterpType -> interp) =
  a `op1` (\ptr x -> af_translate ptr x trans0 trans1 odim0 odim1 interp)

-- | Scale an input image.
--
-- [ArrayFire Docs](http://arrayfire.org/docs/group__transform__func__scale.htm)
--
-- Scale is the same functionality as af::resize except that the scale function uses the transform kernels. The other difference is that scale does not set boundary values to be the boundary of the input array. Instead these are set to 0.
--
scale
  :: Array a
  -- ^ is input image
  -> Float
  -- ^ is amount by which the first dimension is scaled
  -> Float
  -- ^ is amount by which the second dimension is scaled
  -> Int
  -- ^ is the first output dimension
  -> Int
  -- ^ is the second output dimension
  -> InterpType
  -- ^ is the interpolation type (Nearest by default)
  -> Array a
  -- ^ will contain the scaled image
scale a trans0 trans1 (fromIntegral -> odim0) (fromIntegral -> odim1) (fromInterpType -> interp) =
  a `op1` (\ptr x -> af_scale ptr x trans0 trans1 odim0 odim1 interp)

-- | Skew an input image.
--
-- [ArrayFire Docs](http://arrayfire.org/docs/group__transform__func__skew.htm)
--
-- Skew function skews the input array along dim0 by skew0 and along dim1 by skew1. The skew areguments are in radians. Skewing the data means the data remains parallel along 1 dimensions but the other dimensions gets moved along based on the angle. If both skew0 and skew1 are specified, then the data will be skewed along both directions.
--
skew
  :: Array a
  -- ^ is input image
  -> Float
  -- ^ is amount by which the first dimension is skewed
  -> Float
  -- ^ is amount by which the second dimension is skewed
  -> Int
  -- ^ is the first output dimension
  -> Int
  -- ^ is the second output dimension
  -> InterpType
  -- ^ if true applies inverse transform, if false applies forward transoform
  -> Bool
  -- ^ is the interpolation type (Nearest by default)
  -> Array a
  -- ^ will contain the skewed image
skew a trans0 trans1 (fromIntegral -> odim0) (fromIntegral -> odim1) (fromInterpType -> interp) (fromIntegral . fromEnum -> b) =
  a `op1` (\ptr x -> af_skew ptr x trans0 trans1 odim0 odim1 interp b)

-- | Histogram of input data.
--
-- [ArrayFire Docs](http://arrayfire.org/docs/group__image__func__histogram.htm)
--
-- A histogram is a representation of the distribution of given data. This representation is essentially a graph consisting of the data range or domain on one axis and frequency of occurence on the other axis. All the data in the domain is counted in the appropriate bin. The total number of elements belonging to each bin is known as the bin's frequency.
--
histogram
  :: AFType a
  => Array a
  -- ^ the input array
  -> Int
  -- ^ Number of bins to populate between min and max
  -> Double
  -- ^ minimum bin value (accumulates -inf to min)
  -> Double
  -- ^ minimum bin value (accumulates max to +inf)
  -> Array Word32
  -- ^ (type u32) is the histogram for input array in
histogram a (fromIntegral -> b) c d =
  cast (a `op1` (\ptr x -> af_histogram ptr x b c d))

-- | Dilation(morphological operator) for images.
--
-- [ArrayFire Docs](http://arrayfire.org/docs/group__image__func__dilate.htm)
--
-- The dilation function takes two pieces of data as inputs. The first is the input image to be morphed, and the second is the mask indicating the neighborhood around each pixel to match.
--
-- *Note* if mask is all ones, this function behaves like max filter
--
dilate
  :: Array a
  -- ^ the input image
  -> Array a
  -- ^ the neighborhood window
  -> Array a
  -- ^ the dilated image
dilate in' mask = op2 in' mask af_dilate

-- | Dilation (morphological operator) for volumes.
--
-- [ArrayFire Docs](http://arrayfire.org/docs/group__image__func__dilate3d.htm)
--
-- Dilation for a volume is similar to the way dilation works on an image. Only difference is that the masking operation is performed on a volume instead of a rectangular region.
--
dilate3
  :: Array a
  -- ^ the input volume
  -> Array a
  -- ^ the neighborhood delta volume
  -> Array a
  -- ^ the dilated volume
dilate3 in' mask = op2 in' mask af_dilate3

-- | Erosion (morphological operator) for volumes.
--
-- [ArrayFire Docs](http://arrayfire.org/docs/group__image__func__erode.htm)
--
-- The erosion function is a morphological transformation on an image that requires two inputs. The first is the image to be morphed, and the second is the mask indicating neighborhood that must be white in order to preserve each pixel.
--
-- *Note* if mask is all ones, this function behaves like min filter
--
erode
  :: Array a
  -- ^ 'Array' is the input image
  -> Array a
  -- ^ (mask) is the neighborhood window
  -> Array a
  -- ^ 'Array' is the eroded image
erode in' mask = op2 in' mask af_erode

-- | Erosion (morphological operator) for volumes.
--
-- [ArrayFire Docs](http://arrayfire.org/docs/group__image__func__erode3d.htm)
--
-- Erosion for a volume is similar to the way erosion works on an image. Only difference is that the masking operation is performed on a volume instead of a rectangular region.
--
erode3
  :: Array a
  -- ^ 'Array' is the input volume
  -> Array a
  -- ^ (mask) is the neighborhood delta volume
  -> Array a
  -- ^ 'Array' is the eroded volume
erode3 in' mask = op2 in' mask af_erode3

-- | Bilateral Filter.
--
-- [ArrayFire Docs](http://arrayfire.org/docs/group__image__func__bilateral.htm)
--
-- A bilateral filter is a edge-preserving filter that reduces noise in an image. The intensity of each pixel is replaced by a weighted average of the intensities of nearby pixels. The weights follow a Gaussian distribution and depend on the distance as well as the color distance.
--
bilateral
  :: Array a
  -- ^ 'Array' is the input image
  -> Float
  -- ^ is the spatial variance parameter that decides the filter window
  -> Float
  -- ^ is the chromatic variance parameter
  -> Bool
  -- ^ indicates if the input in is color image or grayscale
  -> Array a
  -- ^ 'Array' is the processed image
bilateral in' a b (fromIntegral . fromEnum -> c) = in' `op1` (\ptr k -> af_bilateral ptr k a b c)

-- | Meanshift Filter.
--
-- [ArrayFire Docs](http://arrayfire.org/docs/group__image__func__mean__shift.htm)
--
-- A meanshift filter is an edge-preserving smoothing filter commonly used in object tracking and image segmentation.
--
meanShift
  :: Array a
  -- ^ 'Array' is the input image
  -> Float
  -- ^ is the spatial variance parameter that decides the filter window
  -> Float
  -- ^ is the chromatic variance parameter
  -> Int
  -- ^ is the number of iterations filter operation is performed
  -> Bool
  -- ^ indicates if the input in is color image or grayscale
  -> Array a
  -- ^ 'Array' is the processed image
meanShift in' a b (fromIntegral -> c) (fromIntegral . fromEnum -> d) = in' `op1` (\ptr k -> af_mean_shift ptr k a b c d)

-- | Find minimum value from a window.
--
-- [ArrayFire Docs](http://arrayfire.org/docs/group__image__func__minfilt.htm)
--
-- minfilt finds the smallest value from a 2D window and assigns it to the current pixel.
--
minFilt
  :: Array a
  -- ^ 'Array' is the input image
  -> Int
  -- ^ is the kernel height
  -> Int
  -- ^ is the kernel width
  -> BorderType
  -- ^ value will decide what happens to border when running filter in their neighborhood. It takes one of the values [AF_PAD_ZERO | AF_PAD_SYM]
  -> Array a
  -- ^ 'Array' is the processed image
minFilt in' (fromIntegral -> a) (fromIntegral -> b) (fromBorderType -> c) =
  in' `op1` (\ptr k -> af_minfilt ptr k a b c)

-- | Find maximum value from a window.
--
-- [ArrayFire Docs](http://arrayfire.org/docs/group__image__func__maxfilt.htm)
--
-- 'maxFilt' finds the smallest value from a 2D window and assigns it to the current pixel.
--
maxFilt
  :: Array a
  -- ^ 'Array' is the input image
  -> Int
  -- ^ is the kernel height
  -> Int
  -- ^ is the kernel width
  -> BorderType
  -- ^ value will decide what happens to border when running filter in their neighborhood. It takes one of the values [AF_PAD_ZERO | AF_PAD_SYM]
  -> Array a
  -- ^ 'Array' is the processed image
maxFilt in' (fromIntegral -> a) (fromIntegral -> b) (fromBorderType -> c) =
  in' `op1` (\ptr k -> af_maxfilt ptr k a b c)

-- | Find blobs in given image.
--
-- [ArrayFire Docs](http://arrayfire.org/docs/group__image__func__regions.htm)
--
-- Given a binary image (with zero representing background pixels), regions computes a floating point image where each connected component is labeled from 1 to N, the total number of components in the image.
-- ** FIX ME**
regions
  :: forall a . (AFType a)
  => Array a
  -- ^ array should be binary image of type CBool
  -> Connectivity
  -- ^
  -> Array a
  -- ^ array will have labels indicating different regions
regions in' (fromConnectivity -> conn) =
  in' `op1` (\ptr k -> af_regions ptr k conn dtype)
    where
      dtype = afType (Proxy @ a)

-- | Sobel Operators.
--
-- [ArrayFire Docs](http://arrayfire.org/docs/group__image__func__sobel.htm)
--
-- Sobel operators perform a 2-D spatial gradient measurement on an image to emphasize the regions of high spatial frequency, namely edges
--
-- *Note* If img is 3d array, a batch operation will be performed.
--
sobel_operator
  :: Array a
  -- ^ is an array with image data
  -> Int
  -- ^ sobel kernel size or window size
  -> (Array a, Array a)
  -- ^ Derivative along the horizontal and vertical directions
sobel_operator in' (fromIntegral -> kerSize) =
  in' `op2p` (\ptrA ptrB k -> af_sobel_operator ptrA ptrB k kerSize)

-- | RGB to Grayscale colorspace converter.
--
-- [ArrayFire Docs](http://arrayfire.org/docs/group__image__func__rgb2gray.htm)
--
-- RGB (Red, Green, Blue) is the most common format used in computer imaging. RGB stores individual values for red, green and blue, and hence the 3 values per pixel. A combination of these three values produces the gamut of unique colors.
--
rgb2gray
  :: Array a
  -- ^ is an array in the RGB color space
  -> Float
  -- ^ is percentage of red channel value contributing to grayscale intensity
  -> Float
  -- ^ is percentage of green channel value contributing to grayscale intensity
  -> Float
  -- ^ is percentage of blue channel value contributing to grayscale intensity
  -> Array a
  -- ^ is an array in target color space
rgb2gray in' a b c =
  in' `op1` (\ptr k -> af_rgb2gray ptr k a b c)

-- | Grayscale to RGB colorspace converter.
--
-- [ArrayFire Docs](http://arrayfire.org/docs/group__image__func__gray2rgb.htm)
--
-- Grayscale is a single channel color space where pixel value ranges from 0 to 1. Zero represents black, one represent white and any value between zero & one is a gray value
--
gray2rgb
  :: Array a
  -- ^ is an array in the Grayscale color space
  -> Float
  -- ^ is percentage of intensity value contributing to red channel
  -> Float
  -- ^ is percentage of intensity value contributing to green channel
  -> Float
  -- ^ is percentage of intensity value contributing to blue channel
  -> Array a
  -- ^ is an array in target color space
gray2rgb in' a b c = in' `op1` (\ptr k -> af_gray2rgb ptr k a b c)

-- | Histogram equalization of input image.
--
-- [ArrayFire Docs](http://arrayfire.org/docs/group__image__func__histequal.htm)
--
-- Histogram equalization is a method in image processing of contrast adjustment using the image's histogram.
--
histEqual
  :: Array a
  -- ^ is the input array, non-normalized input (!! assumes values [0-255] !!)
  -> Array a
  -- ^ target histogram to approximate in output (based on number of bins)
  -> Array a
  -- ^ is an array with data that has histogram approximately equal to histogram
histEqual in' mask = op2 in' mask af_hist_equal

-- | Creates a Gaussian Kernel.
--
-- [ArrayFire Docs](http://arrayfire.org/docs/group__image__func__gauss.htm)
--
-- This function creates a kernel of a specified size that contains a Gaussian distribution. This distribution is normalized to one. This is most commonly used when performing a Gaussian blur on an image. The function takes two sets of arguments, the size of the kernel (width and height in pixels) and the sigma parameters (for row and column) which effect the distribution of the weights in the y and x directions, respectively.
--
gaussianKernel
  :: Int
  -- ^ number of rows of the gaussian kernel
  -> Int
  -- ^ number of columns of the gaussian kernel
  -> Double
  -- ^ (default 0) (calculated internally as 0.25 * rows + 0.75)
  -> Double
  -- ^ (default 0) (calculated internally as 0.25 * cols + 0.75)
  -> Array a
  -- ^ is an array with values generated using gaussian function
gaussianKernel (fromIntegral -> i1) (fromIntegral -> i2) d1 d2 =
  createArray (\ptr -> af_gaussian_kernel ptr i1 i2 d1 d2)

-- | HSV to RGB colorspace converter.
--
-- [ArrayFire Docs](http://arrayfire.org/docs/group__image__func__hsv2rgb.htm)
--
-- C Interface for converting HSV to RGB.
--
-- *Note* input must be three dimensional
--
hsv2rgb
  :: Array a
  -- ^ is an array in the HSV color space
  -> Array a
  -- ^ is an array in the RGB color space
hsv2rgb = (`op1` af_hsv2rgb)

-- | RGB to HSV colorspace converter.
--
-- [ArrayFire Docs](http://arrayfire.org/docs/group__image__func__rgb2hsv.htm)
--
-- RGB (Red, Green, Blue) is the most common format used in computer imaging. RGB stores individual values for red, green and blue, and hence the 3 values per pixel. A combination of these three values produces the gamut of unique colors.
--
rgb2hsv
  :: Array a
  -- ^ is an array in the RGB color space
  -> Array a
  -- ^ is an array in the HSV color space
rgb2hsv = (`op1` af_rgb2hsv)

-- | Colorspace conversion function.
--
-- [ArrayFire Docs](http://arrayfire.org/docs/group__image__func__colorspace.htm)
--
-- C Interface wrapper for color space conversion.
--
colorSpace
  :: Array a
  -- ^ is the input array
  -> CSpace
  -- ^ is the target array color space
  -> CSpace
  -- ^ is the input array color space
  -> Array a
  -- ^ is an array in target color space
colorSpace in' (fromCSpace -> to) (fromCSpace -> from) =
  in' `op1` (\p a -> af_color_space p a to from)

-- | Rearrange windowed sections of an array into columns (or rows).
--
-- [ArrayFire Docs](http://arrayfire.org/docs/group__image__func__unwrap.htm)
--
-- C Interface for rearranging windowed sections of an input into columns (or rows)
--
unwrap
  :: Array a
  -- ^ the input 'Array'
  -> Int
  -- ^ is the window size along dimension 0
  -> Int
  -- ^ is the window size along dimension 1
  -> Int
  -- ^ is the stride along dimension 0
  -> Int
  -- ^ is the stride along dimension 1
  -> Int
  -- ^ is the padding along dimension 0
  -> Int
  -- ^ is the padding along dimension 1
  -> Bool
  -- ^ determines whether an output patch is formed from a column (if true) or a row (if false)
  -> Array a
  -- ^ an array with the input's sections rearraged as columns (or rows)
unwrap in' (fromIntegral -> wx) (fromIntegral -> wy) (fromIntegral -> sx) (fromIntegral ->  sy) (fromIntegral -> px) (fromIntegral -> py) (fromIntegral . fromEnum -> b)
  = in' `op1` (\ptr a -> af_unwrap ptr a wx wy sx sy px py b)

-- | Performs the opposite of unwrap.
--
-- [ArrayFire Docs](http://arrayfire.org/docs/group__image__func__wrap.htm)
--
wrap
  :: Array a
  -- ^ the input 'Array'
  -> Int
  -- ^ is the output's dimension 0 size
  -> Int
  -- ^ is the output's dimension 1 size
  -> Int
  -- ^ is the window size along dimension 0
  -> Int
  -- ^ is the window size along dimension 1
  -> Int
  -- ^ is the stride along dimension 0
  -> Int
  -- ^ is the stride along dimension 1
  -> Int
  -- ^ is the padding along dimension 0
  -> Int
  -- ^ is the padding along dimension 1
  -> Bool
  -- ^ determines whether an output patch is formed from a column (if true) or a row (if false)
  -> Array a
  -- ^ is an array with the input's columns (or rows) reshaped as patches
wrap in' (fromIntegral -> ox) (fromIntegral -> oy) (fromIntegral -> wx) (fromIntegral -> wy)
         (fromIntegral -> sx) (fromIntegral -> sy) (fromIntegral -> px) (fromIntegral -> py) (fromIntegral . fromEnum -> b)
  = in' `op1` (\ptr a -> af_wrap ptr a ox oy wx wy sx sy px py b)

-- | Summed Area Tables.
--
-- [ArrayFire Docs](http://arrayfire.org/docs/group__image__func__sat.htm)
--
-- RGB (Red, Green, Blue) is the most common format used in computer imaging. RGB stores individual values for red, green and blue, and hence the 3 values per pixel. A combination of these three values produces the gamut of unique colors.
--
sat
  :: Array a
  -- ^ the input 'Array'
  -> Array a
  -- ^ is the summed area table on input image(s)
sat = (`op1` af_sat)

-- | YCbCr to RGB colorspace converter
--
-- [ArrayFire Docs](http://arrayfire.org/docs/group__image__func__ycbcr2rgb.htm)
--
-- YCbCr is a family of color spaces used as a part of the color image pipeline in video and digital photography systems where Y is luma component and Cb & Cr are the blue-difference and red-difference chroma components.
--
ycbcr2rgb
  :: Array a
  -> YccStd
  -> Array a
ycbcr2rgb a y = a `op1` (\p k -> af_ycbcr2rgb p k (fromAFYccStd y))

-- | RGB to YCbCr colorspace converter.
--
-- [ArrayFire Docs](http://arrayfire.org/docs/group__image__func__rgb2ycbcr.htm)
--
-- RGB (Red, Green, Blue) is the most common format used in computer imaging. RGB stores individual values for red, green and blue, and hence the 3 values per pixel. A combination of these three values produces the gamut of unique colors.
--
rgb2ycbcr
  :: Array a
  -- ^ is an array in the RGB color space
  -> YccStd
  -- ^ specifies the ITU-R BT "xyz" standard which determines the Kb, Kr values used in colorspace conversion equation
  -> Array a
  -- ^ is an 'Array' in the YCbCr color space
rgb2ycbcr a y = a `op1` (\p k -> af_rgb2ycbcr p k (fromAFYccStd y))

-- | Finding different properties of image regions.
--
-- [ArrayFire Docs](http://arrayfire.org/docs/group__image__func__moments.htm)
--
-- C Interface for calculating image moment(s) of a single image.
--
moments
  :: Array a
  -- ^ is an array of image(s)
  -> MomentType
  -- ^ is moment(s) to calculate
  -> Array a
  -- ^ is an array containing the calculated moments
moments in' m =
  in' `op1` (\p k -> af_moments p k (fromMomentType m))

-- | Finding different properties of image regions.
--
-- [ArrayFire Docs](http://arrayfire.org/docs/group__image__func__moments.htm)
--
-- C Interface for calculating image moment(s) of a single image.
--
momentsAll
  :: Array a
  -- ^ is the input image
  -> MomentType
  -- ^ is moment(s) to calculate
  -> Double
  -- ^ is a pointer to a pre-allocated array where the calculated moment(s) will be placed. User is responsible for ensuring enough space to hold all requested moments
momentsAll in' m =
  in' `infoFromArray` (\p a -> af_moments_all p a (fromMomentType m))

-- | Canny Edge Detector
--
-- [ArrayFire Docs](http://arrayfire.org/docs/group__image__func__canny.htm)
--
-- The Canny edge detector is an edge detection operator that uses a multi-stage algorithm to detect a wide range of edges in images.
--
canny
  :: Array a
  -- ^ the input image
  -> CannyThreshold
  -- ^ determines if user set high threshold is to be used or not.
  -> Float
  -- ^ is the lower threshold % of the maximum or auto-derived high threshold
  -> Float
  -- ^ is the higher threshold % of maximum value in gradient image used in hysteresis procedure. This value is ignored if AF_CANNY_THRESHOLD_AUTO_OTSU is chosen as af_canny_threshold
  -> Int
  -- ^ is the window size of sobel kernel for computing gradient direction and magnitude
  -> Bool
  -- ^ indicates if L1 norm(faster but less accurate) is used to compute image gradient magnitude instead of L2 norm.
  -> Array a
  -- ^ is an binary array containing edges
canny in' (fromCannyThreshold -> canny') low high (fromIntegral -> window) (fromIntegral . fromEnum -> fast) =
  in' `op1` (\p a -> af_canny p a canny' low high window fast)

-- | Anisotropic Smoothing Filter.
--
-- [ArrayFire Docs](http://arrayfire.org/docs/group__image__func__anisotropic__diffusion.htm)
--
-- C Interface for anisotropic diffusion.
--
anisotropicDiffusion
  :: Array a
  -- ^ is the input image, expects non-integral (float/double) typed af_array
  -> Float
  -- ^ is the time step used in solving the diffusion equation.
  -> Float
  -- ^ parameter controls the sensitivity of conductance in diffusion equation.
  -> Int
  -- ^ is the number of times the diffusion step is performed.
  -> FluxFunction
  -- ^ indicates whether quadratic or exponential flux function is used by algorithm.
  -> DiffusionEq
  -- ^ will let the user choose what kind of diffusion method to perform.
  -> Array a
  -- ^ is an 'Array' containing anisotropically smoothed image pixel values
anisotropicDiffusion in' ts con (fromIntegral -> iter) (fromFluxFunction -> flux) (fromDiffusionEq -> diff) =
  in' `op1` (\p a -> af_anisotropic_diffusion p a ts con iter flux diff)

-- iterativeDeconv
--   :: Array a
--   -> Array a
--   -> Int
--   -> Float
--   -> IterativeDeconvAlgo
--   -> Array a
-- iterativeDeconv in1 in2 (fromIntegral -> i) f1 (fromIterativeDeconvAlgo -> algo) =
--   op2 in1 in2 (\p a k -> af_iterative_deconv p a k i f1 algo)

-- inverseDeconv
--   :: Array a
--   -> Array a
--   -> Float
--   -> InverseDeconvAlgo
--   -> Array a
-- inverseDeconv in1 in2 f1 (fromInverseDeconvAlgo -> algo) =
--   op2 in1 in2 (\p a k -> af_inverse_deconv p a k f1 algo)
