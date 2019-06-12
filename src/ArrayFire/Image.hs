{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module ArrayFire.Image where



import Data.Typeable





import ArrayFire.Types
import ArrayFire.FFI
import ArrayFire.Internal.Image


gradient :: Array a -> (Array a, Array a)
gradient a = a `op2p` af_gradient

loadImage :: String -> Bool -> IO (Array a)
loadImage s b = loadAFImage s b af_load_image

saveImage :: Array a -> String -> IO ()
saveImage a s = afSaveImage a s af_save_image

-- af_err af_load_image_memory(af_array *out, const void* ptr);
-- af_err af_save_image_memory(void** ptr, const af_array in, const af_image_format format);
-- af_err af_delete_image_memory(void* ptr);

loadImageNative :: String -> IO (Array a)
loadImageNative s = loadAFImageNative s af_load_image_native

saveImageNative :: Array a -> String -> IO ()
saveImageNative a s = afSaveImage a s af_save_image_native

isImageIOAvailable :: IO Bool
isImageIOAvailable = afCall1 af_is_image_io_available

resize
  :: Array a
  -> Int
  -> Int
  -> InterpType
  -> Array a
resize a (fromIntegral -> d0) (fromIntegral -> d1) (fromInterpType -> interp) =
  a `op1` (\ptr x -> af_resize ptr x d0 d1 interp)

transform
  :: Array a
  -> Array a
  -> Int
  -> Int
  -> InterpType
  -> Bool
  -> Array a
transform inp trans (fromIntegral -> d0) (fromIntegral -> d1) (fromInterpType -> interp) inverse =
  op2 inp trans (\ptr a1 a2 -> af_transform ptr a1 a2 d0 d1 interp inverse)

transformCoordinates
  :: Array a
  -> Float
  -> Float
  -> Array a
transformCoordinates a d0 d1 =
  a `op1` (\ptr x -> af_transform_coordinates ptr x d0 d1)

rotate
  :: Array a
  -> Float
  -> Bool
  -> InterpType
  -> Array a
rotate a theta crop (fromInterpType -> interp) =
  a `op1` (\ptr x -> af_rotate ptr x theta crop interp)

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

skew
  :: Array a
  -> Float
  -> Float
  -> Int
  -> Int
  -> InterpType
  -> Bool
  -> Array a
skew a trans0 trans1 (fromIntegral -> odim0) (fromIntegral -> odim1) (fromInterpType -> interp) b =
  a `op1` (\ptr x -> af_skew ptr x trans0 trans1 odim0 odim1 interp b)

histogram
  :: Array a
  -> Int
  -> Double
  -> Double
  -> Array a
histogram a (fromIntegral -> b) c d =
  a `op1` (\ptr x -> af_histogram ptr x b c d)

dilate
  :: Array a
  -> Array a
  -> Array a
dilate in' mask = op2 in' mask af_dilate

dilate3
  :: Array a
  -> Array a
  -> Array a
dilate3 in' mask = op2 in' mask af_dilate3

erode
  :: Array a
  -> Array a
  -> Array a
erode in' mask = op2 in' mask af_erode

erode3
  :: Array a
  -> Array a
  -> Array a
erode3 in' mask = op2 in' mask af_erode3

bilateral
  :: Array a
  -> Float
  -> Float
  -> Bool
  -> Array a
bilateral in' a b c = in' `op1` (\ptr k -> af_bilateral ptr k a b c)

meanShift
  :: Array a
  -> Float
  -> Float
  -> Int
  -> Bool
  -> Array a
meanShift in' a b (fromIntegral -> c) d = in' `op1` (\ptr k -> af_mean_shift ptr k a b c d)

minFilt
  :: Array a
  -> Int
  -> Int
  -> BorderType
  -> Array a
minFilt in' (fromIntegral -> a) (fromIntegral -> b) (fromBorderType -> c) =
  in' `op1` (\ptr k -> af_minfilt ptr k a b c)

maxFilt
  :: Array a
  -> Int
  -> Int
  -> BorderType
  -> Array a
maxFilt in' (fromIntegral -> a) (fromIntegral -> b) (fromBorderType -> c) =
  in' `op1` (\ptr k -> af_maxfilt ptr k a b c)

regions
  :: forall a . AFType a
  => Array a
  -> Connectivity
  -> Array a
regions in' (fromConnectivity -> conn) =
  in' `op1` (\ptr k -> af_regions ptr k conn dtype)
    where
      dtype = afType (Proxy @ a)

sobel_operator
  :: Array a
  -> Int
  -> (Array a, Array a)
sobel_operator in' (fromIntegral -> kerSize) =
  in' `op2p` (\ptrA ptrB k -> af_sobel_operator ptrA ptrB k kerSize)

rgb2gray
  :: Array a
  -> Float
  -> Float
  -> Float
  -> Array a
rgb2gray in' a b c =
  in' `op1` (\ptr k -> af_rgb2gray ptr k a b c)

gray2rgb
  :: Array a
  -> Float
  -> Float
  -> Float
  -> Array a
gray2rgb in' a b c = in' `op1` (\ptr k -> af_gray2rgb ptr k a b c)

histEqual
  :: Array a
  -> Array a
  -> Array a
histEqual in' mask = op2 in' mask af_hist_equal

gaussianKernel
  :: Int
  -> Int
  -> Double
  -> Double
  -> Array a
gaussianKernel i1 i2 d1 d2 =
  createArray (\ptr -> af_gaussian_kernel ptr i1 i2 d1 d2)

hsv2rgb
  :: Array a
  -> Array a
hsv2rgb = (`op1` af_hsv2rgb)

rgb2hsv
  :: Array a
  -> Array a
rgb2hsv = (`op1` af_rgb2hsv)

colorSpace
  :: Array a
  -> CSpace
  -> CSpace
  -> Array a
colorSpace in' (fromCSpace -> to) (fromCSpace -> from) =
  in' `op1` (\p a -> af_color_space p a to from)

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
unwrap in' (fromIntegral -> wx) (fromIntegral -> wy) (fromIntegral -> sx) (fromIntegral ->  sy) (fromIntegral -> px) (fromIntegral -> py) b
  = in' `op1` (\ptr a -> af_unwrap ptr a wx wy sx sy px py b)

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
         (fromIntegral -> sx) (fromIntegral -> sy) (fromIntegral -> px) (fromIntegral -> py) b
  = in' `op1` (\ptr a -> af_wrap ptr a ox oy wx wy sx sy px py b)

sat
  :: Array a
  -> Array a
sat = (`op1` af_sat)

ycbcr2rgb
  :: Array a
  -> YccStd
  -> Array a
ycbcr2rgb a y = a `op1` (\p k -> af_ycbcr2rgb p k (fromAFYccStd y))

rgb2ycbcr
  :: Array a
  -> YccStd
  -> Array a
rgb2ycbcr a y = a `op1` (\p k -> af_rgb2ycbcr p k (fromAFYccStd y))

moments
  :: Array a
  -> MomentType
  -> Array a
moments in' m =
  in' `op1` (\p k -> af_moments p k (fromMomentType m))

momentsAll
  :: Array a
  -> MomentType
  -> Double
momentsAll in' m =
  in' `infoFromArray` (\p a -> af_moments_all p a (fromMomentType m))

canny
  :: Array a
  -> CannyThreshold
  -> Float
  -> Float
  -> Int
  -> Bool
  -> Array a
canny in' (fromCannyThreshold -> canny') low high (fromIntegral -> window) fast =
  in' `op1` (\p a -> af_canny p a canny' low high window fast)

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

iterativeDeconv
  :: Array a
  -> Array a
  -> Int
  -> Float
  -> IterativeDeconvAlgo
  -> Array a
iterativeDeconv in1 in2 (fromIntegral -> i) f1 (fromIterativeDeconvAlgo -> algo) =
  op2 in1 in2 (\p a k -> af_iterative_deconv p a k i f1 algo)

inverseDeconv
  :: Array a
  -> Array a
  -> Float
  -> InverseDeconvAlgo
  -> Array a
inverseDeconv in1 in2 f1 (fromInverseDeconvAlgo -> algo) =
  op2 in1 in2 (\p a k -> af_inverse_deconv p a k f1 algo)

