{-# LANGUAGE CPP #-}
module ArrayFire.Internal.Image where

import ArrayFire.Internal.Defines

import Foreign.Ptr
import Foreign.C.Types

#include "af/image.h"
foreign import ccall unsafe "af_gradient"
    af_gradient :: Ptr AFArray -> Ptr AFArray -> AFArray -> IO AFErr
foreign import ccall unsafe "af_load_image"
    af_load_image :: Ptr AFArray -> Ptr CChar -> CBool -> IO AFErr
foreign import ccall unsafe "af_save_image"
    af_save_image :: Ptr CChar -> AFArray -> IO AFErr
foreign import ccall unsafe "af_load_image_memory"
    af_load_image_memory :: Ptr AFArray -> Ptr () -> IO AFErr
foreign import ccall unsafe "af_save_image_memory"
    af_save_image_memory :: Ptr (Ptr ()) -> AFArray -> AFImageFormat -> IO AFErr
foreign import ccall unsafe "af_delete_image_memory"
    af_delete_image_memory :: Ptr () -> IO AFErr
foreign import ccall unsafe "af_load_image_native"
    af_load_image_native :: Ptr AFArray -> Ptr CChar -> IO AFErr
foreign import ccall unsafe "af_save_image_native"
    af_save_image_native :: Ptr CChar -> AFArray -> IO AFErr
foreign import ccall unsafe "af_is_image_io_available"
    af_is_image_io_available :: Ptr CBool -> IO AFErr
foreign import ccall unsafe "af_resize"
    af_resize :: Ptr AFArray -> AFArray -> DimT -> DimT -> AFInterpType -> IO AFErr
foreign import ccall unsafe "af_transform"
    af_transform :: Ptr AFArray -> AFArray -> AFArray -> DimT -> DimT -> AFInterpType -> CBool -> IO AFErr
foreign import ccall unsafe "af_transform_coordinates"
    af_transform_coordinates :: Ptr AFArray -> AFArray -> Float -> Float -> IO AFErr
foreign import ccall unsafe "af_rotate"
    af_rotate :: Ptr AFArray -> AFArray -> Float -> CBool -> AFInterpType -> IO AFErr
foreign import ccall unsafe "af_translate"
    af_translate :: Ptr AFArray -> AFArray -> Float -> Float -> DimT -> DimT -> AFInterpType -> IO AFErr
foreign import ccall unsafe "af_scale"
    af_scale :: Ptr AFArray -> AFArray -> Float -> Float -> DimT -> DimT -> AFInterpType -> IO AFErr
foreign import ccall unsafe "af_skew"
    af_skew :: Ptr AFArray -> AFArray -> Float -> Float -> DimT -> DimT -> AFInterpType -> CBool -> IO AFErr
foreign import ccall unsafe "af_histogram"
    af_histogram :: Ptr AFArray -> AFArray -> CUInt -> Double -> Double -> IO AFErr
foreign import ccall unsafe "af_dilate"
    af_dilate :: Ptr AFArray -> AFArray -> AFArray -> IO AFErr
foreign import ccall unsafe "af_dilate3"
    af_dilate3 :: Ptr AFArray -> AFArray -> AFArray -> IO AFErr
foreign import ccall unsafe "af_erode"
    af_erode :: Ptr AFArray -> AFArray -> AFArray -> IO AFErr
foreign import ccall unsafe "af_erode3"
    af_erode3 :: Ptr AFArray -> AFArray -> AFArray -> IO AFErr
foreign import ccall unsafe "af_bilateral"
    af_bilateral :: Ptr AFArray -> AFArray -> Float -> Float -> CBool -> IO AFErr
foreign import ccall unsafe "af_mean_shift"
    af_mean_shift :: Ptr AFArray -> AFArray -> Float -> Float -> CUInt -> CBool -> IO AFErr
foreign import ccall unsafe "af_minfilt"
    af_minfilt :: Ptr AFArray -> AFArray -> DimT -> DimT -> AFBorderType -> IO AFErr
foreign import ccall unsafe "af_maxfilt"
    af_maxfilt :: Ptr AFArray -> AFArray -> DimT -> DimT -> AFBorderType -> IO AFErr
foreign import ccall unsafe "af_regions"
    af_regions :: Ptr AFArray -> AFArray -> AFConnectivity -> AFDtype -> IO AFErr
foreign import ccall unsafe "af_sobel_operator"
    af_sobel_operator :: Ptr AFArray -> Ptr AFArray -> AFArray -> CUInt -> IO AFErr
foreign import ccall unsafe "af_rgb2gray"
    af_rgb2gray :: Ptr AFArray -> AFArray -> Float -> Float -> Float -> IO AFErr
foreign import ccall unsafe "af_gray2rgb"
    af_gray2rgb :: Ptr AFArray -> AFArray -> Float -> Float -> Float -> IO AFErr
foreign import ccall unsafe "af_hist_equal"
    af_hist_equal :: Ptr AFArray -> AFArray -> AFArray -> IO AFErr
foreign import ccall unsafe "af_gaussian_kernel"
    af_gaussian_kernel :: Ptr AFArray -> CInt -> CInt -> Double -> Double -> IO AFErr
foreign import ccall unsafe "af_hsv2rgb"
    af_hsv2rgb :: Ptr AFArray -> AFArray -> IO AFErr
foreign import ccall unsafe "af_rgb2hsv"
    af_rgb2hsv :: Ptr AFArray -> AFArray -> IO AFErr
foreign import ccall unsafe "af_color_space"
    af_color_space :: Ptr AFArray -> AFArray -> AFCSpace -> AFCSpace -> IO AFErr
foreign import ccall unsafe "af_unwrap"
    af_unwrap :: Ptr AFArray -> AFArray -> DimT -> DimT -> DimT -> DimT -> DimT -> DimT -> CBool -> IO AFErr
foreign import ccall unsafe "af_wrap"
    af_wrap :: Ptr AFArray -> AFArray -> DimT -> DimT -> DimT -> DimT -> DimT -> DimT -> DimT -> DimT -> CBool -> IO AFErr
foreign import ccall unsafe "af_sat"
    af_sat :: Ptr AFArray -> AFArray -> IO AFErr
foreign import ccall unsafe "af_ycbcr2rgb"
    af_ycbcr2rgb :: Ptr AFArray -> AFArray -> AFYccStd -> IO AFErr
foreign import ccall unsafe "af_rgb2ycbcr"
    af_rgb2ycbcr :: Ptr AFArray -> AFArray -> AFYccStd -> IO AFErr
foreign import ccall unsafe "af_moments"
    af_moments :: Ptr AFArray -> AFArray -> AFMomentType -> IO AFErr
foreign import ccall unsafe "af_moments_all"
    af_moments_all :: Ptr Double -> AFArray -> AFMomentType -> IO AFErr
foreign import ccall unsafe "af_canny"
    af_canny :: Ptr AFArray -> AFArray -> AFCannyThreshold -> Float -> Float -> CUInt -> CBool -> IO AFErr
foreign import ccall unsafe "af_anisotropic_diffusion"
    af_anisotropic_diffusion :: Ptr AFArray -> AFArray -> Float -> Float -> CUInt -> AFFluxFunction -> AFDiffusionEq -> IO AFErr
