{-# LANGUAGE ViewPatterns #-}
--------------------------------------------------------------------------------
-- |
-- Module      : ArrayFire.Signal
-- Copyright   : David Johnson (c) 2019-2020
-- License     : BSD 3
-- Maintainer  : David Johnson <djohnson.m@gmail.com>
-- Stability   : Experimental
-- Portability : GHC
--
-- [ArrayFire Docs](http://arrayfire.org/docs/group__signal__func__fft.htm)
--
--------------------------------------------------------------------------------
module ArrayFire.Signal where

import Data.Complex

import ArrayFire.FFI
import ArrayFire.Internal.Signal
import ArrayFire.Internal.Types

-- | 'approx1' interpolates data along the first dimensions
--
-- [ArrayFire Docs](http://arrayfire.org/docs/group__signal__func__approx1.htm)
--
-- Interpolation is performed assuming input data is equally spaced with indices in the range [0, n). The positions are sampled with respect to data at these locations.
--
-- >>> input = vector 3 [10,20,30]
-- >>> positions = vector 5 [0.0, 0.5, 1.0, 1.5, 2.0]
-- >>> approx1 @Double input positions Cubic 0.0
-- ArrayFire Array
-- [5 1 1 1]
--   10.0000    13.7500    20.0000    26.2500    30.0000
--
approx1
  :: AFType a
  => Array a
  -- ^ the input array
  -> Array a
  -- ^ array contains the interpolation locations
  -> InterpType
  -- ^ is the interpolation type, it can take one of the values defined by 'InterpType'
  -> Float
  -- ^ is the value that will set in the output array when certain index is out of bounds
  -> Array a
  -- ^ is the array with interpolated values
approx1 arr1 arr2 (fromInterpType -> i1) f =
  op2 arr1 arr2 (\p x y -> af_approx1 p x y i1 f)

-- | approx2 performs interpolation on data along the first and second dimensions.
--
-- [ArrayFire Docs](http://arrayfire.org/docs/group__signal__func__approx2.htm)
--
-- Interpolation is performed assuming input data is equally spaced with indices in the range [0, n) along each dimension. The positions are sampled with respect to data at these locations.
--
-- >>> input = matrix @Double (3,3) [ [ 1.0,1.0,1.0 ], [ 2.0, 2.0, 2.0 ], [ 3.0,3.0,3.0 ] ]
-- >>> positions1 = matrix @Double (2,2) [ [ 0.5,1.5 ],[ 0.5,1.5 ] ]
-- >>> positions2 = matrix @Double (2,2) [ [ 0.5,0.5 ],[ 1.5,1.5 ] ]
-- >>> approx2 @Double input positions1 positions2 Cubic 0.0
-- ArrayFire Array
-- [2 2 1 1]
--    1.3750     1.3750
--    2.6250     2.6250
--
approx2
  :: AFType a
  => Array a
  -- ^ is the input array
  -> Array a
  -- ^ array contains the interpolation locations for first dimension
  -> Array a
  -- ^ array contains the interpolation locations for second dimension
  -> InterpType
  -- ^ is the interpolation type, it can take one of the values defined by 'InterpType'
  -> Float
  -- ^ is the value that will set in the output array when certain index is out of bounds
  -> Array a
  -- ^	is the array with interpolated values
approx2 arr1 arr2 arr3 (fromInterpType -> i1) f =
  op3 arr1 arr2 arr3 (\p x y z -> af_approx2 p x y z i1 f)

-- DMJ: Where did these functions go? Were they removed?
-- http://arrayfire.org/docs/group__approx__mat.htm
-- approx1Uniform
--   :: AFType a
--   => Array a
--   -> Array a
--   -> Int
--   -> Double
--   -> Double
--   -> InterpType
--   -> Float
--   -> Array a
-- approx1Uniform arr1 arr2 (fromIntegral -> i1) d1 d2 (fromInterpType -> interp) f =
--   op2 arr1 arr2 (\p x y -> af_approx1_uniform p x y i1 d1 d2 interp f)

-- approx2Uniform
--   :: AFType a
--   => Array a
--   -> Array a
--   -> Int
--   -> Double
--   -> Double
--   -> Array a
--   -> Int
--   -> Double
--   -> Double
--   -> InterpType
--   -> Float
--   -> Array a
-- approx2Uniform arr1 arr2 (fromIntegral -> i1) d1 d2 arr3 (fromIntegral -> i2) d3 d4 (fromInterpType -> interp) f =
--   op3 arr1 arr2 arr3 (\p x y z -> af_approx2_uniform p x y i1 d1 d2 z i2 d3 d4 interp f)

-- | Fast Fourier Transform
--
-- [ArrayFire Docs](http://arrayfire.org/docs/group__signal__func__fft.htm#ga64d0db9e59c9410ba738591ad146a884)
--
-- The Fast Fourier Transform (FFT) is an efficient algorithm to compute the discrete Fourier transform (DFT) of a signal or array. This is most commonly used to convert data in the time (or space) domain to the frequency domain, Then, the inverse FFT (iFFT) is used to return the data to the original domain.
--
-- >>> fft (vector @Double 10 [1..]) 2.0 10
-- ArrayFire Array
-- [2 2 1 1]
--    1.3750     1.3750 
--    2.6250     2.6250
--
fft
  :: (AFType a, Fractional a)
  => Array a
  -- ^ input 'Array'
  -> Double
  -- ^ the normalization factor with which the input is scaled after the transformation is applied
  -> Int
  -- ^ is the length of output signals - used to either truncate or pad the input signals.
  -> Array a
  -- ^ is the transformed array
fft a d (fromIntegral -> x) =
  op1 a (\j k -> af_fft j k d x)

-- | Fast Fourier Transform (in-place)
--
-- [ArrayFire Docs](http://arrayfire.org/docs/group__signal__func__fft.htm#gaa2f03c9ee1cb80dc184c0b0a13176da1)
--
-- C Interface for fast fourier transform on one dimensional signals.
--
-- *Note* The input in must be a complex array
-- 
fftInPlace
  :: (AFType a, Fractional a)
  => Array (Complex a)
  -- ^ is the input array on entry and the output of 1D forward fourier transform at exit
  -> Double
  -- ^ is the normalization factor with which the input is scaled after the transformation is applied
  -> IO ()
fftInPlace a d = a `inPlace` (flip af_fft_inplace d)

-- | Fast Fourier Transform (2-dimensional)
--
-- [ArrayFire Docs](http://arrayfire.org/docs/group__signal__func__fft2.htm#gaab3fb1ed398e208a615036b4496da611)
--
-- C Interface for fast fourier transform on two dimensional signals.
-- 
fft2
  :: AFType a
  => Array a
  -- ^ the input array
  -> Double
  -- ^ the normalization factor with which the input is scaled after the transformation is applied
  -> Int
  -- ^ is the length of output signals along first dimension - used to either truncate/pad the input
  -> Int
  -- ^ is the length of output signals along second dimension - used to either truncate/pad the input
  -> Array a
  -- ^ the transformed array
fft2 a d x y =
  op1 a (\j k -> af_fft2 j k d (fromIntegral x) (fromIntegral y))

-- | Fast Fourier Transform (2-dimensional, in-place)
--
-- [ArrayFire Docs](http://arrayfire.org/docs/group__signal__func__fft2.htm#gacdeebb3f221ae698833dc4900a172b8c)
--
-- C Interface for fast fourier transform on two dimensional signals.
--
-- *Note* The input in must be a complex array
-- 
fft2_inplace
  :: (Fractional a, AFType a)
  => Array (Complex a)
  -- ^  input array on entry and the output of 2D forward fourier transform on exit
  -> Double
  -- ^ is the normalization factor with which the input is scaled after the transformation is applied
  -> IO ()
fft2_inplace a d = a `inPlace` (flip af_fft2_inplace d)

-- | Fast Fourier Transform (3-dimensional)
--
-- [ArrayFire Docs](http://arrayfire.org/docs/group__signal__func__fft3.htm#ga5138ef1740ece0fde2c796904d733c12)
--
-- C Interface for fast fourier transform on three dimensional signals.
-- 
fft3
  :: AFType a
  => Array a
  -- ^ the input array
  -> Double
  -- ^ the normalization factor with which the input is scaled after the transformation is applied
  -> Int
  -- ^ is the length of output signals along first dimension - used to either truncate/pad the input
  -> Int
  -- ^ is the length of output signals along second dimension - used to either truncate/pad the input
  -> Int
  -- ^ is the length of output signals along third dimension - used to either truncate/pad the input
  -> Array a
  -- ^ the transformed array
fft3 a d x y z =
  op1 a (\j k -> af_fft3 j k d (fromIntegral x) (fromIntegral y) (fromIntegral z))

-- | Fast Fourier Transform (3-dimensional, in-place)
--
-- [ArrayFire Docs](http://arrayfire.org/docs/group__signal__func__fft2.htm#gacdeebb3f221ae698833dc4900a172b8c)
--
-- C Interface for fast fourier transform on three dimensional signals.
--
-- *Note* The input in must be a complex array
-- 
fft3_inplace
  :: (Fractional a, AFType a)
  => Array (Complex a)
  -- ^  input array on entry and the output of 3D forward fourier transform on exit
  -> Double
  -- ^ is the normalization factor with which the input is scaled after the transformation is applied
  -> IO ()
fft3_inplace a d = a `inPlace` (flip af_fft3_inplace d)

-- | Inverse Fast Fourier Transform
--
-- [ArrayFire Docs](http://arrayfire.org/docs/group__signal__func__ifft.htm#ga2d62c120b474b3b937b0425c994645fe)
--
-- C Interface for inverse fast fourier transform on one dimensional signals.
--
ifft
  :: AFType a
  => Array a
  -- ^ the input array
  -> Double
  -- ^ is the normalization factor with which the input is scaled after the transformation is applied
  -> Int
  -- ^  is the length of output signals - used to either truncate or pad the input signals
  -> Array a
  -- ^ the transformed array
ifft a d x =
  op1 a (\j k -> af_ifft j k d (fromIntegral x))

-- | Inverse Fast Fourier Transform (in-place)
--
-- [ArrayFire Docs](http://arrayfire.org/docs/group__signal__func__ifft.htm#ga827379bef0e2cadb382c1b6301c91429)
--
-- C Interface for fast fourier transform on one dimensional signals.
--
ifft_inplace
  :: (AFType a, Fractional a)
  => Array (Complex a)
  -- ^ is the input array on entry and the output of 1D forward fourier transform at exit
  -> Double
  -- ^ is the normalization factor with which the input is scaled after the transformation is applied
  -> IO ()
ifft_inplace a d = a `inPlace` (flip af_ifft_inplace d)


-- | Inverse Fast Fourier Transform (2-dimensional signals)
--
-- [ArrayFire Docs](http://arrayfire.org/docs/group__signal__func__ifft2.htm#ga7cd29c6a35c19240635b62cc5c30dc4f)
--
-- C Interface for inverse fast fourier transform on two dimensional signals.
--
ifft2
  :: AFType a
  => Array a
  -- ^ the input array
  -> Double
  -- ^ the normalization factor with which the input is scaled after the transformation is applied
  -> Int
  -- ^ is the length of output signals along first dimension - used to either truncate/pad the input
  -> Int
  -- ^ is the length of output signals along second dimension - used to either truncate/pad the input
  -> Array a
  -- ^ the transformed array
ifft2 a d x y =
  op1 a (\j k -> af_ifft2 j k d (fromIntegral x) (fromIntegral y))

-- | Inverse Fast Fourier Transform (2-dimensional, in-place)
--
-- [ArrayFire Docs](http://arrayfire.org/docs/group__signal__func__ifft2.htm#ga9e6a165d44306db4552a56d421ce5d05)
--
-- C Interface for fast fourier transform on two dimensional signals.
--
ifft2_inplace
  :: (AFType a, Fractional a)
  => Array (Complex a)
  -- ^ is the input array on entry and the output of 1D forward fourier transform at exit
  -> Double
  -- ^ is the normalization factor with which the input is scaled after the transformation is applied
  -> IO ()
ifft2_inplace a d = a `inPlace` (flip af_ifft2_inplace d)

-- | Inverse Fast Fourier Transform (3-dimensional)
--
-- [ArrayFire Docs](http://arrayfire.org/docs/group__signal__func__ifft3.htm)
--
-- C Interface for inverse fast fourier transform on three dimensional signals.
--
ifft3
  :: AFType a
  => Array a
  -- ^ the input array
  -> Double
  -- ^ the normalization factor with which the input is scaled after the transformation is applied
  -> Int
  -- ^ is the length of output signals along first dimension - used to either truncate/pad the input
  -> Int
  -- ^ is the length of output signals along second dimension - used to either truncate/pad the input
  -> Int
  -- ^ is the length of output signals along third dimension - used to either truncate/pad the input
  -> Array a
  -- ^ the transformed array
ifft3 a d x y z =
  op1 a (\j k -> af_ifft3 j k d (fromIntegral x) (fromIntegral y) (fromIntegral z))

-- | Inverse Fast Fourier Transform (3-dimensional, in-place)
--
-- [ArrayFire Docs](http://arrayfire.org/docs/group__signal__func__ifft3.htm#ga439a7a49723bc6cf77cf4fe7f8dfe334)
--
-- C Interface for fast fourier transform on two dimensional signals.
--
ifft3_inplace
  :: (AFType a, Fractional a)
  => Array (Complex a)
  -- ^ is the input array on entry and the output of 1D forward fourier transform at exit
  -> Double
  -- ^ is the normalization factor with which the input is scaled after the transformation is applied
  -> IO ()
ifft3_inplace a d = a `inPlace` (flip af_ifft3_inplace d)

-- | Real to Complex Fast Fourier Transform
--
-- [ArrayFire Docs](http://arrayfire.org/docs/group__signal__func__fft__r2c.htm#ga7486f342182a18e773f14cc2ab4cb551)
--
-- C Interface for real to complex fast fourier transform for one dimensional signals.
--
-- The first dimension of the output will be of size (pad0 / 2) + 1. The second dimension of the output will be pad1. The third dimension of the output will be pad 2.
--
fftr2c
  :: (Fractional a, AFType a)
  => Array a
  -- ^ is a real array
  -> Double
  -- ^ is the normalization factor with which the input is scaled after the transformation is applied
  -> Int
  -- ^ is the length of output signals along first dimension - used to either truncate/pad the input
  -> Array a
  -- ^ is a complex array containing the non redundant parts of in.
fftr2c a d x =
  op1 a (\j k -> af_fft_r2c j k d (fromIntegral x))

-- | Real to Complex Fast Fourier Transform
--
-- [ArrayFire Docs](http://arrayfire.org/docs/group__signal__func__fft__r2c.htm#ga7486f342182a18e773f14cc2ab4cb551)
--
-- C Interface for real to complex fast fourier transform for two dimensional signals.
--
-- The first dimension of the output will be of size (pad0 / 2) + 1. The second dimension of the output will be pad1. The third dimension of the output will be pad 2.
--
fft2r2c
  :: (Fractional a, AFType a)
  => Array a
  -- ^ is a real array
  -> Double
  -- ^ is the normalization factor with which the input is scaled after the transformation is applied
  -> Int
  -- ^ is the length of output signals along first dimension - used to either truncate/pad the input
  -> Int
  -- ^ is the length of output signals along second dimension - used to either truncate/pad the input
  -> Array a
  -- ^ is a complex array containing the non redundant parts of in.
fft2r2c a d x y =
  op1 a (\j k -> af_fft2_r2c j k d (fromIntegral x) (fromIntegral y))

-- | Real to Complex Fast Fourier Transform
--
-- [ArrayFire Docs](http://arrayfire.org/docs/group__signal__func__fft__r2c.htm#gab4ca074b54218b74d8cfbda63d38be51)
--
-- C Interface for real to complex fast fourier transform for three dimensional signals.
--
-- The first dimension of the output will be of size (pad0 / 2) + 1. The second dimension of the output will be pad1. The third dimension of the output will be pad 2.
--
fft3r2c
  :: (Fractional a, AFType a)
  => Array a
  -- ^ is a real array
  -> Double
  -- ^ is the normalization factor with which the input is scaled after the transformation is applied
  -> Int
  -- ^ is the length of output signals along first dimension - used to either truncate/pad the input
  -> Int
  -- ^ is the length of output signals along second dimension - used to either truncate/pad the input
  -> Int
  -- ^ is the length of output signals along third dimension - used to either truncate/pad the input
  -> Array a
  -- ^ is a complex array containing the non redundant parts of in.
fft3r2c a d x y z =
  op1 a (\j k -> af_fft3_r2c j k d (fromIntegral x) (fromIntegral y) (fromIntegral z))

-- | Complex to Real Fast Fourier Transform
--
-- [ArrayFire Docs](http://arrayfire.org/docs/group__signal__func__fft__c2r.htm#gaa5efdfd84213a4a07d81a5d534cde5ac)
--
-- C Interface for complex to real fast fourier transform for one dimensional signals.
--
-- The first dimension of the output will be 2 * dim0 - 1 if is_odd is true else 2 * dim0 - 2 where dim0 is the first dimension of the input. The remaining dimensions are unchanged.
--
fftc2r
  :: AFType a
  => Array a
  -- ^ is a complex array containing only the non redundant parts of the signals.
  -> Double
  -- ^ is the normalization factor with which the input is scaled after the transformation is applied
  -> Bool
  -- ^ is a flag signifying if the output should be even or odd size
  -> Array a
  -- ^ is a real array containing the output of the transform.
fftc2r a cm (fromIntegral . fromEnum -> cd) = op1 a (\x y -> af_fft_c2r x y cm cd)

-- | Complex to Real Fast Fourier Transform (2-dimensional)
--
-- [ArrayFire Docs](http://arrayfire.org/docs/group__signal__func__fft__c2r.htm#gaaa7da16f226cacaffced631e08da4493)
--
-- C Interface for complex to real fast fourier transform for two dimensional signals.
--
-- The first dimension of the output will be 2 * dim0 - 1 if is_odd is true else 2 * dim0 - 2 where dim0 is the first dimension of the input. The remaining dimensions are unchanged.
--
fft2C2r
  :: AFType a
  => Array a
  -- ^ is a complex array containing only the non redundant parts of the signals.
  -> Double
  -- ^ is the normalization factor with which the input is scaled after the transformation is applied
  -> Bool
  -- ^ is a flag signifying if the output should be even or odd size
  -> Array a
  -- ^ is a real array containing the output of the transform.
fft2C2r a cm (fromIntegral . fromEnum -> cd) = op1 a (\x y -> af_fft2_c2r x y cm cd)

-- | Complex to Real Fast Fourier Transform (3-dimensional)
--
-- [ArrayFire Docs](http://arrayfire.org/docs/group__signal__func__fft__c2r.htm#gaa9b3322d9ffab15268919e1f114bed24)
--
-- C Interface for complex to real fast fourier transform for three dimensional signals.
--
-- The first dimension of the output will be 2 * dim0 - 1 if is_odd is true else 2 * dim0 - 2 where dim0 is the first dimension of the input. The remaining dimensions are unchanged.
--
fft3C2r
  :: AFType a
  => Array a
  -- ^ is a complex array containing only the non redundant parts of the signals.
  -> Double
  -- ^ is the normalization factor with which the input is scaled after the transformation is applied
  -> Bool
  -- ^ is a flag signifying if the output should be even or odd size
  -> Array a
  -- ^ is a real array containing the output of the transform.
fft3C2r a cm (fromIntegral . fromEnum -> cd) = op1 a (\x y -> af_fft3_c2r x y cm cd)

-- | Convolution Integral for one dimensional data
--
-- [ArrayFire Docs](http://arrayfire.org/docs/group__signal__func__convolve1.htm#ga25d77b794463b5cd72cd0b7f4af140d7)
--
-- C Interface for convolution on one dimensional signals.
--
-- *Note* The default parameter of domain, AF_CONV_AUTO, heuristically switches between frequency and spatial domain.
--
convolve1
  :: AFType a
  => Array a
  -- ^ the input signal
  -> Array a
  -- ^ the signal that shall be flipped for the convolution operation
  -> ConvMode
  -- ^ indicates if the convolution should be expanded or not(where output size equals input)
  -> ConvDomain
  -- ^ specifies if the convolution should be performed in frequency os spatial domain
  -> Array a
  -- ^ convolved array
convolve1 a b (toConvMode -> cm) (fromConvDomain -> cd) = op2 a b (\x y z -> af_convolve1 x y z cm cd)

-- | Convolution Integral for two dimensional data
--
-- [ArrayFire Docs](http://arrayfire.org/docs/group__signal__func__convolve2.htm#ga25d77b794463b5cd72cd0b7f4af140d7)
--
-- C Interface for convolution on two dimensional signals.
--
-- *Note* The default parameter of domain, AF_CONV_AUTO, heuristically switches between frequency and spatial domain.
--
convolve2
  :: AFType a
  => Array a
  -- ^ the input signal
  -> Array a
  -- ^ the signal that shall be flipped for the convolution operation
  -> ConvMode
  -- ^ indicates if the convolution should be expanded or not(where output size equals input)
  -> ConvDomain
  -- ^ specifies if the convolution should be performed in frequency os spatial domain
  -> Array a
  -- ^ convolved array
convolve2 a b (toConvMode -> cm) (fromConvDomain -> cd) = op2 a b (\x y z -> af_convolve2 x y z cm cd)

-- | Convolution Integral for three dimensional data
--
-- [ArrayFire Docs](http://arrayfire.org/docs/group__signal__func__convolve3.htm#ga25d77b794463b5cd72cd0b7f4af140d7)
--
-- C Interface for convolution on three dimensional signals.
--
-- *Note* The default parameter of domain, AF_CONV_AUTO, heuristically switches between frequency and spatial domain.
--
convolve3
  :: AFType a
  => Array a
  -- ^ the input signal
  -> Array a
  -- ^ the signal that shall be flipped for the convolution operation
  -> ConvMode
  -- ^ indicates if the convolution should be expanded or not(where output size equals input)
  -> ConvDomain
  -- ^ specifies if the convolution should be performed in frequency os spatial domain
  -> Array a
  -- ^ convolved array
convolve3 a b (toConvMode -> cm) (fromConvDomain -> cd) =
  op2 a b (\x y z -> af_convolve3 x y z cm cd)

-- | C Interface for separable convolution on two dimensional signals.
--
-- [ArrayFire Docs](http://arrayfire.org/docs/group__signal__func__convolve.htm#gaeb6ba88155cf3ef29d93f97b147e372f)
--
-- C Interface for separable convolution on two dimensional signals.
--
-- *Note* The default parameter of domain, AF_CONV_AUTO, heuristically switches between frequency and spatial domain.
--
convolve2Sep
  :: AFType a
  => Array a
  -- ^ filter that has to be applied along the coloumns
  -> Array a
  -- ^ filter that has to be applied along the rows
  -> Array a
  -- ^ the input array
  -> ConvMode
  -- ^ indicates if the convolution should be expanded or not(where output size equals input)
  -> Array a
  -- ^ convolved array
convolve2Sep a b c (toConvMode -> d) = op3 a b c (\x y z j -> af_convolve2_sep x y z j d)

-- DMJ: did this get removed? Can't find in latest docs
-- fftConvolve1
--   :: AFType a
--   => Array a
--   -> Array a
--   -> ConvMode
--   -> Array a
-- fftConvolve1 a b (toConvMode -> c) = op2 a b (\x y z -> af_fft_convolve1 x y z c)

-- | 2D Convolution using Fast Fourier Transform
--  
-- [ArrayFire Docs](http://arrayfire.org/docs/group__signal__func__fft__convolve2.htm#gab52ebe631d8358cdef1b5c8a95550556)
--
-- A convolution is a common operation between a source array, a, and a filter (or kernel) array b. The answer to the convolution is the same as computing the coefficients in polynomial multiplication, if a and b are the coefficients.
--
-- C Interface for FFT-based convolution on two dimensional signals.
--
fftConvolve2
  :: AFType a
  => Array a
  -- ^ is the input signal
  -> Array a
  --  ^ is the signal that shall be used for the convolution operation
  -> ConvMode
  -- ^ indicates if the convolution should be expanded or not(where output size equals input)
  -> Array a
  -- ^  is convolved array
fftConvolve2 a b (toConvMode -> c) = op2 a b (\x y z -> af_fft_convolve2 x y z c)

-- | 3D Convolution using Fast Fourier Transform
--  
-- [ArrayFire Docs](http://arrayfire.org/docs/group__signal__func__fft__convolve3.htm)
--
-- A convolution is a common operation between a source array, a, and a filter (or kernel) array b. The answer to the convolution is the same as computing the coefficients in polynomial multiplication, if a and b are the coefficients.
--
-- C Interface for FFT-based convolution on three dimensional signals.
--
fftConvolve3
  :: AFType a
  => Array a
  -- ^ is the input signal
  -> Array a
  --  ^ is the signal that shall be used for the convolution operation
  -> ConvMode
  -- ^ indicates if the convolution should be expanded or not(where output size equals input)
  -> Array a
  -- ^  is convolved array
fftConvolve3 a b (toConvMode -> c) = op2 a b (\x y z -> af_fft_convolve3 x y z c)

-- | Finite Impulse Filter.
--
-- [ArrayFire Docs](http://arrayfire.org/docs/group__signal__func__fir.htm#ga2a850e69775eede4709e0d607bba240b)
--
-- Finite impulse filters take an input x and a co-efficient array b to generate an output y such that:
--
-- C Interface for finite impulse response filter.
--
fir
  :: AFType a
  => Array a
  -- ^ is the input signal to the filter
  -> Array a
  -- ^ is the array containing the coefficients of the filter
  -> Array a
  -- ^ is the output signal from the filter
fir a b = op2 a b af_fir

-- | Infinite Impulse Filter.
--
-- [ArrayFire Docs](http://arrayfire.org/docs/group__signal__func__iir.htm#ga7adcc364da0a66cdfd2bb351215456c4)
--
-- Infinite impulse filters take an input x and a feedforward array b, feedback array a to generate an output y such that:
--
-- C Interface for infinite impulse response filter.
--
-- *Note* The feedforward coefficients are currently limited to a length of 512
--
iir
  :: AFType a
  => Array a
  -- ^ the array containing the feedforward coefficient
  -> Array a
  -- ^ is the array containing the feedback coefficients
  -> Array a
  -- ^ is the input signal to the filter
  -> Array a
  -- ^ the output signal from the filter
iir a b c = op3 a b c af_iir

-- | Median Filter
--
-- [ArrayFire Docs](http://arrayfire.org/docs/group__image__func__medfilt.htm#gaaf3f62f2de0f4dc315b831e494e1b2c0)
--
-- A median filter is similar to the arbitrary filter except that instead of a weighted sum, the median value of the pixels covered by the kernel is returned.
--
-- C Interface for median filter.
--
medFilt
  :: AFType a
  => Array a
  -- ^ 'Array' is the input image
  -> Int
  -> Int
  -> BorderType
  -> Array a
  -- ^ 'Array' is the processed image
medFilt a l w (fromBorderType -> b) =
 a `op1` (\x y -> af_medfilt x y (fromIntegral l) (fromIntegral w) b)

-- | 1D Median Filter
--
-- [ArrayFire Docs](http://arrayfire.org/docs/group__image__func__medfilt.htm#gad108ea62cbbb5371bd14a17d06384359)
--
-- A median filter is similar to the arbitrary filter except that instead of a weighted sum, the median value of the pixels covered by the kernel is returned.
--
-- C Interface for 1D median filter.
--
medFilt1
  :: AFType a
  => Array a
  -- ^ 'Array' is the input signal
  -> Int
  -- ^ Is the kernel width
  -> BorderType
  -- ^ value will decide what happens to border when running filter in their neighborhood. It takes one of the values [AF_PAD_ZERO | AF_PAD_SYM]
  -> Array a
  -- ^ 'Array' is the processed signal
medFilt1 a w (fromBorderType -> b) =
 a `op1` (\x y -> af_medfilt1 x y (fromIntegral w) b)

-- | 2D Median Filter
--
-- [ArrayFire Docs](http://arrayfire.org/docs/group__image__func__medfilt.htm#ga2cb99dca5842f74f6b9cd28eb187a9cd)
--
-- A median filter is similar to the arbitrary filter except that instead of a weighted sum, the median value of the pixels covered by the kernel is returned.
--
-- C Interface for 2D median filter.
--
medFilt2
  :: AFType a
  => Array a
  -- ^ 'Array' is the input image
  -> Int
  -- ^ the kernel height
  -> Int
  -- ^ the kernel width
  -> BorderType
  -- ^ value will decide what happens to border when running filter in their neighborhood. It takes one of the values [AF_PAD_ZERO | AF_PAD_SYM]
  -> Array a
  -- ^ 'Array' is the processed image
medFilt2 a l w (fromBorderType -> b) =
 a `op1` (\x y -> af_medfilt2 x y (fromIntegral l) (fromIntegral w) b)

-- | C Interface for setting plan cache size.
--
-- [ArrayFire Docs](http://arrayfire.org/docs/group__signal__func__fft.htm#ga4ddef19b43d9a50c97b1a835df60279a)
--
-- This function doesn't do anything if called when CPU backend is active. The plans associated with the most recently used array sizes are cached.
--
-- >>> setFFTPlanCacheSize 2
-- ()
--
setFFTPlanCacheSize
  :: Int
  -- ^ is the number of plans that shall be cached
  -> IO ()
setFFTPlanCacheSize =
 afCall . af_set_fft_plan_cache_size . fromIntegral
