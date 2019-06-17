{-# LANGUAGE ViewPatterns #-}
module ArrayFire.Signal where

import Data.Complex

import ArrayFire.FFI
import ArrayFire.Internal.Defines
import ArrayFire.Internal.Signal
import ArrayFire.Types

approx1
  :: AFType a
  => Array a
  -> Array a
  -> InterpType
  -> Float
  -> Array a
approx1 arr1 arr2 (fromInterpType -> i1) f =
  op2 arr1 arr2 (\p x y -> af_approx1 p x y i1 f)

approx2
  :: AFType a
  => Array a
  -> Array a
  -> Array a
  -> InterpType
  -> Float
  -> Array a
approx2 arr1 arr2 arr3 (fromInterpType -> i1) f =
  op3 arr1 arr2 arr3 (\p x y z -> af_approx2 p x y z i1 f)

approx1Uniform
  :: AFType a
  => Array a
  -> Array a
  -> Int
  -> Double
  -> Double
  -> InterpType
  -> Float
  -> Array a
approx1Uniform arr1 arr2 i1 d1 d2 (fromInterpType -> interp) f =
  op2 arr1 arr2 (\p x y -> af_approx1_uniform p x y i1 d1 d2 interp f)

approx2Uniform
  :: AFType a
  => Array a
  -> Array a
  -> Int
  -> Double
  -> Double
  -> Array a
  -> Int
  -> Double
  -> Double
  -> InterpType
  -> Float
  -> Array a
approx2Uniform arr1 arr2 i1 d1 d2 arr3 i2 d3 d4 (fromInterpType -> interp) f =
  op3 arr1 arr2 arr3 (\p x y z -> af_approx2_uniform p x y i1 d1 d2 z i2 d3 d4 interp f)

fft
  :: (AFType a, Fractional a)
  => Array (Complex a)
  -> Double
  -> Int
  -> Array (Complex a)
fft a d x =
  op1 a (\j k -> af_fft j k d (fromIntegral x))

fftInPlace
  :: (AFType a, Fractional a)
  => Array (Complex a)
  -> Double
  -> IO ()
fftInPlace a d = a `inPlace` (flip af_fft_inplace d)

fft2
  :: AFType a
  => Array a
  -> Double
  -> Int
  -> Int
  -> Array a
fft2 a d x y =
  op1 a (\j k -> af_fft2 j k d (fromIntegral x) (fromIntegral y))

fft2_inplace
  :: AFType a
  => Array a
  -> Double
  -> IO ()
fft2_inplace a d = a `inPlace` (flip af_fft2_inplace d)

fft3
  :: AFType a
  => Array a
  -> Double
  -> Int
  -> Int
  -> Int
  -> Array a
fft3 a d x y z =
  op1 a (\j k -> af_fft3 j k d (fromIntegral x) (fromIntegral y) (fromIntegral z))

fft3_inplace
  :: AFType a
  => Array a
  -> Double
  -> IO ()
fft3_inplace a d = a `inPlace` (flip af_fft3_inplace d)

ifft
  :: AFType a
  => Array a
  -> Double
  -> Int
  -> Array a
ifft a d x =
  op1 a (\j k -> af_ifft j k d (fromIntegral x))

ifft_inplace
  :: AFType a
  => Array a
  -> Double
  -> IO ()
ifft_inplace a d = a `inPlace` (flip af_ifft_inplace d)

ifft2
  :: AFType a
  => Array a
  -> Double
  -> Int
  -> Int
  -> Array a
ifft2 a d x y =
  op1 a (\j k -> af_ifft2 j k d (fromIntegral x) (fromIntegral y))

ifft2_inplace
  :: AFType a
  => Array a
  -> Double
  -> IO ()
ifft2_inplace a d = a `inPlace` (flip af_ifft2_inplace d)

ifft3
  :: AFType a
  => Array a
  -> Double
  -> Int
  -> Int
  -> Int
  -> Array a
ifft3 a d x y z =
  op1 a (\j k -> af_ifft3 j k d (fromIntegral x) (fromIntegral y) (fromIntegral z))

ifft3_inplace
  :: AFType a
  => Array a
  -> Double
  -> IO ()
ifft3_inplace a d = a `inPlace` (flip af_ifft3_inplace d)

fftr2c
  :: AFType a
  => Array a
  -> Double
  -> Int
  -> Array a
fftr2c a d x =
  op1 a (\j k -> af_fft_r2c j k d (fromIntegral x))

fft2r2c
  :: AFType a
  => Array a
  -> Double
  -> Int
  -> Int
  -> Array a
fft2r2c a d x y =
  op1 a (\j k -> af_fft2_r2c j k d (fromIntegral x) (fromIntegral y))

fft3r2c
  :: AFType a
  => Array a
  -> Double
  -> Int
  -> Int
  -> Int
  -> Array a
fft3r2c a d x y z =
  op1 a (\j k -> af_fft3_r2c j k d (fromIntegral x) (fromIntegral y) (fromIntegral z))

fftc2r
  :: AFType a
  => Array a
  -> Double
  -> Bool
  -> Array a
fftc2r a cm cd = op1 a (\x y -> af_fft_c2r x y cm cd)

fft2C2r
  :: AFType a
  => Array a
  -> Double
  -> Bool
  -> Array a
fft2C2r a cm cd = op1 a (\x y -> af_fft2_c2r x y cm cd)

fft3C2r
  :: AFType a
  => Array a
  -> Double
  -> Bool
  -> Array a
fft3C2r a cm cd = op1 a (\x y -> af_fft3_c2r x y cm cd)

convolve1
  :: AFType a
  => Array a
  -> Array a
  -> AFConvMode
  -> AFConvDomain
  -> Array a
convolve1 a b cm cd = op2 a b (\x y z -> af_convolve1 x y z cm cd)

convolve2
  :: AFType a
  => Array a
  -> Array a
  -> AFConvMode
  -> AFConvDomain
  -> Array a
convolve2 a b cm cd = op2 a b (\x y z -> af_convolve2 x y z cm cd)

convolve3
  :: AFType a
  => Array a
  -> Array a
  -> AFConvMode
  -> AFConvDomain
  -> Array a
convolve3 a b cm cd = op2 a b (\x y z -> af_convolve3 x y z cm cd)

convolve2Sep
  :: AFType a
  => Array a
  -> Array a
  -> Array a
  -> AFConvMode
  -> Array a
convolve2Sep a b c d = op3 a b c (\x y z j -> af_convolve2_sep x y z j d)

fftConvolve1
  :: AFType a
  => Array a
  -> Array a
  -> AFConvMode
  -> Array a
fftConvolve1 a b c = op2 a b (\x y z -> af_fft_convolve1 x y z c)

fftConvolve2
  :: AFType a
  => Array a
  -> Array a
  -> AFConvMode
  -> Array a
fftConvolve2 a b c = op2 a b (\x y z -> af_fft_convolve2 x y z c)

fftConvolve3
  :: AFType a
  => Array a
  -> Array a
  -> AFConvMode
  -> Array a
fftConvolve3 a b c = op2 a b (\x y z -> af_fft_convolve3 x y z c)

fir
  :: AFType a
  => Array a
  -> Array a
  -> Array a
fir a b = op2 a b af_fir

iir
  :: AFType a
  => Array a
  -> Array a
  -> Array a
  -> Array a
iir a b c = op3 a b c af_iir

medFilt
  :: AFType a
  => Array a
  -> Int
  -> Int
  -> AFBorderType
  -> Array a
medFilt a l w b =
 a `op1` (\x y -> af_medfilt x y (fromIntegral l) (fromIntegral w) b)

medFilt1
  :: AFType a
  => Array a
  -> Int
  -> AFBorderType
  -> Array a
medFilt1 a w b =
 a `op1` (\x y -> af_medfilt1 x y (fromIntegral w) b)

medFilt2
  :: AFType a
  => Array a
  -> Int
  -> Int
  -> AFBorderType
  -> Array a
medFilt2 a l w b =
 a `op1` (\x y -> af_medfilt2 x y (fromIntegral l) (fromIntegral w) b)

setFFTPlanCacheSize
  :: Int
  -> IO ()
setFFTPlanCacheSize =
 afCall . af_set_fft_plan_cache_size . fromIntegral
