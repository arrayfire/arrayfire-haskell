{-# LANGUAGE CPP #-}
module ArrayFire.Internal.Statistics where

import ArrayFire.Internal.Defines
import ArrayFire.Internal.Types
import Data.Word
import Data.Int
import Foreign.Ptr
import Foreign.C.Types

foreign import ccall unsafe "af_mean"
    af_mean :: Ptr AFArray -> AFArray -> DimT -> IO AFErr
foreign import ccall unsafe "af_mean_weighted"
    af_mean_weighted :: Ptr AFArray -> AFArray -> AFArray -> DimT -> IO AFErr
foreign import ccall unsafe "af_var"
    af_var :: Ptr AFArray -> AFArray -> Bool -> DimT -> IO AFErr
foreign import ccall unsafe "af_var_weighted"
    af_var_weighted :: Ptr AFArray -> AFArray -> AFArray -> DimT -> IO AFErr
foreign import ccall unsafe "af_stdev"
    af_stdev :: Ptr AFArray -> AFArray -> DimT -> IO AFErr
foreign import ccall unsafe "af_cov"
    af_cov :: Ptr AFArray -> AFArray -> AFArray -> Bool -> IO AFErr
foreign import ccall unsafe "af_median"
    af_median :: Ptr AFArray -> AFArray -> DimT -> IO AFErr
foreign import ccall unsafe "af_mean_all"
    af_mean_all :: Ptr Double -> Ptr Double -> AFArray -> IO AFErr
foreign import ccall unsafe "af_mean_all_weighted"
    af_mean_all_weighted :: Ptr Double -> Ptr Double -> AFArray -> AFArray -> IO AFErr
foreign import ccall unsafe "af_var_all"
    af_var_all :: Ptr Double -> Ptr Double -> AFArray -> Bool -> IO AFErr
foreign import ccall unsafe "af_var_all_weighted"
    af_var_all_weighted :: Ptr Double -> Ptr Double -> AFArray -> AFArray -> IO AFErr
foreign import ccall unsafe "af_stdev_all"
    af_stdev_all :: Ptr Double -> Ptr Double -> AFArray -> IO AFErr
foreign import ccall unsafe "af_median_all"
    af_median_all :: Ptr Double -> Ptr Double -> AFArray -> IO AFErr
foreign import ccall unsafe "af_corrcoef"
    af_corrcoef :: Ptr Double -> Ptr Double -> AFArray -> AFArray -> IO AFErr
foreign import ccall unsafe "af_topk"
    af_topk :: Ptr AFArray -> Ptr AFArray -> AFArray -> Int -> Int -> AFTopkFunction -> IO AFErr