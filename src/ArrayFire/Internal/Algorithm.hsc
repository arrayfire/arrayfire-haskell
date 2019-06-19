{-# LANGUAGE CPP #-}
module ArrayFire.Internal.Algorithm where

import ArrayFire.Internal.Defines
import ArrayFire.Internal.Types
import Data.Word
import Data.Int
import Foreign.Ptr
import Foreign.C.Types

#include "af/algorithm.h"
foreign import ccall unsafe "af_sum"
    af_sum :: Ptr AFArray -> AFArray -> CInt -> IO AFErr
foreign import ccall unsafe "af_sum_nan"
    af_sum_nan :: Ptr AFArray -> AFArray -> CInt -> Double -> IO AFErr
foreign import ccall unsafe "af_product"
    af_product :: Ptr AFArray -> AFArray -> CInt -> IO AFErr
foreign import ccall unsafe "af_product_nan"
    af_product_nan :: Ptr AFArray -> AFArray -> CInt -> Double -> IO AFErr
foreign import ccall unsafe "af_min"
    af_min :: Ptr AFArray -> AFArray -> CInt -> IO AFErr
foreign import ccall unsafe "af_max"
    af_max :: Ptr AFArray -> AFArray -> CInt -> IO AFErr
foreign import ccall unsafe "af_all_true"
    af_all_true :: Ptr AFArray -> AFArray -> CInt -> IO AFErr
foreign import ccall unsafe "af_any_true"
    af_any_true :: Ptr AFArray -> AFArray -> CInt -> IO AFErr
foreign import ccall unsafe "af_count"
    af_count :: Ptr AFArray -> AFArray -> CInt -> IO AFErr
foreign import ccall unsafe "af_sum_all"
    af_sum_all :: Ptr Double -> Ptr Double -> AFArray -> IO AFErr
foreign import ccall unsafe "af_sum_nan_all"
    af_sum_nan_all :: Ptr Double -> Ptr Double -> AFArray -> Double -> IO AFErr
foreign import ccall unsafe "af_product_all"
    af_product_all :: Ptr Double -> Ptr Double -> AFArray -> IO AFErr
foreign import ccall unsafe "af_product_nan_all"
    af_product_nan_all :: Ptr Double -> Ptr Double -> AFArray -> Double -> IO AFErr
foreign import ccall unsafe "af_min_all"
    af_min_all :: Ptr Double -> Ptr Double -> AFArray -> IO AFErr
foreign import ccall unsafe "af_max_all"
    af_max_all :: Ptr Double -> Ptr Double -> AFArray -> IO AFErr
foreign import ccall unsafe "af_all_true_all"
    af_all_true_all :: Ptr Double -> Ptr Double -> AFArray -> IO AFErr
foreign import ccall unsafe "af_any_true_all"
    af_any_true_all :: Ptr Double -> Ptr Double -> AFArray -> IO AFErr
foreign import ccall unsafe "af_count_all"
    af_count_all :: Ptr Double -> Ptr Double -> AFArray -> IO AFErr
foreign import ccall unsafe "af_imin"
    af_imin :: Ptr AFArray -> Ptr AFArray -> AFArray -> CInt -> IO AFErr
foreign import ccall unsafe "af_imax"
    af_imax :: Ptr AFArray -> Ptr AFArray -> AFArray -> CInt -> IO AFErr
foreign import ccall unsafe "af_imin_all"
    af_imin_all :: Ptr Double -> Ptr Double -> Ptr CUInt -> AFArray -> IO AFErr
foreign import ccall unsafe "af_imax_all"
    af_imax_all :: Ptr Double -> Ptr Double -> Ptr CUInt -> AFArray -> IO AFErr
foreign import ccall unsafe "af_accum"
    af_accum :: Ptr AFArray -> AFArray -> CInt -> IO AFErr
foreign import ccall unsafe "af_scan"
    af_scan :: Ptr AFArray -> AFArray -> CInt -> AFBinaryOp -> CBool -> IO AFErr
foreign import ccall unsafe "af_scan_by_key"
    af_scan_by_key :: Ptr AFArray -> AFArray -> AFArray -> CInt -> AFBinaryOp -> CBool -> IO AFErr
foreign import ccall unsafe "af_where"
    af_where :: Ptr AFArray -> AFArray -> IO AFErr
foreign import ccall unsafe "af_diff1"
    af_diff1 :: Ptr AFArray -> AFArray -> CInt -> IO AFErr
foreign import ccall unsafe "af_diff2"
    af_diff2 :: Ptr AFArray -> AFArray -> CInt -> IO AFErr
foreign import ccall unsafe "af_sort"
    af_sort :: Ptr AFArray -> AFArray -> CUInt -> CBool -> IO AFErr
foreign import ccall unsafe "af_sort_index"
    af_sort_index :: Ptr AFArray -> Ptr AFArray -> AFArray -> CUInt -> CBool -> IO AFErr
foreign import ccall unsafe "af_sort_by_key"
    af_sort_by_key :: Ptr AFArray -> Ptr AFArray -> AFArray -> AFArray -> CUInt -> CBool -> IO AFErr
foreign import ccall unsafe "af_set_unique"
    af_set_unique :: Ptr AFArray -> AFArray -> CBool -> IO AFErr
foreign import ccall unsafe "af_set_union"
    af_set_union :: Ptr AFArray -> AFArray -> AFArray -> CBool -> IO AFErr
foreign import ccall unsafe "af_set_intersect"
    af_set_intersect :: Ptr AFArray -> AFArray -> AFArray -> CBool -> IO AFErr