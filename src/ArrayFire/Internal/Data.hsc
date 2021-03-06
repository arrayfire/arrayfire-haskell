{-# LANGUAGE CPP #-}
module ArrayFire.Internal.Data where

import ArrayFire.Internal.Defines

import Foreign.Ptr
import Foreign.C.Types

#include "af/data.h"
foreign import ccall unsafe "af_constant"
    af_constant :: Ptr AFArray -> Double -> CUInt -> Ptr DimT -> AFDtype -> IO AFErr
foreign import ccall unsafe "af_constant_complex"
    af_constant_complex :: Ptr AFArray -> Double -> Double -> CUInt -> Ptr DimT -> AFDtype -> IO AFErr
foreign import ccall unsafe "af_constant_long"
    af_constant_long :: Ptr AFArray -> IntL -> CUInt -> Ptr DimT -> IO AFErr
foreign import ccall unsafe "af_constant_ulong"
    af_constant_ulong :: Ptr AFArray -> UIntL -> CUInt -> Ptr DimT -> IO AFErr
foreign import ccall unsafe "af_range"
    af_range :: Ptr AFArray -> CUInt -> Ptr DimT -> CInt -> AFDtype -> IO AFErr
foreign import ccall unsafe "af_iota"
    af_iota :: Ptr AFArray -> CUInt -> Ptr DimT -> CUInt -> Ptr DimT -> AFDtype -> IO AFErr
foreign import ccall unsafe "af_identity"
    af_identity :: Ptr AFArray -> CUInt -> Ptr DimT -> AFDtype -> IO AFErr
foreign import ccall unsafe "af_diag_create"
    af_diag_create :: Ptr AFArray -> AFArray -> CInt -> IO AFErr
foreign import ccall unsafe "af_diag_extract"
    af_diag_extract :: Ptr AFArray -> AFArray -> CInt -> IO AFErr
foreign import ccall unsafe "af_join"
    af_join :: Ptr AFArray -> CInt -> AFArray -> AFArray -> IO AFErr
foreign import ccall unsafe "af_join_many"
    af_join_many :: Ptr AFArray -> CInt -> CUInt -> Ptr AFArray -> IO AFErr
foreign import ccall unsafe "af_tile"
    af_tile :: Ptr AFArray -> AFArray -> CUInt -> CUInt -> CUInt -> CUInt -> IO AFErr
foreign import ccall unsafe "af_reorder"
    af_reorder :: Ptr AFArray -> AFArray -> CUInt -> CUInt -> CUInt -> CUInt -> IO AFErr
foreign import ccall unsafe "af_shift"
    af_shift :: Ptr AFArray -> AFArray -> CInt -> CInt -> CInt -> CInt -> IO AFErr
foreign import ccall unsafe "af_moddims"
    af_moddims :: Ptr AFArray -> AFArray -> CUInt -> Ptr DimT -> IO AFErr
foreign import ccall unsafe "af_flat"
    af_flat :: Ptr AFArray -> AFArray -> IO AFErr
foreign import ccall unsafe "af_flip"
    af_flip :: Ptr AFArray -> AFArray -> CUInt -> IO AFErr
foreign import ccall unsafe "af_lower"
    af_lower :: Ptr AFArray -> AFArray -> CBool -> IO AFErr
foreign import ccall unsafe "af_upper"
    af_upper :: Ptr AFArray -> AFArray -> CBool -> IO AFErr
foreign import ccall unsafe "af_select"
    af_select :: Ptr AFArray -> AFArray -> AFArray -> AFArray -> IO AFErr
foreign import ccall unsafe "af_select_scalar_r"
    af_select_scalar_r :: Ptr AFArray -> AFArray -> AFArray -> Double -> IO AFErr
foreign import ccall unsafe "af_select_scalar_l"
    af_select_scalar_l :: Ptr AFArray -> AFArray -> Double -> AFArray -> IO AFErr
foreign import ccall unsafe "af_replace"
    af_replace :: AFArray -> AFArray -> AFArray -> IO AFErr
foreign import ccall unsafe "af_replace_scalar"
    af_replace_scalar :: AFArray -> AFArray -> Double -> IO AFErr
