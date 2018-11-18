module ArrayFire.Internal.Data where

import ArrayFire.Internal.Defines

import Data.Word

import Data.Int

#include "data.h"

#include "extra.h"

import Foreign.Ptr

foreign import ccall unsafe "af_constant_complex"
    af_constant_complex :: Ptr AFArray -> Double -> Double -> Word32 -> Ptr Word64 -> AFDtype -> IO AFErr
foreign import ccall unsafe "af_constant_long"
    af_constant_long :: Ptr AFArray -> Int64 -> Word32 -> Ptr Word64 -> IO AFErr
foreign import ccall unsafe "af_constant_ulong"
    af_constant_ulong :: Ptr AFArray -> Word64 -> Word32 -> Ptr Word64 -> IO AFErr
foreign import ccall unsafe "af_range"
    af_range :: Ptr AFArray -> Word32 -> Ptr Word64 -> Int -> AFDtype -> IO AFErr
foreign import ccall unsafe "af_iota"
    af_iota :: Ptr AFArray -> Word32 -> Ptr Word64 -> Word32 -> Ptr Word64 -> AFDtype -> IO AFErr
foreign import ccall unsafe "af_identity"
    af_identity :: Ptr AFArray -> Word32 -> Ptr Word64 -> AFDtype -> IO AFErr
foreign import ccall unsafe "af_diag_create"
    af_diag_create :: Ptr AFArray -> AFArray -> Int -> IO AFErr
foreign import ccall unsafe "af_diag_extract"
    af_diag_extract :: Ptr AFArray -> AFArray -> Int -> IO AFErr
foreign import ccall unsafe "af_join"
    af_join :: Ptr AFArray -> Int -> AFArray -> AFArray -> IO AFErr
foreign import ccall unsafe "af_join_many"
    af_join_many :: Ptr AFArray -> Int -> Word32 -> Ptr AFArray -> IO AFErr
foreign import ccall unsafe "af_tile"
    af_tile :: Ptr AFArray -> AFArray -> Word32 -> Word32 -> Word32 -> Word32 -> IO AFErr
foreign import ccall unsafe "af_reorder"
    af_reorder :: Ptr AFArray -> AFArray -> Word32 -> Word32 -> Word32 -> Word32 -> IO AFErr
foreign import ccall unsafe "af_shift"
    af_shift :: Ptr AFArray -> AFArray -> Int -> Int -> Int -> Int -> IO AFErr
foreign import ccall unsafe "af_moddims"
    af_moddims :: Ptr AFArray -> AFArray -> Word32 -> Ptr Word64 -> IO AFErr
foreign import ccall unsafe "af_flat"
    af_flat :: Ptr AFArray -> AFArray -> IO AFErr
foreign import ccall unsafe "af_flip"
    af_flip :: Ptr AFArray -> AFArray -> Word32 -> IO AFErr
foreign import ccall unsafe "af_lower"
    af_lower :: Ptr AFArray -> AFArray -> Bool -> IO AFErr
foreign import ccall unsafe "af_upper"
    af_upper :: Ptr AFArray -> AFArray -> Bool -> IO AFErr
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