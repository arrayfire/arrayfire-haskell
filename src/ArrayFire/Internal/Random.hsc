{-# LANGUAGE CPP #-}
module ArrayFire.Internal.Random where

import ArrayFire.Internal.Defines

import Foreign.Ptr
import Foreign.C.Types

#include "af/random.h"
foreign import ccall unsafe "af_create_random_engine"
    af_create_random_engine :: Ptr AFRandomEngine -> AFRandomEngineType -> UIntL -> IO AFErr
foreign import ccall unsafe "af_retain_random_engine"
    af_retain_random_engine :: Ptr AFRandomEngine -> AFRandomEngine -> IO AFErr
foreign import ccall unsafe "af_random_engine_set_type"
    af_random_engine_set_type :: Ptr AFRandomEngine -> AFRandomEngineType -> IO AFErr
foreign import ccall unsafe "af_random_engine_get_type"
    af_random_engine_get_type :: Ptr AFRandomEngineType -> AFRandomEngine -> IO AFErr
foreign import ccall unsafe "af_random_uniform"
    af_random_uniform :: Ptr AFArray -> CUInt -> Ptr DimT -> AFDtype -> AFRandomEngine -> IO AFErr
foreign import ccall unsafe "af_random_normal"
    af_random_normal :: Ptr AFArray -> CUInt -> Ptr DimT -> AFDtype -> AFRandomEngine -> IO AFErr
foreign import ccall unsafe "af_random_engine_set_seed"
    af_random_engine_set_seed :: Ptr AFRandomEngine -> UIntL -> IO AFErr
foreign import ccall unsafe "af_get_default_random_engine"
    af_get_default_random_engine :: Ptr AFRandomEngine -> IO AFErr
foreign import ccall unsafe "af_set_default_random_engine_type"
    af_set_default_random_engine_type :: AFRandomEngineType -> IO AFErr
foreign import ccall unsafe "af_random_engine_get_seed"
    af_random_engine_get_seed :: Ptr UIntL -> AFRandomEngine -> IO AFErr
foreign import ccall unsafe "af_release_random_engine"
    af_release_random_engine :: AFRandomEngine -> IO AFErr
foreign import ccall unsafe "af_randu"
    af_randu :: Ptr AFArray -> CUInt -> Ptr DimT -> AFDtype -> IO AFErr
foreign import ccall unsafe "af_randn"
    af_randn :: Ptr AFArray -> CUInt -> Ptr DimT -> AFDtype -> IO AFErr
foreign import ccall unsafe "af_set_seed"
    af_set_seed :: UIntL -> IO AFErr
foreign import ccall unsafe "af_get_seed"
    af_get_seed :: Ptr UIntL -> IO AFErr
