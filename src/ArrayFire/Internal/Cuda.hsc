module ArrayFire.Internal.CUDA where

import ArrayFire.Internal.Defines

import Data.Word

import Data.Int

#include "cuda.h"

#include "extra.h"

import Foreign.Ptr

foreign import ccall unsafe "afcu_get_native_id"
    afcu_get_native_id :: Ptr Int -> Int -> IO AFErr
foreign import ccall unsafe "afcu_set_native_id"
    afcu_set_native_id :: Int -> IO AFErr