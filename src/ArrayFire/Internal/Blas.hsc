module ArrayFire.Internal.BLAS where

import ArrayFire.Internal.Defines

import Data.Word

import Data.Int

#include "blas.h"

#include "extra.h"

import Foreign.Ptr

foreign import ccall unsafe "af_dot"
    af_dot :: Ptr AFArray -> AFArray -> AFArray -> AFMatProp -> AFMatProp -> IO AFErr
foreign import ccall unsafe "af_dot_all"
    af_dot_all :: Ptr Double -> Ptr Double -> AFArray -> AFArray -> AFMatProp -> AFMatProp -> IO AFErr
foreign import ccall unsafe "af_transpose"
    af_transpose :: Ptr AFArray -> AFArray -> Bool -> IO AFErr
foreign import ccall unsafe "af_transpose_inplace"
    af_transpose_inplace :: AFArray -> Bool -> IO AFErr