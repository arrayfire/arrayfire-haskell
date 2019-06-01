{-# LANGUAGE CPP #-}
module ArrayFire.Internal.BLAS where

import ArrayFire.Internal.Defines
import ArrayFire.Internal.Types
import Data.Word
import Data.Int
import Foreign.Ptr
import Foreign.C.Types

#include "af/blas.h"
foreign import ccall unsafe "af_matmul"
    af_matmul :: Ptr AFArray -> AFArray -> AFArray -> AFMatProp -> AFMatProp -> IO AFErr
foreign import ccall unsafe "af_dot"
    af_dot :: Ptr AFArray -> AFArray -> AFArray -> AFMatProp -> AFMatProp -> IO AFErr
foreign import ccall unsafe "af_dot_all"
    af_dot_all :: Ptr Double -> Ptr Double -> AFArray -> AFArray -> AFMatProp -> AFMatProp -> IO AFErr
foreign import ccall unsafe "af_transpose"
    af_transpose :: Ptr AFArray -> AFArray -> Bool -> IO AFErr
foreign import ccall unsafe "af_transpose_inplace"
    af_transpose_inplace :: AFArray -> Bool -> IO AFErr