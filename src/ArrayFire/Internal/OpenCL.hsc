module ArrayFire.Internal.OpenCL where

import ArrayFire.Internal.Defines

#include "opencl.h"

#include "extra.h"

import Foreign.Ptr

foreign import ccall unsafe "afcl_get_platform"
    afcl_get_platform :: Ptr AfclPlatform -> IO AFErr