module ArrayFire.Internal.Exception where

import ArrayFire.Internal.Defines

import Data.Word

import Data.Int

#include "exception.h"

#include "extra.h"

import Foreign.Ptr

foreign import ccall unsafe "af_err_to_string"
    af_err_to_string :: AFErr -> IO (Ptr Char)