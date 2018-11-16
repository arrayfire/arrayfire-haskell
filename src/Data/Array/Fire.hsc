{-# LANGUAGE CPP #-}
module Data.Array.Fire where

import Foreign
import Foreign.C.Types

#include "fire.h"

afVersion = #const AF_API_VERSION

newtype AFDType = AFDType { afDType :: Int }
  deriving (Show, Eq)

#{enum AFDType, AFDType
 , f32 = f32
 , c32 = c32
 , f64 = f64
 , c64 = c64
 , b8 = b8
 , s32 = s32
 , u32 = u32
 , u8 = u8
 , s64 = s64
 , u64 = u64
 , s16 = s16
 , u16 = u16
 }

newtype AFError = AFError { afError :: Int }
  deriving (Show, Eq)

#{enum AFError, AFError
 , afSuccess = AF_SUCCESS
 , afErrNoMem = AF_ERR_NO_MEM
 , afErrDriver = AF_ERR_DRIVER
 }

newtype AFArray = AFArray (ForeignPtr AFArray)
