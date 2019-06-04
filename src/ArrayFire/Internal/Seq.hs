{-# LANGUAGE CPP #-}
module ArrayFire.Internal.Seq where

import ArrayFire.Internal.Defines
import ArrayFire.Internal.Types
import Data.Word
import Data.Int
import Foreign.Ptr
import Foreign.C.Types

foreign import ccall unsafe "af_make_seq"
    af_make_seq :: Double -> Double -> Double -> IO AFSeq