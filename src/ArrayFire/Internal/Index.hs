{-# LANGUAGE CPP #-}
module ArrayFire.Internal.Index where

import ArrayFire.Internal.Defines
import ArrayFire.Internal.Types
import Data.Word
import Data.Int
import Foreign.Ptr
import Foreign.C.Types

#include "af/index.h"
foreign import ccall unsafe "af_index"
    af_index :: Ptr AFArray -> AFArray -> CUInt -> Ptr AFSeq -> IO AFErr
foreign import ccall unsafe "af_lookup"
    af_lookup :: Ptr AFArray -> AFArray -> AFArray -> CUInt -> IO AFErr
foreign import ccall unsafe "af_assign_seq"
    af_assign_seq :: Ptr AFArray -> AFArray -> CUInt -> Ptr AFSeq -> AFArray -> IO AFErr
foreign import ccall unsafe "af_index_gen"
    af_index_gen :: Ptr AFArray -> AFArray -> DimT -> Ptr AFIndex -> IO AFErr
foreign import ccall unsafe "af_assign_gen"
    af_assign_gen :: Ptr AFArray -> AFArray -> DimT -> Ptr AFIndex -> AFArray -> IO AFErr
foreign import ccall unsafe "af_create_indexers"
    af_create_indexers :: Ptr (Ptr AFIndex) -> IO AFErr
foreign import ccall unsafe "af_set_array_indexer"
    af_set_array_indexer :: Ptr AFIndex -> AFArray -> DimT -> IO AFErr
foreign import ccall unsafe "af_set_seq_indexer"
    af_set_seq_indexer :: Ptr AFIndex -> Ptr AFSeq -> DimT -> Bool -> IO AFErr
foreign import ccall unsafe "af_set_seq_param_indexer"
    af_set_seq_param_indexer :: Ptr AFIndex -> Double -> Double -> Double -> DimT -> Bool -> IO AFErr
foreign import ccall unsafe "af_release_indexers"
    af_release_indexers :: Ptr AFIndex -> IO AFErr