module ArrayFire.Internal.Index where

import ArrayFire.Internal.Defines

import Data.Word

import Data.Int

#include "index.h"

#include "extra.h"

import Foreign.Ptr

foreign import ccall unsafe "af_lookup"
    af_lookup :: Ptr AFArray -> AFArray -> AFArray -> Word32 -> IO AFErr
foreign import ccall unsafe "af_assign_seq"
    af_assign_seq :: Ptr AFArray -> AFArray -> Word32 -> Ptr AFSeq -> AFArray -> IO AFErr
foreign import ccall unsafe "af_index_gen"
    af_index_gen :: Ptr AFArray -> AFArray -> Word64 -> Ptr AFIndexT -> IO AFErr
foreign import ccall unsafe "af_assign_gen"
    af_assign_gen :: Ptr AFArray -> AFArray -> Word64 -> Ptr AFIndexT -> AFArray -> IO AFErr
foreign import ccall unsafe "af_create_indexers"
    af_create_indexers :: Ptr (Ptr AFIndexT) -> IO AFErr
foreign import ccall unsafe "af_set_array_indexer"
    af_set_array_indexer :: Ptr AFIndexT -> AFArray -> Word64 -> IO AFErr
foreign import ccall unsafe "af_set_seq_indexer"
    af_set_seq_indexer :: Ptr AFIndexT -> Ptr AFSeq -> Word64 -> Bool -> IO AFErr
foreign import ccall unsafe "af_set_seq_param_indexer"
    af_set_seq_param_indexer :: Ptr AFIndexT -> Double -> Double -> Double -> Word64 -> Bool -> IO AFErr
foreign import ccall unsafe "af_release_indexers"
    af_release_indexers :: Ptr AFIndexT -> IO AFErr