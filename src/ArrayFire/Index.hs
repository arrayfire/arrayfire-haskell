--------------------------------------------------------------------------------
-- |
-- Module      : ArrayFire.Index
-- Copyright   : David Johnson (c) 2019-2020
-- License     : BSD 3
-- Maintainer  : David Johnson <djohnson.m@gmail.com>
-- Stability   : Experimental
-- Portability : GHC
--
--------------------------------------------------------------------------------
module ArrayFire.Index where

import ArrayFire.Internal.Index
import ArrayFire.Types
import ArrayFire.FFI
import ArrayFire.Exception

import Foreign

import System.IO.Unsafe
import Control.Exception

(!) :: Array a -> Int -> Array a
(!) = undefined

-- | Index into an 'Array' by 'Seq'
index
  :: Array a
  -- ^ 'Array' argument
  -> [Seq]
  -- ^ 'Seq' to use for indexing
  -> Array a
index (Array fptr) seqs =
  unsafePerformIO . mask_ . withForeignPtr fptr $ \ptr -> do
    alloca $ \aptr ->
      withArray (toAFSeq <$> seqs) $ \sptr -> do
        throwAFError =<< af_index aptr ptr n sptr
        Array <$> do
          newForeignPtr af_release_array_finalizer
            =<< peek aptr
   where
     n = fromIntegral (length seqs)

lookup :: Array a -> Array a -> Int -> Array a
lookup a b n = op2 a b $ \p x y -> af_lookup p x y (fromIntegral n)

-- af_err af_assign_seq( af_array *out, const af_array lhs, const unsigned ndims, const af_seq* const indices, const af_array rhs);
assignSeq :: Array a -> Int -> [Seq] -> Array a -> Array a
assignSeq = undefined

-- af_err af_index_gen(  af_array *out, const af_array in, const dim_t ndims, const af_index_t* indices);
indexGen :: Array a -> Int -> [Index a] -> Array a -> Array a
indexGen = undefined

-- af_err af_assingn_gen( af_array *out, const af_array lhs, const dim_t ndims, const af_index_t* indices, const af_array rhs);
assignGen :: Array a -> Int -> [Index a] -> Array a -> Array a
assignGen = undefined

-- af_err af_create_indexers(af_index_t** indexers);
-- af_err af_set_array_indexer(af_index_t* indexer, const af_array idx, const dim_t dim);
-- af_err af_set_seq_indexer(af_index_t* indexer, const af_seq* idx, const dim_t dim, const bool is_batch);
-- af_err af_set_seq_param_indexer(af_index_t* indexer, const double begin, const double end, const double step, const dim_t dim, const bool is_batch);
-- af_err af_release_indexers(af_index_t* indexers);
