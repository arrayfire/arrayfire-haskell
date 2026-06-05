--------------------------------------------------------------------------------
-- |
-- Module      : ArrayFire.Index
-- Copyright   : David Johnson (c) 2019-2020
-- License     : BSD 3
-- Maintainer  : David Johnson <code@dmj.io>
-- Stability   : Experimental
-- Portability : GHC
--
-- Functions for indexing into an 'Array'
--
--------------------------------------------------------------------------------
module ArrayFire.Index where

import ArrayFire.Internal.Index
import ArrayFire.Internal.Types
import ArrayFire.FFI
import ArrayFire.Exception

import Foreign
import Foreign.ForeignPtr (touchForeignPtr)

import System.IO.Unsafe
import Control.Exception

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

-- | Lookup an Array by keys along a specified dimension
lookup
  :: Array a
  -- ^ Input Array
  -> Array Int
  -- ^ Indices
  -> Int
  -- ^ Dimension
  -> Array a
lookup a b n = op2 a b $ \p x y -> af_lookup p x y (fromIntegral n)

-- | Assign values into an 'Array' slice defined by 'Seq' indices
--
-- @
-- >>> let a = vector \@Double 5 [1..]
-- >>> assignSeq a [Seq 1 3 1] (vector \@Double 3 [0,0,0])
-- @
assignSeq
  :: Array a
  -- ^ Destination array
  -> [Seq]
  -- ^ Indices defining the slice to assign into
  -> Array a
  -- ^ Source array
  -> Array a
  -- ^ Result with values written at the specified indices
assignSeq (Array fptr) seqs (Array rhsFptr) =
  unsafePerformIO . mask_ $
    withForeignPtr fptr $ \ptr ->
      withForeignPtr rhsFptr $ \rhsPtr ->
        withArray (toAFSeq <$> seqs) $ \sptr ->
          alloca $ \aptr -> do
            throwAFError =<< af_assign_seq aptr ptr n sptr rhsPtr
            Array <$> (newForeignPtr af_release_array_finalizer =<< peek aptr)
  where
    n = fromIntegral (length seqs)

-- | Index into an 'Array' using generalized 'Index' values (arrays or sequences)
--
-- @
-- >>> let a = matrix \@Double (3,3) [[1..],[1..],[1..]]
-- >>> indexGen a [seqIdx (Seq 0 1 1) False, seqIdx (Seq 0 1 1) False]
-- @
indexGen
  :: Array a
  -- ^ Input array
  -> [Index]
  -- ^ List of 'Index' values (one per dimension)
  -> Array a
  -- ^ Indexed result
indexGen (Array fptr) indices =
  unsafePerformIO . mask_ $
    withForeignPtr fptr $ \ptr -> do
      afIndices <- traverse toAFIndex indices
      withArray afIndices $ \iptr ->
        alloca $ \aptr -> do
          throwAFError =<< af_index_gen aptr ptr (fromIntegral n) iptr
          mapM_ touchIdxFPtr indices
          Array <$> (newForeignPtr af_release_array_finalizer =<< peek aptr)
  where
    n = length indices
    touchIdxFPtr (ArrIndex _ (Array p)) = touchForeignPtr p
    touchIdxFPtr _ = pure ()

-- | Assign values into an 'Array' using generalized 'Index' values
--
-- @
-- >>> let a = matrix \@Double (3,3) [[1..],[1..],[1..]]
-- >>> let b = matrix \@Double (2,2) [[0,0],[0,0]]
-- >>> assignGen a [seqIdx (Seq 0 1 1) False, seqIdx (Seq 0 1 1) False] b
-- @
assignGen
  :: Array a
  -- ^ Destination array
  -> [Index]
  -- ^ List of 'Index' values defining the slice to assign into
  -> Array a
  -- ^ Source array
  -> Array a
  -- ^ Result with values written at the specified indices
assignGen (Array fptr) indices (Array rhsFptr) =
  unsafePerformIO . mask_ $
    withForeignPtr fptr $ \ptr ->
      withForeignPtr rhsFptr $ \rhsPtr -> do
        afIndices <- traverse toAFIndex indices
        withArray afIndices $ \iptr ->
          alloca $ \aptr -> do
            throwAFError =<< af_assign_gen aptr ptr (fromIntegral n) iptr rhsPtr
            mapM_ touchIdxFPtr indices
            Array <$> (newForeignPtr af_release_array_finalizer =<< peek aptr)
  where
    n = length indices
    touchIdxFPtr (ArrIndex _ (Array p)) = touchForeignPtr p
    touchIdxFPtr _ = pure ()

-- | A special 'Seq' value representing the entire axis of an 'Array'.
--
-- Use this instead of @Prelude.span@.
-- Hard-coded from include\/af\/seq.h because FFI cannot import static const values.
afSpan :: Seq
afSpan = Seq 1 1 0
