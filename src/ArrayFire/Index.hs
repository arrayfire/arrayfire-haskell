--------------------------------------------------------------------------------
-- |
-- Module      : ArrayFire.Index
-- Copyright   : David Johnson (c) 2019-2026
-- License     : BSD 3
-- Maintainer  : David Johnson <code@dmj.io>
-- Stability   : Experimental
-- Portability : GHC
--
-- Functions for indexing into an 'Array'
--
--------------------------------------------------------------------------------
{-# LANGUAGE FlexibleInstances #-}
module ArrayFire.Index where

import ArrayFire.Internal.Index
import ArrayFire.Internal.Types
import ArrayFire.FFI
import ArrayFire.Exception

import Foreign

import System.IO.Unsafe
import Control.Exception

-- | Index into an 'Array' by 'Seq'
index
  :: Array a
  -- ^ 'Array' argument
  -> [Seq]
  -- ^ 'Seq' to use for indexing
  -> Array a
{-# NOINLINE index #-}
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

-- | Assign values into an 'Array' range defined by 'Seq' indices
--
-- @
-- >>> let a = vector \@Double 5 [1..]
-- >>> assignSeq a [Seq 1 3 1] (vector \@Double 3 [0,0,0])
-- @
assignSeq
  :: Array a
  -- ^ Destination array
  -> [Seq]
  -- ^ Indices defining the range to assign into
  -> Array a
  -- ^ Source array
  -> Array a
  -- ^ Result with values written at the specified indices
{-# NOINLINE assignSeq #-}
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
{-# NOINLINE indexGen #-}
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
  -- ^ List of 'Index' values defining the range to assign into
  -> Array a
  -- ^ Source array
  -> Array a
  -- ^ Result with values written at the specified indices
{-# NOINLINE assignGen #-}
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
-- Hard-coded from include\/af\/seq.h because FFI cannot import static const values.
afSpan :: Seq
afSpan = Seq 1 1 0

-- | Select the full extent of a dimension. Use in tuple indices where you want all elements along an axis.
--
-- @
-- arr ! (range 0 2, full, at 1)
-- @
full :: Index
full = SeqIndex False afSpan

-- | Convert index expressions to a list of 'Index'.
-- Supports a single 'Index' or tuples of up to four 'Index' values
-- (matching ArrayFire's maximum of 4 dimensions).
class ToIndexList a where
  toIndexList :: a -> [Index]

instance ToIndexList Index where
  toIndexList x = [x]

instance ToIndexList (Index, Index) where
  toIndexList (a, b) = [a, b]

instance ToIndexList (Index, Index, Index) where
  toIndexList (a, b, c) = [a, b, c]

instance ToIndexList (Index, Index, Index, Index) where
  toIndexList (a, b, c, d) = [a, b, c, d]

-- | Lift a 'Seq' to an 'Index' for use in tuple-based indexing.
idx :: Seq -> Index
idx s = SeqIndex False s

-- | Index an 'Array'. Accepts a single 'Index' or a tuple of up to four.
--
-- @
-- arr ! at 0                      -- 1D: element 0
-- arr ! range 1 3                 -- 1D: rows 1-3
-- arr ! (range 0 2, at 1)         -- 2D
-- arr ! (range 0 2, full, at 1)   -- 3D, full second axis
-- @
(!) :: ToIndexList ix => Array a -> ix -> Array a
a ! ix = indexGen a (toIndexList ix)
infixl 9 !

-- | Assign into a range of an 'Array'. Lens-style: use with '(&)'.
--
-- @
-- arr & range 1 3 .~ src
-- arr & (range 0 1, at 2) .~ src
-- @
(.~) :: ToIndexList ix => ix -> Array a -> Array a -> Array a
(ix .~ rhs) arr = assignGen arr (toIndexList ix) rhs
infixr 4 .~
