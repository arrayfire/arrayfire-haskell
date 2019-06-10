{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}
module ArrayFire.Internal.Types where

#include "af/seq.h"
#include "af/complex.h"
#include "af/graphics.h"
#include "af/index.h"

import Foreign.Storable
import Foreign.C.String
import ArrayFire.Internal.Defines

data AFSeq
  = AFSeq
  { afSeqBegin :: {-# UNPACK #-} !Double
  , afSeqEnd   :: {-# UNPACK #-} !Double
  , afSeqStep  :: {-# UNPACK #-} !Double
  } deriving (Show, Eq)

instance Storable AFSeq where
  sizeOf _ = #{size af_seq}
  alignment _ = #{alignment af_seq}
  peek ptr = do
    afSeqBegin <- #{peek af_seq, begin} ptr
    afSeqEnd <- #{peek af_seq, end} ptr
    afSeqStep <- #{peek af_seq, step} ptr
    pure AFSeq {..}
  poke ptr AFSeq{..} = do
    #{poke af_seq, begin} ptr afSeqBegin
    #{poke af_seq, end} ptr afSeqEnd
    #{poke af_seq, step} ptr afSeqStep

data AFIndex
  = AFIndex
  { afIdx :: !(Either AFArray AFSeq)
  , afIsSeq :: !Bool
  , afIsBatch :: !Bool
  }
instance Storable AFIndex where
  sizeOf _ = #{size af_index_t}
  alignment _ = #{alignment af_index_t}
  peek ptr = do
    afIsSeq <- #{peek af_index_t, isSeq} ptr
    afIsBatch <- #{peek af_index_t, isBatch} ptr
    afIdx <-
      if afIsSeq
        then Left <$> #{peek af_index_t, idx.arr} ptr
        else Right <$> #{peek af_index_t, idx.seq} ptr
    pure AFIndex{..}
  poke ptr AFIndex{..} = do
    case afIdx of
      Left afarr -> #{poke af_index_t, idx.arr} ptr afarr
      Right afseq -> #{poke af_index_t, idx.seq} ptr afseq
    #{poke af_index_t, isSeq} ptr afIsSeq
    #{poke af_index_t, isBatch} ptr afIsBatch

data AFCFloat
  = AFCFloat
  { afcReal :: {-# UNPACK #-} !Float
  , afcImag :: {-# UNPACK #-} !Float
  } deriving (Eq, Show)

instance Storable AFCFloat where
  sizeOf _ = #{size af_cfloat}
  alignment _ = #{alignment af_cfloat}
  peek ptr = do
    afcReal <- #{peek af_cfloat, real} ptr
    afcImag <- #{peek af_cfloat, imag} ptr
    pure AFCFloat{..}
  poke ptr AFCFloat{..} = do
    #{poke af_cfloat, real} ptr afcReal
    #{poke af_cfloat, imag} ptr afcImag

data AFCell
  = AFCell
  { afCellRow :: {-# UNPACK #-} !Int
  , afCellCol :: {-# UNPACK #-} !Int
  , afCellTitle :: {-# UNPACK #-} !CString
  , afCellColorMap :: {-# UNPACK #-} !AFColorMap
  } deriving (Show, Eq)

instance Storable AFCell where
  sizeOf _ = #{size af_cell}
  alignment _ = #{alignment af_cell}
  peek ptr = do
    afCellRow <- #{peek af_cell, row} ptr
    afCellCol <- #{peek af_cell, col} ptr
    afCellTitle <- #{peek af_cell, title} ptr
    afCellColorMap <- #{peek af_cell, cmap} ptr
    pure AFCell{..}
  poke ptr AFCell{..} = do
    #{poke af_cell, row} ptr afCellRow
    #{poke af_cell, col} ptr afCellCol
    #{poke af_cell, title} ptr afCellTitle
    #{poke af_cell, cmap} ptr afCellColorMap
