{-# LANGUAGE RecordWildCards #-}
module Data.Array.Fire.Internal.Complex where

#include "complex.h"

import Foreign.Storable

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
