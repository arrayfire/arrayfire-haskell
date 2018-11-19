module ArrayFire.Random where

import Foreign.Marshal
import Foreign.Storable
import Foreign.C.String

import ArrayFire.Internal.Random
import ArrayFire.Exception
import ArrayFire.Internal.Defines

randn arr n dimt typ =
  alloca $ \ptr ->
    alloca $ \d -> do
      poke d dimt
      print =<< af_randn  ptr n d typ
      peek ptr
