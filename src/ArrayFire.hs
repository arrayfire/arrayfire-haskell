module ArrayFire
  ( module ArrayFire.Algorithm
  , module ArrayFire.Arith
  , module ArrayFire.Array
  , module ArrayFire.Backend
  , module ArrayFire.BLAS
  , module ArrayFire.Data
  , module ArrayFire.Device
  , module ArrayFire.Exception
  , module ArrayFire.Features
  , module ArrayFire.Graphics
  , module ArrayFire.Image
  , module ArrayFire.Index
  , module ArrayFire.LAPACK
  , module ArrayFire.Random
  , module ArrayFire.Signal
  , module ArrayFire.Sparse
  , module ArrayFire.Statistics
  , module ArrayFire.Types
  , module ArrayFire.Util
  , module ArrayFire.Vision
  , module ArrayFire.FFI
  ) where

import ArrayFire.Algorithm
import ArrayFire.Arith
import ArrayFire.Array
import ArrayFire.Backend
import ArrayFire.BLAS
import ArrayFire.Data
import ArrayFire.Device
import ArrayFire.Exception
import ArrayFire.Features
import ArrayFire.Graphics
import ArrayFire.Image
import ArrayFire.Index
import ArrayFire.LAPACK
import ArrayFire.Random
import ArrayFire.Signal
import ArrayFire.Sparse
import ArrayFire.Statistics
import ArrayFire.Types
import ArrayFire.Util
import ArrayFire.Vision
import ArrayFire.FFI hiding (createArray)

-- newtype Array a = Array (A.Array a)

-- instance A.AFType a => Num (Array a) where
--   Array x + Array y = add x y True
--   Array x * Array y = mul x y True
--   abs (Array x) = ArrayFire.Arith.abs a
--   signum (Array x) = ArrayFire.Arith.sign (Array x)
--   negate (Array x) = negate x
--   Array x - Array y = sub x y True
--   fromInteger n = error "ok"
