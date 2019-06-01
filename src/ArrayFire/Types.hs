{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE KindSignatures      #-}
module ArrayFire.Types where

import ArrayFire.Internal.Defines
import Data.Kind
import Data.Complex
import Data.Proxy
import Data.Word
import Foreign.C.Types
import Foreign.ForeignPtr
import GHC.Int
import GHC.TypeLits

newtype Array a = Array (ForeignPtr ())

-- instance Eq a => Eq (Array a) where

-- | Mapping of Haskell types to ArrayFire types
class AFType a where
  afType :: Proxy a -> AFDtype

instance AFType Double where
  afType Proxy = f64

instance AFType Float where
  afType Proxy = f32

instance AFType (Complex Double) where
  afType Proxy = c64

instance AFType Bool where
  afType Proxy = b8

instance AFType CBool where
  afType Proxy = b8

instance AFType Int32 where
  afType Proxy = s32

instance AFType Word32 where
  afType Proxy = u32

instance AFType Word8 where
  afType Proxy = u8

instance AFType Int64 where
  afType Proxy = s64

instance AFType Int where
  afType Proxy = s64

instance AFType Int16 where
  afType Proxy = s16

instance AFType Word16 where
  afType Proxy = u16

instance AFType Word64 where
  afType Proxy = u64

class Dims (a :: k) where
  toDims :: Proxy a -> [DimT]

instance KnownNat a => Dims (a :: Nat) where
  toDims Proxy = [DimT x]
    where
      x = fromIntegral $ natVal (Proxy @ a)

instance (KnownNat a, KnownNat b) => Dims '(a, b) where
  toDims Proxy = [DimT x, DimT y]
    where
      x = fromIntegral $ natVal (Proxy @ a)
      y = fromIntegral $ natVal (Proxy @ b)

instance (KnownNat a, KnownNat b, KnownNat c) => Dims '(a,b,c) where
  toDims Proxy = [DimT x, DimT y, DimT z]
    where
      x = fromIntegral $ natVal (Proxy @ a)
      y = fromIntegral $ natVal (Proxy @ b)
      z = fromIntegral $ natVal (Proxy @ c)

instance (KnownNat a, KnownNat b, KnownNat c, KnownNat d) =>
  Dims '(a,b,c,d) where
  toDims Proxy = [DimT w, DimT x, DimT y, DimT z]
    where
      w = fromIntegral $ natVal (Proxy @ a)
      x = fromIntegral $ natVal (Proxy @ b)
      y = fromIntegral $ natVal (Proxy @ c)
      z = fromIntegral $ natVal (Proxy @ d)
