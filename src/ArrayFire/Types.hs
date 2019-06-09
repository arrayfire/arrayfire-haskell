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
newtype Features = Features (ForeignPtr ())


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

data Backend
  = Default
  | CPU
  | CUDA
  | OpenCL
  deriving (Show, Eq, Ord)

toBackend :: AFBackend -> Backend
toBackend (AFBackend 0) = Default
toBackend (AFBackend 1) = CPU
toBackend (AFBackend 2) = CUDA
toBackend (AFBackend 4) = OpenCL

toAFBackend :: Backend -> AFBackend
toAFBackend Default = (AFBackend 0)
toAFBackend CPU = (AFBackend 1)
toAFBackend CUDA = (AFBackend 2)
toAFBackend OpenCL = (AFBackend 4)

toBackends :: Int -> [Backend]
toBackends 0 = []
toBackends 1 = [CPU]
toBackends 2 = [CUDA]
toBackends 3 = [CPU,CUDA]
toBackends 4 = [OpenCL]
toBackends 5 = [CPU,OpenCL]
toBackends 6 = [CUDA,OpenCL]
toBackends 7 = [CPU,CUDA,OpenCL]

data MatProp
  = None
  | Trans
  | CTrans
  | Conj
  | Upper
  | Lower
  | DiagUnit
  | Sym
  | PosDef
  | Orthog
  | TriDiag
  | BlockDiag
  deriving (Show, Eq, Ord)

fromMatProp
  :: AFMatProp
  -> MatProp
fromMatProp (AFMatProp 0) = None
fromMatProp (AFMatProp 1) = Trans
fromMatProp (AFMatProp 2) = CTrans
fromMatProp (AFMatProp 4) = Conj
fromMatProp (AFMatProp 32) = Upper
fromMatProp (AFMatProp 64) = Lower
fromMatProp (AFMatProp 128) = DiagUnit
fromMatProp (AFMatProp 512) = Sym
fromMatProp (AFMatProp 1024) = PosDef
fromMatProp (AFMatProp 2048) = Orthog
fromMatProp (AFMatProp 4096) = TriDiag
fromMatProp (AFMatProp 8192) = BlockDiag
fromMatProp x = error $ "Invalid AFMatProp value: " <> show x

toMatProp
  :: MatProp
  -> AFMatProp
toMatProp None = (AFMatProp 0)
toMatProp Trans = (AFMatProp 1)
toMatProp CTrans = (AFMatProp 2)
toMatProp Conj = (AFMatProp 4)
toMatProp Upper = (AFMatProp 32)
toMatProp Lower = (AFMatProp 64)
toMatProp DiagUnit = (AFMatProp 128)
toMatProp Sym = (AFMatProp 512)
toMatProp PosDef = (AFMatProp 1024)
toMatProp Orthog = (AFMatProp 2048)
toMatProp TriDiag = (AFMatProp 4096)
toMatProp BlockDiag = (AFMatProp 8192)

data BinaryOp
  = Add
  | Mul
  | Min
  | Max
  deriving (Show, Eq, Ord)

toBinaryOp :: BinaryOp -> AFBinaryOp
toBinaryOp Add = AFBinaryOp 0
toBinaryOp Mul = AFBinaryOp 1
toBinaryOp Min = AFBinaryOp 2
toBinaryOp Max = AFBinaryOp 3

fromBinaryOp :: AFBinaryOp -> BinaryOp
fromBinaryOp (AFBinaryOp 0) = Add
fromBinaryOp (AFBinaryOp 1) = Mul
fromBinaryOp (AFBinaryOp 2) = Min
fromBinaryOp (AFBinaryOp 3) = Max
fromBinaryOp x = error ("Invalid Binary Op: " <> show x)

data Storage
  = Dense
  | CSR
  | CSC
  | COO
  deriving (Show, Eq, Ord, Enum)

toStorage :: Storage -> AFStorage
toStorage = AFStorage . fromEnum

fromStorage :: AFStorage -> Storage
fromStorage (AFStorage x)
  | x `elem` [0..3] = toEnum x
  | otherwise = error $ "Invalid Storage " <> (show x)
