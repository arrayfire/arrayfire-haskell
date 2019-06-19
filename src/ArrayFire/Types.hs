{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE ViewPatterns         #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE TypeFamilies         #-}
--------------------------------------------------------------------------------
-- |
-- Module      : ArrayFire.Types
-- Copyright   : David Johnson (c) 2019-2020
-- License     : BSD 3
-- Maintainer  : David Johnson <djohnson.m@gmail.com>
-- Stability   : Experimental
-- Portability : GHC
--
--------------------------------------------------------------------------------
module ArrayFire.Types
  ( module ArrayFire.Types
  , AFException (..)
  , AFExceptionType (..)
  ) where

import ArrayFire.Internal.Defines
import ArrayFire.Internal.Types
import ArrayFire.Exception

import Data.Complex
import Data.Proxy
import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Storable
import GHC.Int

newtype Array a = Array (ForeignPtr ())
newtype Features = Features (ForeignPtr ())
newtype RandomEngine = RandomEngine (ForeignPtr ())
newtype Window = Window (ForeignPtr ())

-- | Mapping of Haskell types to ArrayFire types
class Storable a => AFType a where
  afType :: Proxy a -> AFDtype

instance AFType Double where
  afType Proxy = f64

instance AFType Float where
  afType Proxy = f32

instance AFType (Complex Double) where
  afType Proxy = c64

instance AFType (Complex Float) where
  afType Proxy = c32

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

instance AFType Word where
  afType Proxy = u64

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
toBackend (AFBackend x) = error $ "Invalid backend: " <> show x

toAFBackend :: Backend -> AFBackend
toAFBackend Default = (AFBackend 0)
toAFBackend CPU = (AFBackend 1)
toAFBackend CUDA = (AFBackend 2)
toAFBackend OpenCL = (AFBackend 4)

toBackends :: Int -> [Backend]
toBackends 1 = [CPU]
toBackends 2 = [CUDA]
toBackends 3 = [CPU,CUDA]
toBackends 4 = [OpenCL]
toBackends 5 = [CPU,OpenCL]
toBackends 6 = [CUDA,OpenCL]
toBackends 7 = [CPU,CUDA,OpenCL]
toBackends _ = []

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
toStorage = AFStorage . fromIntegral . fromEnum

fromStorage :: AFStorage -> Storage
fromStorage (AFStorage (fromIntegral -> x))
  | x `elem` [0..3] = toEnum x
  | otherwise = error $ "Invalid Storage " <> (show x)

data RandomEngineType
  = Philox
  | ThreeFry
  | Mersenne
  deriving (Eq, Show)

toRandomEngine :: AFRandomEngineType -> RandomEngineType
toRandomEngine (AFRandomEngineType 100) = Philox
toRandomEngine (AFRandomEngineType 200) = ThreeFry
toRandomEngine (AFRandomEngineType 300) = Mersenne
toRandomEngine (AFRandomEngineType x) =
  error ("Invalid random engine: " <> show x)

fromRandomEngine :: RandomEngineType ->  AFRandomEngineType
fromRandomEngine Philox = (AFRandomEngineType 100)
fromRandomEngine ThreeFry = (AFRandomEngineType 200)
fromRandomEngine Mersenne = (AFRandomEngineType 300)

data InterpType
  = Nearest
  | Linear
  | Bilinear
  | Cubic
  | LowerInterp
  | LinearCosine
  | BilinearCosine
  | Bicubic
  | CubicSpline
  | BicubicSpline
  deriving (Show, Eq, Ord, Enum)

toInterpType :: AFInterpType -> InterpType
toInterpType (AFInterpType (fromIntegral -> x)) = toEnum x

fromInterpType :: InterpType -> AFInterpType
fromInterpType = AFInterpType . fromIntegral . fromEnum

data BorderType
  = PadZero
  | PadSym
  deriving (Show, Ord, Enum, Eq)

toBorderType :: AFBorderType -> BorderType
toBorderType (AFBorderType (fromIntegral -> x)) = toEnum x

fromBorderType :: BorderType -> AFBorderType
fromBorderType = AFBorderType . fromIntegral . fromEnum

data Connectivity
  = Conn4
  | Conn8
  deriving (Show, Ord, Enum, Eq)

toConnectivity :: AFConnectivity -> Connectivity
toConnectivity (AFConnectivity 4) = Conn4
toConnectivity (AFConnectivity 8) = Conn4
toConnectivity (AFConnectivity x) = error ("Unknown connectivity option: " <> show x)

fromConnectivity :: Connectivity -> AFConnectivity
fromConnectivity Conn4 = AFConnectivity 4
fromConnectivity Conn8 = AFConnectivity 8

data CSpace
  = Gray
  | RGB
  | HSV
  | YCBCR
  deriving (Show, Eq, Ord, Enum)

toCSpace :: AFCSpace -> CSpace
toCSpace (AFCSpace (fromIntegral -> x)) = toEnum x

fromCSpace :: CSpace -> AFCSpace
fromCSpace = AFCSpace . fromIntegral . fromEnum

data YccStd
  = Ycc601
  | Ycc709
  | Ycc2020
  deriving (Show, Eq, Ord)

toAFYccStd :: AFYccStd -> YccStd
toAFYccStd (AFYccStd 601) = Ycc601
toAFYccStd (AFYccStd 709) = Ycc709
toAFYccStd (AFYccStd 2020) = Ycc2020
toAFYccStd (AFYccStd x) = error ("Unknown AFYccStd option: " <> show x)

fromAFYccStd :: YccStd -> AFYccStd
fromAFYccStd Ycc601 = afYcc601
fromAFYccStd Ycc709 = afYcc709
fromAFYccStd Ycc2020 = afYcc2020

data MomentType
  = M00
  | M01
  | M10
  | M11
  | FirstOrder
  deriving (Show, Eq, Ord)

toMomentType :: AFMomentType -> MomentType
toMomentType x
  | x == afMomentM00 = M00
  | x == afMomentM01 = M01
  | x == afMomentM10 = M10
  | x == afMomentM11 = M11
  | x == afMomentFirstOrder = FirstOrder
  | otherwise = error ("Unknown moment type: " <> show x)

fromMomentType :: MomentType -> AFMomentType
fromMomentType M00 = afMomentM00
fromMomentType M01 = afMomentM01
fromMomentType M10 = afMomentM10
fromMomentType M11 = afMomentM11
fromMomentType FirstOrder = afMomentFirstOrder

data CannyThreshold
  = Manual
  | AutoOtsu
  deriving (Show, Eq, Ord, Enum)

toCannyThreshold :: AFCannyThreshold -> CannyThreshold
toCannyThreshold (AFCannyThreshold (fromIntegral -> x)) = toEnum x

fromCannyThreshold :: CannyThreshold -> AFCannyThreshold
fromCannyThreshold = AFCannyThreshold . fromIntegral . fromEnum

data FluxFunction
  = FluxDefault
  | FluxQuadratic
  | FluxExponential
  deriving (Show, Eq, Ord, Enum)

toFluxFunction :: AFFluxFunction -> FluxFunction
toFluxFunction (AFFluxFunction (fromIntegral -> x)) = toEnum x

fromFluxFunction :: FluxFunction -> AFFluxFunction
fromFluxFunction = AFFluxFunction . fromIntegral . fromEnum

data DiffusionEq
  = DiffusionDefault
  | DiffusionGrad
  | DiffusionMCDE
  deriving (Show, Eq, Ord, Enum)

toDiffusionEq :: AFDiffusionEq -> DiffusionEq
toDiffusionEq (AFDiffusionEq (fromIntegral -> x)) = toEnum x

fromDiffusionEq :: DiffusionEq -> AFDiffusionEq
fromDiffusionEq = AFDiffusionEq . fromIntegral . fromEnum

data IterativeDeconvAlgo
  = DeconvDefault
  | DeconvLandweber
  | DeconvRichardsonLucy
  deriving (Show, Eq, Ord, Enum)

toIterativeDeconvAlgo :: AFIterativeDeconvAlgo -> IterativeDeconvAlgo
toIterativeDeconvAlgo (AFIterativeDeconvAlgo (fromIntegral -> x)) = toEnum x

fromIterativeDeconvAlgo :: IterativeDeconvAlgo -> AFIterativeDeconvAlgo
fromIterativeDeconvAlgo = AFIterativeDeconvAlgo . fromIntegral . fromEnum

data InverseDeconvAlgo
  = InverseDeconvDefault
  | InverseDeconvTikhonov
  deriving (Show, Eq, Ord, Enum)

toInverseDeconvAlgo :: AFInverseDeconvAlgo -> InverseDeconvAlgo
toInverseDeconvAlgo (AFInverseDeconvAlgo (fromIntegral -> x)) = toEnum x

fromInverseDeconvAlgo :: InverseDeconvAlgo -> AFInverseDeconvAlgo
fromInverseDeconvAlgo = AFInverseDeconvAlgo . fromIntegral . fromEnum

data Cell
  = Cell
  { cellRow :: Int
  , cellCol :: Int
  , cellTitle :: String
  , cellColorMap :: ColorMap
  } deriving (Show, Eq)

cellToAFCell :: Cell -> IO AFCell
cellToAFCell Cell {..} =
  withCString cellTitle $ \cstr ->
    pure AFCell { afCellRow = cellRow
                , afCellCol = cellCol
                , afCellTitle = cstr
                , afCellColorMap = fromColorMap cellColorMap
                }

data ColorMap
  = ColorMapDefault
  | ColorMapSpectrum
  | ColorMapColors
  | ColorMapRed
  | ColorMapMood
  | ColorMapHeat
  | ColorMapBlue
  | ColorMapInferno
  | ColorMapMagma
  | ColorMapPlasma
  | ColorMapViridis
  deriving (Show, Eq, Ord, Enum)

fromColorMap :: ColorMap -> AFColorMap
fromColorMap = AFColorMap . fromIntegral . fromEnum

toColorMap :: AFColorMap -> ColorMap
toColorMap (AFColorMap (fromIntegral -> x)) = toEnum x

data MarkerType
  = MarkerTypeNone
  | MarkerTypePoint
  | MarkerTypeCircle
  | MarkerTypeSquare
  | MarkerTypeTriangle
  | MarkerTypeCross
  | MarkerTypePlus
  | MarkerTypeStar
  deriving (Show, Eq, Ord, Enum)

fromMarkerType :: MarkerType -> AFMarkerType
fromMarkerType = AFMarkerType . fromIntegral . fromEnum

toMarkerType :: AFMarkerType -> MarkerType
toMarkerType (AFMarkerType (fromIntegral -> x)) = toEnum x

data MatchType
  = MatchTypeSAD
  | MatchTypeZSAD
  | MatchTypeLSAD
  | MatchTypeSSD
  | MatchTypeZSSD
  | MatchTypeLSSD
  | MatchTypeNCC
  | MatchTypeZNCC
  | MatchTypeSHD
  deriving (Show, Eq, Ord, Enum)

fromMatchType :: MatchType -> AFMatchType
fromMatchType = AFMatchType . fromIntegral . fromEnum

toMatchType :: AFMatchType -> MatchType
toMatchType (AFMatchType (fromIntegral -> x)) = toEnum x

data HomographyType
  = RANSAC
  | LMEDS
  deriving (Show, Eq, Ord, Enum)

fromHomographyType :: HomographyType -> AFHomographyType
fromHomographyType = AFHomographyType . fromIntegral . fromEnum

toHomographyType :: AFHomographyType -> HomographyType
toHomographyType (AFHomographyType (fromIntegral -> x)) = toEnum x

data Seq
  = Seq
  { seqBegin :: !Double
  , seqEnd :: !Double
  , seqStep :: !Double
  } deriving (Show, Eq, Ord)

toAFSeq :: Seq -> AFSeq
toAFSeq (Seq x y z) = (AFSeq x y z)

data Index a
  = Index
  { afIdx :: Either (Array a) Seq
  , afIsSeq :: !Bool
  , afIsBatch :: !Bool
  }

seqIdx :: Seq -> Bool -> Index a
seqIdx s = Index (Right s) True

arrIdx :: Array a -> Bool -> Index a
arrIdx a = Index (Left a) False

toAFIndex :: Index a -> IO AFIndex
toAFIndex (Index a b c) = do
  case a of
    Right s -> pure $ AFIndex (Right (toAFSeq s)) b c
    Left (Array fptr) -> do
      withForeignPtr fptr $ \ptr ->
        pure $ AFIndex (Left ptr) b c
