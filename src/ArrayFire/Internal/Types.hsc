{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE CPP               #-}
module ArrayFire.Internal.Types where

#include "af/seq.h"
#include "af/complex.h"
#include "af/graphics.h"
#include "af/index.h"

import ArrayFire.Internal.Defines

import Data.Complex
import Data.Proxy
import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Storable
import GHC.Int

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

-- | Type used for indexing into 'Array'
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

-- | ArrayFire 'Array'
newtype Array a = Array (ForeignPtr ())

-- | ArrayFire 'Features'
newtype Features = Features (ForeignPtr ())

-- | ArrayFire 'RandomEngine'
newtype RandomEngine = RandomEngine (ForeignPtr ())

-- | ArrayFire 'Window'
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

-- | ArrayFire backends
data Backend
  = Default
  | CPU
  | CUDA
  | OpenCL
  deriving (Show, Eq, Ord)

-- | Low-level to high-level Backend conversion
toBackend :: AFBackend -> Backend
toBackend (AFBackend 0) = Default
toBackend (AFBackend 1) = CPU
toBackend (AFBackend 2) = CUDA
toBackend (AFBackend 4) = OpenCL
toBackend (AFBackend x) = error $ "Invalid backend: " <> show x

-- | High-level to low-level Backend conversion
toAFBackend :: Backend -> AFBackend
toAFBackend Default = (AFBackend 0)
toAFBackend CPU = (AFBackend 1)
toAFBackend CUDA = (AFBackend 2)
toAFBackend OpenCL = (AFBackend 4)

-- | Read multiple backends
toBackends :: Int -> [Backend]
toBackends 1 = [CPU]
toBackends 2 = [CUDA]
toBackends 3 = [CPU,CUDA]
toBackends 4 = [OpenCL]
toBackends 5 = [CPU,OpenCL]
toBackends 6 = [CUDA,OpenCL]
toBackends 7 = [CPU,CUDA,OpenCL]
toBackends _ = []

-- | Matrix properties
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

-- | Low-level to High-level 'MatProp' conversion
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

-- | High-level to low-level 'MatProp' conversion
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

-- | Binary operation support
data BinaryOp
  = Add
  | Mul
  | Min
  | Max
  deriving (Show, Eq, Ord)

-- | High-level to low-level 'MatProp' conversion
toBinaryOp :: BinaryOp -> AFBinaryOp
toBinaryOp Add = AFBinaryOp 0
toBinaryOp Mul = AFBinaryOp 1
toBinaryOp Min = AFBinaryOp 2
toBinaryOp Max = AFBinaryOp 3

-- | 'BinaryOp' conversion helper
fromBinaryOp :: AFBinaryOp -> BinaryOp
fromBinaryOp (AFBinaryOp 0) = Add
fromBinaryOp (AFBinaryOp 1) = Mul
fromBinaryOp (AFBinaryOp 2) = Min
fromBinaryOp (AFBinaryOp 3) = Max
fromBinaryOp x = error ("Invalid Binary Op: " <> show x)

-- | Storage type used for Sparse arrays
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

-- | Type for different RandomEngines
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

-- | Interpolation type
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

-- | Border Type
data BorderType
  = PadZero
  | PadSym
  deriving (Show, Ord, Enum, Eq)

toBorderType :: AFBorderType -> BorderType
toBorderType (AFBorderType (fromIntegral -> x)) = toEnum x

fromBorderType :: BorderType -> AFBorderType
fromBorderType = AFBorderType . fromIntegral . fromEnum

-- | Connectivity Type
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

-- | Color Space type
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

-- | YccStd type
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

-- | Moment types
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

-- | Canny Theshold type
data CannyThreshold
  = Manual
  | AutoOtsu
  deriving (Show, Eq, Ord, Enum)

toCannyThreshold :: AFCannyThreshold -> CannyThreshold
toCannyThreshold (AFCannyThreshold (fromIntegral -> x)) = toEnum x

fromCannyThreshold :: CannyThreshold -> AFCannyThreshold
fromCannyThreshold = AFCannyThreshold . fromIntegral . fromEnum

-- | Flux function type
data FluxFunction
  = FluxDefault
  | FluxQuadratic
  | FluxExponential
  deriving (Show, Eq, Ord, Enum)

toFluxFunction :: AFFluxFunction -> FluxFunction
toFluxFunction (AFFluxFunction (fromIntegral -> x)) = toEnum x

fromFluxFunction :: FluxFunction -> AFFluxFunction
fromFluxFunction = AFFluxFunction . fromIntegral . fromEnum

-- | Diffusion type
data DiffusionEq
  = DiffusionDefault
  | DiffusionGrad
  | DiffusionMCDE
  deriving (Show, Eq, Ord, Enum)

toDiffusionEq :: AFDiffusionEq -> DiffusionEq
toDiffusionEq (AFDiffusionEq (fromIntegral -> x)) = toEnum x

fromDiffusionEq :: DiffusionEq -> AFDiffusionEq
fromDiffusionEq = AFDiffusionEq . fromIntegral . fromEnum

-- | Iterative deconvolution algo type
data IterativeDeconvAlgo
  = DeconvDefault
  | DeconvLandweber
  | DeconvRichardsonLucy
  deriving (Show, Eq, Ord, Enum)

toIterativeDeconvAlgo :: AFIterativeDeconvAlgo -> IterativeDeconvAlgo
toIterativeDeconvAlgo (AFIterativeDeconvAlgo (fromIntegral -> x)) = toEnum x

fromIterativeDeconvAlgo :: IterativeDeconvAlgo -> AFIterativeDeconvAlgo
fromIterativeDeconvAlgo = AFIterativeDeconvAlgo . fromIntegral . fromEnum

-- | Inverse deconvolution algo type
data InverseDeconvAlgo
  = InverseDeconvDefault
  | InverseDeconvTikhonov
  deriving (Show, Eq, Ord, Enum)

toInverseDeconvAlgo :: AFInverseDeconvAlgo -> InverseDeconvAlgo
toInverseDeconvAlgo (AFInverseDeconvAlgo (fromIntegral -> x)) = toEnum x

fromInverseDeconvAlgo :: InverseDeconvAlgo -> AFInverseDeconvAlgo
fromInverseDeconvAlgo = AFInverseDeconvAlgo . fromIntegral . fromEnum

-- | Cell type, used in Graphics module
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

-- | ColorMap type
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

-- | Marker type
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

-- | Match type
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

-- | TopK type
data TopK
  = TopKDefault
  | TopKMin
  | TopKMax
  deriving (Show, Eq, Ord, Enum)

fromTopK :: TopK -> AFTopkFunction
fromTopK = AFTopkFunction . fromIntegral . fromEnum

toTopK :: AFTopkFunction -> TopK
toTopK (AFTopkFunction (fromIntegral -> x)) = toEnum x

-- | Homography Type
data HomographyType
  = RANSAC
  | LMEDS
  deriving (Show, Eq, Ord, Enum)

fromHomographyType :: HomographyType -> AFHomographyType
fromHomographyType = AFHomographyType . fromIntegral . fromEnum

toHomographyType :: AFHomographyType -> HomographyType
toHomographyType (AFHomographyType (fromIntegral -> x)) = toEnum x

-- | Sequence Type
data Seq
  = Seq
  { seqBegin :: !Double
  , seqEnd :: !Double
  , seqStep :: !Double
  } deriving (Show, Eq, Ord)

toAFSeq :: Seq -> AFSeq
toAFSeq (Seq x y z) = (AFSeq x y z)

-- | Index Type
data Index a
  = Index
  { idx :: Either (Array a) Seq
  , isSeq :: !Bool
  , isBatch :: !Bool
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


-- | Type alias for ArrayFire API version
type Version = (Int,Int,Int)

-- | Norm Type
data NormType
  = NormVectorOne
  -- ^ treats the input as a vector and returns the sum of absolute values
  | NormVectorInf
  -- ^ treats the input as a vector and returns the max of absolute values
  | NormVector2
  -- ^ treats the input as a vector and returns euclidean norm
  | NormVectorP
  -- ^ treats the input as a vector and returns the p-norm
  | NormMatrix1
  -- ^ return the max of column sums
  | NormMatrixInf
  -- ^ return the max of row sums
  | NormMatrix2
  -- ^ returns the max singular value). Currently NOT SUPPORTED
  | NormMatrixLPQ
  -- ^ returns Lpq-norm
  | NormEuclid
  -- ^ The default. Same as AF_NORM_VECTOR_2
  deriving (Show, Eq, Enum)

fromNormType :: NormType -> AFNormType
fromNormType = AFNormType . fromIntegral . fromEnum

toNormType :: AFNormType -> NormType
toNormType (AFNormType (fromIntegral -> x)) = toEnum x

-- | Convolution Domain
data ConvDomain
  = ConvDomainAuto
  -- ^ ArrayFire automatically picks the right convolution algorithm
  | ConvDomainSpatial
  -- ^ Perform convolution in spatial domain
  | ConvDomainFreq
  -- ^ Perform convolution in frequency domain
  deriving (Show, Eq, Enum)

-- | Convolution Mode
data ConvMode
  = ConvDefault
  -- ^ Output of the convolution is the same size as input
  | ConvExpand
  -- ^ Output of the convolution is signal_len + filter_len - 1
  deriving (Show, Eq, Enum)

fromConvDomain :: ConvDomain -> AFConvDomain
fromConvDomain = AFConvDomain . fromIntegral . fromEnum

toConvDomain :: AFConvDomain -> ConvDomain
toConvDomain (AFConvDomain (fromIntegral -> x)) = toEnum x

fromConvMode :: AFConvMode -> ConvMode
fromConvMode (AFConvMode (fromIntegral -> x)) = toEnum x

toConvMode :: ConvMode -> AFConvMode
toConvMode = AFConvMode . fromIntegral . fromEnum

-- | Array Fire types
data AFDType
  = F32
  | C32
  | F64
  | C64
  | B8
  | S32
  | U32
  | U8
  | S64
  | U64
  | S16
  | U16
  deriving (Show, Eq, Enum)

fromAFType :: AFDtype -> AFDType
fromAFType (AFDtype (fromIntegral -> x)) = toEnum x

toAFType :: AFDType -> AFDtype
toAFType = AFDtype . fromIntegral . fromEnum

