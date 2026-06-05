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
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
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
        then Right <$> #{peek af_index_t, idx.seq} ptr
        else Left <$> #{peek af_index_t, idx.arr} ptr
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
  -- ^ Use the default backend (determined by ArrayFire)
  | CPU
  -- ^ CPU backend (always available)
  | CUDA
  -- ^ NVIDIA CUDA GPU backend
  | OpenCL
  -- ^ OpenCL backend (AMD, Intel, NVIDIA)
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
  -- ^ No property
  | Trans
  -- ^ Data needs to be transposed
  | CTrans
  -- ^ Data needs to be conjugate transposed
  | Conj
  -- ^ Data needs to be conjugated
  | Upper
  -- ^ Matrix is upper triangular
  | Lower
  -- ^ Matrix is lower triangular
  | DiagUnit
  -- ^ Diagonal contains units; used with triangular solvers
  | Sym
  -- ^ Matrix is symmetric
  | PosDef
  -- ^ Matrix is positive definite
  | Orthog
  -- ^ Matrix is orthogonal
  | TriDiag
  -- ^ Matrix is tri-diagonal
  | BlockDiag
  -- ^ Matrix is block diagonal
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

-- | Binary operation support (used with scan-by-key and similar operations)
data BinaryOp
  = Add
  -- ^ Addition
  | Mul
  -- ^ Multiplication
  | Min
  -- ^ Minimum
  | Max
  -- ^ Maximum
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
  -- ^ Dense storage (not sparse)
  | CSR
  -- ^ Compressed Sparse Row format
  | CSC
  -- ^ Compressed Sparse Column format
  | COO
  -- ^ Coordinate list (COO) format
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
  -- ^ Nearest-neighbor interpolation
  | Linear
  -- ^ Linear interpolation
  | Bilinear
  -- ^ Bilinear interpolation
  | Cubic
  -- ^ Cubic interpolation
  | LowerInterp
  -- ^ Floor interpolation (rounds down to nearest integer)
  | LinearCosine
  -- ^ Cosine-windowed linear interpolation
  | BilinearCosine
  -- ^ Cosine-windowed bilinear interpolation
  | Bicubic
  -- ^ Bicubic interpolation
  | CubicSpline
  -- ^ Cubic spline interpolation
  | BicubicSpline
  -- ^ Bicubic spline interpolation
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
toConnectivity (AFConnectivity 8) = Conn8
toConnectivity (AFConnectivity x) = error ("Unknown connectivity option: " <> show x)

fromConnectivity :: Connectivity -> AFConnectivity
fromConnectivity Conn4 = AFConnectivity 4
fromConnectivity Conn8 = AFConnectivity 8

-- | Color Space type
data CSpace
  = Gray
  -- ^ Grayscale
  | RGB
  -- ^ Red-Green-Blue
  | HSV
  -- ^ Hue-Saturation-Value
  | YCBCR
  -- ^ Luminance + chroma (blue-difference, red-difference)
  deriving (Show, Eq, Ord, Enum)

toCSpace :: AFCSpace -> CSpace
toCSpace (AFCSpace (fromIntegral -> x)) = toEnum x

fromCSpace :: CSpace -> AFCSpace
fromCSpace = AFCSpace . fromIntegral . fromEnum

-- | YCbCr standard
data YccStd
  = Ycc601
  -- ^ ITU-R BT.601 (standard definition)
  | Ycc709
  -- ^ ITU-R BT.709 (high definition)
  | Ycc2020
  -- ^ ITU-R BT.2020 (ultra high definition)
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

-- | Image moment types
data MomentType
  = M00
  -- ^ Zeroth-order moment (image area / mass)
  | M01
  -- ^ First-order moment about x-axis
  | M10
  -- ^ First-order moment about y-axis
  | M11
  -- ^ Mixed first-order moment
  | FirstOrder
  -- ^ All first-order moments (M00, M01, M10, M11)
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

-- | Threshold mode for Canny edge detection
data CannyThreshold
  = Manual
  -- ^ User-supplied low and high threshold values
  | AutoOtsu
  -- ^ Thresholds computed automatically via Otsu's method
  deriving (Show, Eq, Ord, Enum)

toCannyThreshold :: AFCannyThreshold -> CannyThreshold
toCannyThreshold (AFCannyThreshold (fromIntegral -> x)) = toEnum x

fromCannyThreshold :: CannyThreshold -> AFCannyThreshold
fromCannyThreshold = AFCannyThreshold . fromIntegral . fromEnum

-- | Flux function for anisotropic diffusion
data FluxFunction
  = FluxDefault
  -- ^ Default flux function (same as 'FluxQuadratic')
  | FluxQuadratic
  -- ^ Quadratic flux function (Perona-Malik)
  | FluxExponential
  -- ^ Exponential flux function (Perona-Malik)
  deriving (Show, Eq, Ord, Enum)

toFluxFunction :: AFFluxFunction -> FluxFunction
toFluxFunction (AFFluxFunction (fromIntegral -> x)) = toEnum x

fromFluxFunction :: FluxFunction -> AFFluxFunction
fromFluxFunction = AFFluxFunction . fromIntegral . fromEnum

-- | Diffusion equation type for anisotropic smoothing
data DiffusionEq
  = DiffusionDefault
  -- ^ Default (same as 'DiffusionGrad')
  | DiffusionGrad
  -- ^ Gradient-based diffusion (Perona-Malik)
  | DiffusionMCDE
  -- ^ Mean curvature diffusion equation
  deriving (Show, Eq, Ord, Enum)

toDiffusionEq :: AFDiffusionEq -> DiffusionEq
toDiffusionEq (AFDiffusionEq (fromIntegral -> x)) = toEnum x

fromDiffusionEq :: DiffusionEq -> AFDiffusionEq
fromDiffusionEq = AFDiffusionEq . fromIntegral . fromEnum

-- | Iterative deconvolution algorithm
data IterativeDeconvAlgo
  = DeconvDefault
  -- ^ Default algorithm (same as 'DeconvLandweber')
  | DeconvLandweber
  -- ^ Landweber iteration (gradient descent on least squares)
  | DeconvRichardsonLucy
  -- ^ Richardson-Lucy algorithm (maximum likelihood for Poisson noise)
  deriving (Show, Eq, Ord, Enum)

toIterativeDeconvAlgo :: AFIterativeDeconvAlgo -> IterativeDeconvAlgo
toIterativeDeconvAlgo (AFIterativeDeconvAlgo (fromIntegral -> x)) = toEnum x

fromIterativeDeconvAlgo :: IterativeDeconvAlgo -> AFIterativeDeconvAlgo
fromIterativeDeconvAlgo = AFIterativeDeconvAlgo . fromIntegral . fromEnum

-- | Inverse (non-iterative) deconvolution algorithm
data InverseDeconvAlgo
  = InverseDeconvDefault
  -- ^ Default algorithm (same as 'InverseDeconvTikhonov')
  | InverseDeconvTikhonov
  -- ^ Tikhonov regularized Wiener filter
  deriving (Show, Eq, Ord, Enum)

toInverseDeconvAlgo :: AFInverseDeconvAlgo -> InverseDeconvAlgo
toInverseDeconvAlgo (AFInverseDeconvAlgo (fromIntegral -> x)) = toEnum x

fromInverseDeconvAlgo :: InverseDeconvAlgo -> AFInverseDeconvAlgo
fromInverseDeconvAlgo = AFInverseDeconvAlgo . fromIntegral . fromEnum

-- | Cell type, used in Graphics module to describe a subplot position
data Cell
  = Cell
  { cellRow :: Int
  -- ^ Row index of the subplot (0-based)
  , cellCol :: Int
  -- ^ Column index of the subplot (0-based)
  , cellTitle :: String
  -- ^ Title string displayed above the plot
  , cellColorMap :: ColorMap
  -- ^ Color map used for rendering
  } deriving (Show, Eq)

cellToAFCell :: Cell -> IO AFCell
cellToAFCell Cell {..} =
  withCString cellTitle $ \cstr ->
    pure AFCell { afCellRow = cellRow
                , afCellCol = cellCol
                , afCellTitle = cstr
                , afCellColorMap = fromColorMap cellColorMap
                }

-- | Color map for rendering
data ColorMap
  = ColorMapDefault
  -- ^ Default grayscale color map
  | ColorMapSpectrum
  -- ^ Rainbow spectrum (violet to red)
  | ColorMapColors
  -- ^ Distinct colors
  | ColorMapRed
  -- ^ Red gradient
  | ColorMapMood
  -- ^ Mood color map (cool tones)
  | ColorMapHeat
  -- ^ Heat map (black to red to yellow to white)
  | ColorMapBlue
  -- ^ Blue gradient
  | ColorMapInferno
  -- ^ Perceptually uniform: black-purple-orange-yellow
  | ColorMapMagma
  -- ^ Perceptually uniform: black-purple-pink-white
  | ColorMapPlasma
  -- ^ Perceptually uniform: blue-purple-yellow
  | ColorMapViridis
  -- ^ Perceptually uniform: purple-teal-yellow
  deriving (Show, Eq, Ord, Enum)

fromColorMap :: ColorMap -> AFColorMap
fromColorMap = AFColorMap . fromIntegral . fromEnum

toColorMap :: AFColorMap -> ColorMap
toColorMap (AFColorMap (fromIntegral -> x)) = toEnum x

-- | Marker shape for scatter plots
data MarkerType
  = MarkerTypeNone
  -- ^ No marker
  | MarkerTypePoint
  -- ^ Single pixel point
  | MarkerTypeCircle
  -- ^ Circle
  | MarkerTypeSquare
  -- ^ Square
  | MarkerTypeTriangle
  -- ^ Triangle
  | MarkerTypeCross
  -- ^ X cross
  | MarkerTypePlus
  -- ^ Plus sign
  | MarkerTypeStar
  -- ^ Star
  deriving (Show, Eq, Ord, Enum)

fromMarkerType :: MarkerType -> AFMarkerType
fromMarkerType = AFMarkerType . fromIntegral . fromEnum

toMarkerType :: AFMarkerType -> MarkerType
toMarkerType (AFMarkerType (fromIntegral -> x)) = toEnum x

-- | Template matching metric type
data MatchType
  = MatchTypeSAD
  -- ^ Sum of Absolute Differences
  | MatchTypeZSAD
  -- ^ Zero-mean Sum of Absolute Differences
  | MatchTypeLSAD
  -- ^ Locally scaled Sum of Absolute Differences
  | MatchTypeSSD
  -- ^ Sum of Squared Differences
  | MatchTypeZSSD
  -- ^ Zero-mean Sum of Squared Differences
  | MatchTypeLSSD
  -- ^ Locally scaled Sum of Squared Differences
  | MatchTypeNCC
  -- ^ Normalized Cross Correlation
  | MatchTypeZNCC
  -- ^ Zero-mean Normalized Cross Correlation
  | MatchTypeSHD
  -- ^ Sum of Hamming Distances
  deriving (Show, Eq, Ord, Enum)

fromMatchType :: MatchType -> AFMatchType
fromMatchType = AFMatchType . fromIntegral . fromEnum

toMatchType :: AFMatchType -> MatchType
toMatchType (AFMatchType (fromIntegral -> x)) = toEnum x

-- | Order for @topk@ results
data TopK
  = TopKDefault
  -- ^ Default order (same as 'TopKMax')
  | TopKMin
  -- ^ Return the k smallest values
  | TopKMax
  -- ^ Return the k largest values
  deriving (Show, Eq, Ord, Enum)

fromTopK :: TopK -> AFTopkFunction
fromTopK = AFTopkFunction . fromIntegral . fromEnum

toTopK :: AFTopkFunction -> TopK
toTopK (AFTopkFunction (fromIntegral -> x)) = toEnum x

-- | Variance bias correction method
data VarBias
  = VarianceDefault
  -- ^ Default (same as 'VariancePopulation')
  | VarianceSample
  -- ^ Sample variance (divides by N-1; Bessel's correction)
  | VariancePopulation
  -- ^ Population variance (divides by N)
  deriving (Show, Eq, Ord, Enum)

fromVarBias :: VarBias -> AFVarBias
fromVarBias = AFVarBias . fromIntegral . fromEnum

-- | Homography estimation method
data HomographyType
  = RANSAC
  -- ^ Random Sample Consensus — robust to outliers
  | LMEDS
  -- ^ Least Median of Squares — robust to up to 50% outliers
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
data Index
  = SeqIndex Bool Seq
  | ArrIndex Bool (Array Int)

seqIdx :: Seq -> Bool -> Index
seqIdx s batch = SeqIndex batch s

arrIdx :: Array Int -> Bool -> Index
arrIdx a batch = ArrIndex batch a

toAFIndex :: Index -> IO AFIndex
toAFIndex (SeqIndex batch s) =
  pure $ AFIndex (Right (toAFSeq s)) True batch
toAFIndex (ArrIndex batch (Array fptr)) =
  pure $ AFIndex (Left (unsafeForeignPtrToPtr fptr)) False batch


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

-- | ArrayFire element types (mirrors @af_dtype@)
data AFDType
  = F32
  -- ^ 32-bit IEEE 754 float
  | C32
  -- ^ Complex number of two 32-bit floats
  | F64
  -- ^ 64-bit IEEE 754 double
  | C64
  -- ^ Complex number of two 64-bit doubles
  | B8
  -- ^ 8-bit boolean
  | S32
  -- ^ 32-bit signed integer
  | U32
  -- ^ 32-bit unsigned integer
  | U8
  -- ^ 8-bit unsigned integer
  | S64
  -- ^ 64-bit signed integer
  | U64
  -- ^ 64-bit unsigned integer
  | S16
  -- ^ 16-bit signed integer
  | U16
  -- ^ 16-bit unsigned integer
  deriving (Show, Eq, Enum)

fromAFType :: AFDtype -> AFDType
fromAFType (AFDtype (fromIntegral -> x)) = toEnum x

toAFType :: AFDType -> AFDtype
toAFType = AFDtype . fromIntegral . fromEnum

