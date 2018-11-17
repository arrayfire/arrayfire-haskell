{-# LANGUAGE CPP #-}
module Data.Array.Fire.Internal.Defines where

import Foreign.ForeignPtr
import Foreign.Ptr

#include "fire.h"

afVersion = #const AF_API_VERSION

newtype AFDType = AFDType { afDType :: Int }
  deriving (Show, Eq)

#{enum AFDType, AFDType
 , f32 = f32
 , c32 = c32
 , f64 = f64
 , c64 = c64
 , b8 = b8
 , s32 = s32
 , u32 = u32
 , u8 = u8
 , s64 = s64
 , u64 = u64
 , s16 = s16
 , u16 = u16
 }

newtype AFError = AFError { afError :: Int }
  deriving (Show, Eq)

#{enum AFError, AFError
 , afSuccess = AF_SUCCESS
 , afErrNoMem = AF_ERR_NO_MEM
 , afErrDriver = AF_ERR_DRIVER
 , afErrRuntime = AF_ERR_RUNTIME
 , afErrInvalidArray = AF_ERR_INVALID_ARRAY
 , afErrArg = AF_ERR_ARG
 , afErrSize = AF_ERR_SIZE
 , afErrType = AF_ERR_TYPE
 , afErrDiffType = AF_ERR_DIFF_TYPE
 , afErrBatch = AF_ERR_BATCH
 , afErrDevice = AF_ERR_DEVICE
 , afErrNotSupported = AF_ERR_NOT_SUPPORTED
 , afErrNotConfigured = AF_ERR_NOT_CONFIGURED
 , afErrNonFree = AF_ERR_NONFREE
 , afErrNoDbl = AF_ERR_NO_DBL
 , afErrNoGfx = AF_ERR_NO_GFX
 , afErrLoadLib = AF_ERR_LOAD_LIB
 , afErrLoadSym = AF_ERR_LOAD_SYM
 , afErrArrBkndMismatch = AF_ERR_ARR_BKND_MISMATCH
 , afErrInternal = AF_ERR_INTERNAL
 , afErrUnknown = AF_ERR_UNKNOWN
 }

newtype AFSource = AFSource Int
  deriving (Ord, Show, Eq)

#{enum AFSource, AFSource
 , afDevice = afDevice
 , afHost = afHost
 }

afMaxDims = #const AF_MAX_DIMS

-- // A handle for an internal array object
newtype AFArray = AFArray (Ptr AFArray)

newtype AFInterpType = AFInterpType Int
  deriving (Ord, Show, Eq)

#{enum AFInterpType, AFInterpType
 , afInterpNearest = AF_INTERP_NEAREST
 , afInterpLinear = AF_INTERP_LINEAR
 , afInterpBilinear =  AF_INTERP_BILINEAR
 , afInterpCubic = AF_INTERP_CUBIC
 , afInterpLower = AF_INTERP_LOWER
 , afInterpLinearCosine =  AF_INTERP_LINEAR_COSINE
 , afInterpBilinearCosine = AF_INTERP_BILINEAR_COSINE
 , afInterpBicubic = AF_INTERP_BICUBIC
 , afInterpCubicSpline = AF_INTERP_CUBIC_SPLINE
 , afInterpBicubicSpline = AF_INTERP_BICUBIC_SPLINE
 }

newtype AFBorderType = AFBorderType Int
  deriving (Ord, Show, Eq)

#{enum AFBorderType, AFBorderType
 , afBorderPadZero = AF_PAD_ZERO
 , afPadSym = AF_PAD_SYM
 , afPadClampToEdge = AF_PAD_CLAMP_TO_EDGE
 }

newtype AFConnectivity = AFConnectivity Int
  deriving (Ord, Show, Eq)

#{enum AFConnectivity, AFConnectivity
 , afConnectivity4 = AF_CONNECTIVITY_4
 , afConnectivity8 = AF_CONNECTIVITY_8
 }

newtype AFConvMode = AFConvMode Int
  deriving (Ord, Show, Eq)

#{enum AFConvMode, AFConvMode
 , afConvDefault = AF_CONV_DEFAULT
 , afConvExpand = AF_CONV_EXPAND
 }

newtype AFConvDomain = AFConvDomain Int
  deriving (Ord, Show, Eq)

#{enum AFConvDomain, AFConvDomain
 , afConvAuto = AF_CONV_AUTO
 , afConvSpatial = AF_CONV_SPATIAL
 , afConvFreq = AF_CONV_FREQ
}

newtype AFMatchType = AFMatchType Int
  deriving (Ord, Show, Eq)

#{enum AFMatchType, AFMatchType
 , afSAD  = AF_SAD
 , afZSAD = AF_ZSAD
 , afLSAD = AF_LSAD
 , afSSD  = AF_SSD
 , afZSSD = AF_ZSSD
 , afLSSD = AF_LSSD
 , afNCC  = AF_NCC
 , afZNCC = AF_ZNCC
 , afSHD  = AF_SHD
}

newtype AFYCCSTD = AFYCCSTD Int
  deriving (Ord, Show, Eq)

#{enum AFYCCSTD, AFYCCSTD
 , afYcc601 = AF_YCC_601
 , afYcc709 = AF_YCC_709
 , afYcc2020 = AF_YCC_2020
 }

newtype AFCSpace = AFCSpace Int
  deriving (Ord, Show, Eq)

#{enum AFCSpace, AFCSpace
 , afGray = AF_GRAY
 , afRgb = AF_RGB
 , afHsv = AF_HSV
 , afYCbCr = AF_YCbCr
 }

newtype AFMatProp = AFMatProp Int
  deriving (Ord, Show, Eq)

#{enum AFMatProp, AFMatProp
 , afMatNone = AF_MAT_NONE
 , afMatTrans = AF_MAT_TRANS
 , afMatCtrans = AF_MAT_CTRANS
 , afMatConj = AF_MAT_CONJ
 , afMatUpper = AF_MAT_UPPER
 , afMatLower = AF_MAT_LOWER
 , afMatDiagUnit = AF_MAT_DIAG_UNIT
 , afMatSym = AF_MAT_SYM
 , afMatPosdef = AF_MAT_POSDEF
 , afMatOrthog = AF_MAT_ORTHOG
 , afMatTriDiag = AF_MAT_TRI_DIAG
 , afMatBlockDiag = AF_MAT_BLOCK_DIAG
 }

newtype AFNormType = AFNormType Int
  deriving (Ord, Show, Eq)

#{enum AFNormType, AFNormType
 , afNormVector1 = AF_NORM_VECTOR_1
 , afNormVectorInf = AF_NORM_VECTOR_INF
 , afNormVector2 = AF_NORM_VECTOR_2
 , afNormVectorP = AF_NORM_VECTOR_P
 , afNormMatrix1 = AF_NORM_MATRIX_1
 , afNormMatrixInf = AF_NORM_MATRIX_INF
 , afNormMatrix2 = AF_NORM_MATRIX_2
 , afNormMatrixLPq = AF_NORM_MATRIX_L_PQ
 , afNormEuclid = AF_NORM_VECTOR_2
}

newtype AFImageFormat = AFImageFormat Int
  deriving (Ord, Show, Eq)

#{enum AFImageFormat, AFImageFormat
 , afFIFBmp = AF_FIF_BMP
 , afFIFIco = AF_FIF_ICO
 , afFIFJpeg = AF_FIF_JPEG
 , afFIFJng = AF_FIF_JNG
 , afFIFPng = AF_FIF_PNG
 , afFIFPpm =  AF_FIF_PPM
 , afFIFPpmraw = AF_FIF_PPMRAW
 , afFIFTiff = AF_FIF_TIFF
 , afFIFPsd = AF_FIF_PSD
 , afFIFHdr = AF_FIF_HDR
 , afFIFExr = AF_FIF_EXR
 , afFIFJp2 = AF_FIF_JP2
 , afFIFRaw = AF_FIF_RAW
 }

newtype AFMomentType = AFMomentType Int
  deriving (Ord, Show, Eq)

#{enum AFMomentType, AFMomentType
 , afMomentM00 = AF_MOMENT_M00
 , afMomentM01 = AF_MOMENT_M01
 , afMomentM10 = AF_MOMENT_M10
 , afMomentM11 = AF_MOMENT_M11
 , afMomentFirstOrder = (AF_MOMENT_M00 | AF_MOMENT_M01 | AF_MOMENT_M10 | AF_MOMENT_M11)
}

newtype AFHomographyType = AFHomographyType Int
  deriving (Ord, Show, Eq)

#{enum AFHomographyType, AFHomographyType
 , afHomographyRansac = AF_HOMOGRAPHY_RANSAC
 , afHomographyLmeds  = AF_HOMOGRAPHY_LMEDS
}

newtype AFBackend = AFBackend Int
  deriving (Ord, Show, Eq)

#{enum AFBackend, AFBackend
 , afBackendDefault = AF_BACKEND_DEFAULT
 , afBackendCpu     = AF_BACKEND_DEFAULT
 , afBackendCuda    = AF_BACKEND_CUDA
 , afBackendOpencl  = AF_BACKEND_OPENCL
}

newtype AFID = AFID Int
  deriving (Ord, Show, Eq)

#{enum AFID, AFID
  afID = AF_ID
}

newtype AFBinaryOP = AFBinaryOP Int
  deriving (Ord, Show, Eq)

#{enum AFBinaryOP, AFBinaryOP
 , afBinaryAdd  = AF_BINARY_ADD
 , afBinaryMul  = AF_BINARY_MUL
 , afBinaryMin  = AF_BINARY_MIN
 , afBinaryMax  = AF_BINARY_MAX
 }

newtype AFRandomEngineType = AFRandomEngineType Int
  deriving (Ord, Show, Eq)

#{enum AFRandomEngineType, AFRandomEngineType
 , afRandomEnginePhilox4X3210 = AF_RANDOM_ENGINE_PHILOX_4X32_10
 , afRandomEngineThreefry2X3216 = AF_RANDOM_ENGINE_THREEFRY_2X32_16
 , afRandomEngineMersenneGp11213 = AF_RANDOM_ENGINE_MERSENNE_GP11213
 , afRandomEnginePhilox  = AF_RANDOM_ENGINE_PHILOX_4X32_10
 , afRandomEngineThreefry = AF_RANDOM_ENGINE_THREEFRY_2X32_16
 , afRandomEngineMersenne = AF_RANDOM_ENGINE_MERSENNE_GP11213
 , afRandomEngineDefault = AF_RANDOM_ENGINE_PHILOX
 }

newtype AFColorMap = AFColorMap Int
  deriving (Ord, Show, Eq)

#{enum AFColorMap, AFColorMap
 , afColormapDefault = AF_COLORMAP_DEFAULT
 , afColormapSpectrum= AF_COLORMAP_SPECTRUM
 , afColormapColors  = AF_COLORMAP_COLORS
 , afColormapRed     = AF_COLORMAP_RED
 , afColormapMood    = AF_COLORMAP_MOOD
 , afColormapHeat    = AF_COLORMAP_HEAT
 , afColormapBlue    = AF_COLORMAP_BLUE
 , afColormapInferno = AF_COLORMAP_INFERNO
 , afColormapMagma   = AF_COLORMAP_MAGMA
 , afColormapPlasma  = AF_COLORMAP_PLASMA
 , afColormapViridis = AF_COLORMAP_VIRIDIS
}

newtype AFMarkerType = AFMarkerType Int
  deriving (Ord, Show, Eq)

#{enum AFMarkerType, AFMarkerType
 , afMarkerNone     = AF_MARKER_NONE
 , afMarkerPoint    = AF_MARKER_POINT
 , afMarkerCircle   = AF_MARKER_CIRCLE
 , afMarkerSquare   = AF_MARKER_SQUARE
 , afMarkerTriangle = AF_MARKER_TRIANGLE
 , afMarkerCross    = AF_MARKER_CROSS
 , afMarkerPlus     = AF_MARKER_PLUS
 , afMarkerStar     = AF_MARKER_STAR
 }

newtype AFCannyThreshold = AFCannyThreshold Int
  deriving (Ord, Show, Eq)

#{enum AFCannyThreshold, AFCannyThreshold
 , afCannyThresholdManual = AF_CANNY_THRESHOLD_MANUAL
 , afCannyThresholdAutoOtsu = AF_CANNY_THRESHOLD_AUTO_OTSU
 }

newtype AFStorage = AFStorage Int
  deriving (Ord, Show, Eq)

#{enum AFStorage, AFStorage
 , afStorageDense = AF_STORAGE_DENSE
 , afStorageCsr = AF_STORAGE_CSR
 , afStorageCsc = AF_STORAGE_CSC
 , afStorageCoo = AF_STORAGE_COO
 }

newtype AFFluxFunction = AFFluxFunction Int
  deriving (Ord, Show, Eq)

#{enum AFFluxFunction, AFFluxFunction
 , afFluxQuadratic = AF_FLUX_QUADRATIC
 , afFluxExponential = AF_FLUX_EXPONENTIAL
 , afFluxDefault = AF_FLUX_DEFAULT
 }

newtype AFDiffusionEq = AFDiffusionEq Int
  deriving (Ord, Show, Eq)

#{enum AFDiffusionEq, AFDiffusionEq
 , afDiffusionGrad = AF_DIFFUSION_GRAD
 , afDiffusionMcde = AF_DIFFUSION_MCDE
 , afDiffusionDefault = AF_DIFFUSION_DEFAULT
 }

newtype AFTopKFunction = AFTopKFunction Int
  deriving (Ord, Show, Eq)

#{enum AFTopKFunction, AFTopKFunction
 , afTopkMin = AF_TOPK_MIN
 , afTopkMax = AF_TOPK_MAX
 , afTopkDefault = AF_TOPK_DEFAULT
 }

newtype AFIterativeDeconvAlgo = AFIterativeDeconvAlgo Int
  deriving (Ord, Show, Eq)

#{enum AFIterativeDeconvAlgo, AFIterativeDeconvAlgo
 , afIterativeDeconvLandweber       = AF_ITERATIVE_DECONV_LANDWEBER
 , afIterativeDeconvRichardsonlucy  = AF_ITERATIVE_DECONV_RICHARDSONLUCY
 , afIterativeDeconvDefault         = AF_ITERATIVE_DECONV_DEFAULT
 }

newtype AFInverseDeconvAlgo = AFInverseDeconvAlgo Int
  deriving (Ord, Show, Eq)

#{enum AFInverseDeconvAlgo, AFInverseDeconvAlgo
  afInverseDeconvTikhonov = AF_INVERSE_DECONV_TIKHONOV
  afInverseDeconvDefault = AF_INVERSE_DECONV_DEFAULT
 }

newtype AFVarBias = AFVarBias Int
  deriving (Ord, Show, Eq)

#{enum AFVarBias, AFVarBias
 , afVarianceDefault = AF_VARIANCE_DEFAULT
 , afVarianceSample = AF_VARIANCE_SAMPLE
 , afVariancePopulation = AF_VARIANCE_POPULATION
}
