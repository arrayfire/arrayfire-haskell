{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE ViewPatterns         #-}
{-# LANGUAGE KindSignatures       #-}
module ArrayFire.Array where

import Control.Exception
import Data.Proxy
import Data.Vector.Storable       hiding (mapM_)
import Foreign.ForeignPtr
import Foreign.Marshal            hiding (void)
import Foreign.Ptr
import Foreign.Storable

import System.IO.Unsafe

import ArrayFire.Exception
import ArrayFire.FFI
import ArrayFire.Internal.Array
import ArrayFire.Internal.Defines
import ArrayFire.Types

scalar :: AFType a => a -> Array a
scalar x = mkArray [1] [x]

vector :: forall a . AFType a => Int -> [a] -> Array a
vector n = mkArray [n]

matrix :: forall a . AFType a => (Int,Int) -> [a] -> Array a
matrix (x,y) = mkArray [x,y]

cube :: AFType a => (Int,Int,Int) -> [a] -> Array a
cube (x,y,z) = mkArray [x,y,z]

mkArray
  :: forall array
   . AFType array
  => [Int]
  -> [array]
  -> Array array
mkArray dims xs =
  unsafePerformIO . mask_ $ do
    dataPtr <- castPtr <$> newArray (Prelude.take size xs)
    let ndims = fromIntegral (Prelude.length dims)
    alloca $ \arrayPtr -> do
      dimsPtr <- newArray (DimT . fromIntegral <$> dims)
      throwAFError =<< af_create_array arrayPtr dataPtr ndims dimsPtr dType
      free dataPtr >> free dimsPtr
      arr <- peek arrayPtr
      Array <$> newForeignPtr af_release_array_finalizer arr
    where
      size  = Prelude.product (fromIntegral <$> dims)
      dType = afType (Proxy @ array)

-- af_err af_create_handle(af_array *arr, const unsigned ndims, const dim_t * const dims, const af_dtype type);
copyArray
  :: AFType (a :: *)
  => Array a
  -> Array a
copyArray = (`op1` af_copy_array)
-- af_err af_write_array(af_array arr, const void *data, const size_t bytes, af_source src);
-- af_err af_get_data_ptr(void *data, const af_array arr);
-- af_err af_release_array(af_array arr);
retainArray
  :: AFType (a :: *)
  => Array a
  -> Array a
retainArray =
  (`op1` af_retain_array)

getDataRefCount
  :: AFType (a :: *)
  => Array a
  -> Int
getDataRefCount =
  (`infoFromArray` af_get_data_ref_count)

-- af_err af_eval(af_array in);
-- af_err af_eval_multiple(const int num, af_array *arrays);

setManualEvalFlag
  :: Bool -> IO ()
setManualEvalFlag =
  afCall . af_set_manual_eval_flag

getManualEvalFlag
  :: IO Bool
getManualEvalFlag =
  afCall1 af_get_manual_eval_flag

getElements
  :: AFType (a :: *)
  => Array a
  -> Int
getElements a =
  fromIntegral (a `infoFromArray` af_get_elements)

getType
  :: AFType (a :: *)
  => Array a
  -> AFDtype
getType = (`infoFromArray` af_get_type)

getDims
  :: AFType (a :: *)
  => Array a
  -> (Int,Int,Int,Int)
getDims arr = do
  let (a,b,c,d) = arr `infoFromArray4` af_get_dims
  (fromIntegral a, fromIntegral b, fromIntegral c, fromIntegral d)

getNumDims
  :: AFType (a :: *)
  => Array a
  -> Int
getNumDims = fromIntegral . (`infoFromArray` af_get_numdims)

isEmpty
  :: AFType (a :: *)
  => Array a
  -> Bool
isEmpty = (`infoFromArray` af_is_empty)

isScalar
  :: AFType (a :: *)
  => Array a
  -> Bool
isScalar = (`infoFromArray` af_is_scalar)

isRow
  :: AFType (a :: *)
  => Array a
  -> Bool
isRow = (`infoFromArray` af_is_row)

isColumn
  :: AFType (a :: *)
  => Array a
  -> Bool
isColumn = (`infoFromArray` af_is_column)

isVector
  :: AFType (a :: *)
  => Array a
  -> Bool
isVector = (`infoFromArray` af_is_vector)

isComplex
  :: AFType (a :: *)
  => Array a
  -> Bool
isComplex = (`infoFromArray` af_is_complex)

isReal
  :: AFType (a :: *)
  => Array a
  -> Bool
isReal = (`infoFromArray` af_is_real)

isDouble
  :: AFType (a :: *)
  => Array a
  -> Bool
isDouble = (`infoFromArray` af_is_double)

isSingle
  :: AFType (a :: *)
  => Array a
  -> Bool
isSingle = (`infoFromArray` af_is_single)

isRealFloating
  :: AFType (a :: *)
  => Array a
  -> Bool
isRealFloating = (`infoFromArray` af_is_realfloating)

isFloating
  :: AFType (a :: *)
  => Array a
  -> Bool
isFloating = (`infoFromArray` af_is_floating)

isInteger
  :: AFType (a :: *)
  => Array a
  -> Bool
isInteger = (`infoFromArray` af_is_integer)

isBool
  :: AFType (a :: *)
  => Array a
  -> Bool
isBool = (`infoFromArray` af_is_bool)

isSparse
  :: AFType (a :: *)
  => Array a
  -> Bool
isSparse = (`infoFromArray` af_is_sparse)

getDataPtr :: AFArray -> IO (Vector Double)
getDataPtr arr = do
  ptr <- mallocBytes (4 * 8)
  throwAFError =<< af_get_data_ptr (castPtr ptr) arr
  fptr <- newForeignPtr_ ptr
  pure $ unsafeFromForeignPtr0 fptr 4

getScalarBool :: AFType a => Array a -> Bool
getScalarBool (Array fptrA) = do
  unsafePerformIO . withForeignPtr fptrA $ \ptr -> do
    alloca $ \newPtr -> do
      throwAFError =<< af_get_data_ptr newPtr ptr
      peek (castPtr newPtr)
