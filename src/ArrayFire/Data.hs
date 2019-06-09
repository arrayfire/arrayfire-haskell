{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE FlexibleContexts    #-}
module ArrayFire.Data where

import Control.Exception
import Control.Monad

import Data.Complex
import Data.Proxy
import Data.Word

import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal            hiding (void)
import Foreign.Marshal.Array
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable

import GHC.Int
import GHC.TypeLits

import ArrayFire.Internal.Array

import ArrayFire.Exception
import ArrayFire.FFI
import ArrayFire.Types
import ArrayFire.Internal.Defines
import ArrayFire.Internal.Data

constant
  :: forall dims
   . (Dims dims)
  => Double -> IO (Array Double)
constant val = do
  ptr <- alloca $ \ptrPtr -> mask_ $ do
    dimArray <- newArray dimt
    throwAFError =<< af_constant ptrPtr val n dimArray typ
    free dimArray
    peek ptrPtr
  Array <$>
    newForeignPtr
      af_release_array_finalizer
        ptr
      where
        n = fromIntegral (length dimt)
        dimt = toDims (Proxy @ dims)
        typ = afType (Proxy @ Double)

constantComplex
  :: forall dims
   . (Dims dims)
  => Complex Double
  -> IO (Array (Complex Double))
constantComplex val = do
  ptr <- alloca $ \ptrPtr -> mask_ $ do
    dimArray <- newArray dimt
    throwAFError =<< af_constant_complex ptrPtr (realPart val) (imagPart val) n dimArray typ
    free dimArray
    peek ptrPtr
  Array <$>
    newForeignPtr
      af_release_array_finalizer
        ptr
      where
        n = fromIntegral (length dimt)
        dimt = toDims (Proxy @ dims)
        typ = afType (Proxy @ (Complex Double))

constantLong
  :: forall dims
   . (Dims dims)
  => Int64
  -> IO (Array Int64)
constantLong val = do
  ptr <- alloca $ \ptrPtr -> mask_ $ do
    dimArray <- newArray dimt
    throwAFError =<< af_constant_long ptrPtr (fromIntegral val) n dimArray
    free dimArray
    peek ptrPtr
  Array <$>
    newForeignPtr
      af_release_array_finalizer
        ptr
      where
        n = fromIntegral (length dimt)
        dimt = toDims (Proxy @ dims)
        typ = afType (Proxy @ Int64)

constantULong
  :: forall dims
   . (Dims dims)
  => Word64
  -> IO (Array Word64)
constantULong val = do
  ptr <- alloca $ \ptrPtr -> mask_ $ do
    dimArray <- newArray dimt
    throwAFError =<< af_constant_ulong ptrPtr (fromIntegral val) n dimArray
    free dimArray
    peek ptrPtr
  Array <$>
    newForeignPtr
      af_release_array_finalizer
        ptr
      where
        n = fromIntegral (length dimt)
        dimt = toDims (Proxy @ dims)
        typ = afType (Proxy @ (Complex Double))

range
  :: forall dims a
   . (Dims dims, AFType a)
  => Int
  -> IO (Array a)
range k = do
  ptr <- alloca $ \ptrPtr -> mask_ $ do
    dimArray <- newArray dimt
    throwAFError =<< af_range ptrPtr n dimArray k typ
    free dimArray

    peek ptrPtr
  Array <$>
    newForeignPtr
      af_release_array_finalizer
        ptr
      where
        n = fromIntegral (length dimt)
        dimt = toDims (Proxy @ dims)
        typ = afType (Proxy @ a)

iota
  :: forall dims tdims a
   . (Dims dims, Dims tdims, AFType a, KnownNat tdims)
  => IO (Array a)
iota = do
  ptr <- alloca $ \ptrPtr -> mask_ $ do
    dimArray <- newArray dimt
    tdimArray <- newArray tdimt
    throwAFError =<< af_iota ptrPtr n dimArray tn tdimArray typ
    free dimArray
    peek ptrPtr
  Array <$>
    newForeignPtr
      af_release_array_finalizer
        ptr
      where
        n = fromIntegral (length dimt)
        dimt = toDims (Proxy @ dims)
        tn = fromIntegral (length dimt)
        tdimt = toDims (Proxy @ tdims)
        typ = afType (Proxy @ a)

identity
  :: forall dims a
   . (Dims dims, AFType a)
  => IO (Array a)
identity = do
  ptr <- alloca $ \ptrPtr -> mask_ $ do
    dimArray <- newArray dimt
    throwAFError =<< af_identity ptrPtr n dimArray typ
    free dimArray
    peek ptrPtr
  Array <$>
    newForeignPtr
      af_release_array_finalizer
        ptr
      where
        n = fromIntegral (length dimt)
        dimt = toDims (Proxy @ dims)
        typ = afType (Proxy @ a)

diagCreate
  :: AFType (a :: *)
  => Array a
  -> Int
  -> Array a
diagCreate x n =
  x `op1` (\p a -> af_diag_create p a n)

diagExtract
  :: AFType (a :: *)
  => Array a
  -> Int
  -> Array a
diagExtract x n =
  x `op1` (\p a -> af_diag_extract p a n)


-- af_err af_join(af_array *out, const int dim, const af_array first, const af_array second);
-- af_err af_join_many(af_array *out, const int dim, const unsigned n_arrays, const af_array *inputs);
-- af_err af_tile(af_array *out, const af_array in, const unsigned x, const unsigned y, const unsigned z, const unsigned w);
-- af_err af_reorder(af_array *out, const af_array in, const unsigned x, const unsigned y, const unsigned z, const unsigned w);
-- af_err af_shift(af_array *out, const af_array in, const int x, const int y, const int z, const int w);
-- af_err af_moddims(af_array *out, const af_array in, const unsigned ndims, const dim_t * const dims);
-- af_err af_flat(af_array *out, const af_array in);
-- af_err af_flip(af_array *out, const af_array in, const unsigned dim);
-- af_err af_lower(af_array *out, const af_array in, bool is_unit_diag);
-- af_err af_upper(af_array *out, const af_array in, bool is_unit_diag);
-- af_err af_select(af_array *out, const af_array cond, const af_array a, const af_array b);
-- af_err af_select_scalar_r(af_array *out, const af_array cond, const af_array a, const double b);
-- af_err af_select_scalar_l(af_array *out, const af_array cond, const double a, const af_array b);
-- af_err af_replace(af_array a, const af_array cond, const af_array b);
-- af_err af_replace_scalar(af_array a, const af_array cond, const double b);
