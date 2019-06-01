{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE KindSignatures      #-}
module ArrayFire.Data where

import Control.Exception
import Control.Monad

import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal            hiding (void)
import Foreign.Marshal.Array
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable

import Data.Proxy

import ArrayFire.Internal.Array

import ArrayFire.Exception
import ArrayFire.Types
import ArrayFire.Internal.Defines
import ArrayFire.Internal.Data

    -- /**
    --     \param[out] arr is the generated array of given type
    --     \param[in] val is the value of each element in the generated array
    --     \param[in] ndims is size of dimension array \p dims
    --     \param[in] dims is the array containing sizes of the dimension
    --     \param[in] type is the type of array to generate
    --    \ingroup data_func_constant
    -- /

constant
  :: forall dims
   . (Dims dims)
  => Double -> IO (Array Double)
constant val = do
  ptr <- alloca $ \ptrPtr -> mask_ $ do
    dimArray <- newArray dimt
    exitCode <- af_constant ptrPtr val n dimArray typ
    unless (exitCode == afSuccess) $ do
      let AFErr afExceptionCode = exitCode
          afExceptionType = toAFExceptionType exitCode
      afExceptionMsg <- errorToString exitCode
      throwIO AFException {..}
    peek ptrPtr
  Array <$>
    newForeignPtr
      af_release_array_finalizer
        ptr
      where
        n = fromIntegral (length dimt)
        dimt :: [DimT] = toDims (Proxy @ dims)
        typ = afType (Proxy @ Double)
