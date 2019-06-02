{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE ViewPatterns         #-}
{-# LANGUAGE KindSignatures       #-}
module ArrayFire.Random where

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
import ArrayFire.Internal.Random

randn
  :: forall dims a
   . (Dims dims, AFType a)
  => IO (Array a)
randn = do
  ptr <- alloca $ \ptrPtr -> mask_ $ do
    dimArray <- newArray dimt
    throwAFError =<< af_randn ptrPtr n dimArray typ
    peek ptrPtr
  Array <$>
    newForeignPtr
      af_release_array_finalizer
        ptr
      where
        n = fromIntegral (length dimt)
        dimt :: [DimT] = toDims (Proxy @ dims)
        typ = afType (Proxy @ a)
