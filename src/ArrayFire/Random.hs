{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE KindSignatures      #-}
module ArrayFire.Random
  ( createRandomEngine
  , retainRandomEngine
  , setRandomEngine
  , getRandomEngine
  , randomEngineSetSeed
  , getDefaultRandomEngine
  , setDefaultRandomEngineType
  , randomEngineGetSeed
  , setSeed
  , getSeed
  , randn
  , randu
  , randomUniform
  , randomNormal
  ) where

import Control.Exception
import Data.Proxy
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Marshal            hiding (void)
import Foreign.Ptr
import Foreign.Storable

import ArrayFire.Exception
import ArrayFire.Types
import ArrayFire.Internal.Defines
import ArrayFire.Internal.Random
import ArrayFire.FFI

createRandomEngine
  :: Int
  -> RandomEngineType
  -> IO RandomEngine
createRandomEngine (fromIntegral -> n) typ =
  mask_ $ do
    ptr <-
      alloca $ \ptrInput -> do
        throwAFError =<< af_create_random_engine ptrInput (fromRandomEngine typ) n
        peek ptrInput
    fptr <- newForeignPtr af_release_random_engine_finalizer ptr
    pure (RandomEngine fptr)

retainRandomEngine
  :: RandomEngine
  -> IO RandomEngine
retainRandomEngine =
  (`op1re` af_retain_random_engine)

foreign import ccall unsafe "af_random_engine_set_type_"
  af_random_engine_set_type_ :: AFRandomEngine -> AFRandomEngineType -> IO AFErr

setRandomEngine
  :: RandomEngine
  -> RandomEngineType
  -> IO ()
setRandomEngine r t =
  r `inPlaceEng` (`af_random_engine_set_type_` (fromRandomEngine t))

getRandomEngine
  :: RandomEngine
  -> IO RandomEngineType
getRandomEngine r =
  toRandomEngine <$>
    r `infoFromRandomEngine` af_random_engine_get_type

foreign import ccall unsafe "af_random_engine_set_seed_"
  af_random_engine_set_seed_ :: AFRandomEngine -> IntL -> IO AFErr

randomEngineSetSeed
  :: RandomEngine
  -> Int
  -> IO ()
randomEngineSetSeed r t =
  r `inPlaceEng` (`af_random_engine_set_seed_` (fromIntegral t))

getDefaultRandomEngine
  :: IO RandomEngine
getDefaultRandomEngine =
  mask_ $ do
    ptr <-
      alloca $ \ptrInput -> do
        throwAFError =<< af_get_default_random_engine ptrInput
        peek ptrInput
    fptr <- newForeignPtr af_release_random_engine_finalizer ptr
    pure (RandomEngine fptr)

setDefaultRandomEngineType
  :: RandomEngineType
  -> IO ()
setDefaultRandomEngineType n =
  afCall (af_set_default_random_engine_type (fromRandomEngine n))

randomEngineGetSeed
  :: RandomEngine
  -> IO Int
randomEngineGetSeed r =
  fromIntegral <$>
    r `infoFromRandomEngine` af_random_engine_get_seed

setSeed :: Int -> IO ()
setSeed = afCall . af_set_seed . fromIntegral

getSeed :: IO Int
getSeed = fromIntegral <$> afCall1 af_get_seed

randEng
  :: forall dims a
   . (Dims dims, AFType a)
   => (Ptr AFArray -> CUInt -> Ptr DimT -> AFDtype -> AFRandomEngine -> IO AFErr)
   -> RandomEngine
   -> IO (Array a)
randEng f (RandomEngine fptr) = mask_ $
  withForeignPtr fptr $ \rptr -> do
    ptr <- alloca $ \ptrPtr -> do
      dimArray <- newArray dimt
      throwAFError =<< f ptrPtr n dimArray typ rptr
      free dimArray
      peek ptrPtr
    Array <$>
      newForeignPtr
        af_release_array_finalizer
          ptr
  where
    n = fromIntegral (length dimt)
    dimt :: [DimT] = toDims (Proxy @ dims)
    typ = afType (Proxy @ a)

rand
  :: forall dims a
   . (Dims dims, AFType a)
   => (Ptr AFArray -> CUInt -> Ptr DimT -> AFDtype -> IO AFErr)
   -> IO (Array a)
rand f = mask_ $ do
  ptr <- alloca $ \ptrPtr -> do
    dimArray <- newArray dimt
    throwAFError =<< f ptrPtr n dimArray typ
    free dimArray
    peek ptrPtr
  Array <$>
    newForeignPtr
      af_release_array_finalizer
        ptr
      where
        n = fromIntegral (length dimt)
        dimt :: [DimT] = toDims (Proxy @ dims)
        typ = afType (Proxy @ a)

randn
  :: forall dims a
   . (Dims dims, AFType a)
  => IO (Array a)
randn = rand @dims @a af_randn

randu
  :: forall dims a
   . (Dims dims, AFType a)
  => IO (Array a)
randu = rand @dims @a af_randu

randomUniform
  :: forall dims a
   . (Dims dims, AFType a)
  => RandomEngine
  -> IO (Array a)
randomUniform eng =
  randEng @dims @a af_random_uniform eng

randomNormal
  :: forall dims a
   . (Dims dims, AFType a)
  => RandomEngine
  -> IO (Array a)
randomNormal eng =
  randEng @dims @a af_random_normal eng
