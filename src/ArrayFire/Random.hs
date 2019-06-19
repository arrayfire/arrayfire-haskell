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
--------------------------------------------------------------------------------
-- |
-- Module      : ArrayFire.Random
-- Copyright   : David Johnson (c) 2019-2020
-- License     : BSD 3
-- Maintainer  : David Johnson <djohnson.m@gmail.com>
-- Stability   : Experimental
-- Portability : GHC
--
--------------------------------------------------------------------------------
module ArrayFire.Random
  ( createRandomEngine
  , retainRandomEngine
  , setRandomEngine
  , getRandomEngineType
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

-- | Create random number generator object.
createRandomEngine
  :: Int
  -- ^ Initial seed value of random number generator
  -> RandomEngineType
  -- ^ Type of random engine to employ
  -> IO RandomEngine
  -- ^ Opaque RandomEngine handle
createRandomEngine (fromIntegral -> n) typ =
  mask_ $ do
    ptr <-
      alloca $ \ptrInput -> do
        throwAFError =<< af_create_random_engine ptrInput (fromRandomEngine typ) n
        peek ptrInput
    fptr <- newForeignPtr af_release_random_engine_finalizer ptr
    pure (RandomEngine fptr)

-- | Retains 'RandomEngine' reference
retainRandomEngine
  :: RandomEngine
  -- ^ 'RandomEngine' to retain
  -> IO RandomEngine
  -- ^ Retained 'RandomEngine'
retainRandomEngine =
  (`op1re` af_retain_random_engine)

foreign import ccall unsafe "af_random_engine_set_type_"
  af_random_engine_set_type_ :: AFRandomEngine -> AFRandomEngineType -> IO AFErr

-- | Sets RandomEngine to a new 'RandomEngineType'
setRandomEngine
  :: RandomEngine
  -- ^ 'RandomEngine' as input
  -> RandomEngineType
  -- ^ 'RandomEngineType' to set 'RandomEngine' to
  -> IO ()
setRandomEngine r t =
  r `inPlaceEng` (`af_random_engine_set_type_` (fromRandomEngine t))

-- | Retrieves 'RandomEngine'
getRandomEngineType
  :: RandomEngine
  -- ^ 'RandomEngine' argument
  -> IO RandomEngineType
  -- ^ 'RandomEngineType' returned
getRandomEngineType r =
  toRandomEngine <$>
    r `infoFromRandomEngine` af_random_engine_get_type

foreign import ccall unsafe "af_random_engine_set_seed_"
  af_random_engine_set_seed_ :: AFRandomEngine -> IntL -> IO AFErr

-- | Sets seed on 'RandomEngine'
randomEngineSetSeed
  :: RandomEngine
  -- ^ 'RandomEngine' argument
  -> Int
  -- ^ Seed
  -> IO ()
randomEngineSetSeed r t =
  r `inPlaceEng` (`af_random_engine_set_seed_` (fromIntegral t))

-- | Retrieve default 'RandomEngine'
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

-- | Set defualt 'RandomEngine' type
setDefaultRandomEngineType
  :: RandomEngineType
  -- ^ 'RandomEngine' type
  -> IO ()
setDefaultRandomEngineType n =
  afCall (af_set_default_random_engine_type (fromRandomEngine n))

-- | Retrieve seed of 'RandomEngine'
randomEngineGetSeed
  :: RandomEngine
  -- ^ RandomEngine argument
  -> IO Int
randomEngineGetSeed r =
  fromIntegral <$>
    r `infoFromRandomEngine` af_random_engine_get_seed

-- | Set random seed
setSeed :: Int -> IO ()
setSeed = afCall . af_set_seed . fromIntegral

-- | Retrieve random seed
getSeed :: IO Int
getSeed = fromIntegral <$> afCall1 af_get_seed

randEng
  :: forall a . AFType a
  => [Int]
  -> (Ptr AFArray -> CUInt -> Ptr DimT -> AFDtype -> AFRandomEngine -> IO AFErr)
  -> RandomEngine
  -> IO (Array a)
randEng dims f (RandomEngine fptr) = mask_ $
  withForeignPtr fptr $ \rptr -> do
    ptr <- alloca $ \ptrPtr -> do
      withArray (fromIntegral <$> dims) $ \dimArray -> do
        throwAFError =<< f ptrPtr n dimArray typ rptr
        peek ptrPtr
    Array <$>
      newForeignPtr
        af_release_array_finalizer
          ptr
  where
    n = fromIntegral (length dims)
    typ = afType (Proxy @ a)

-- | Generate random 'Array'
rand
  :: forall a . AFType a
  => [Int]
  -- ^ Dimensions
  -> (Ptr AFArray -> CUInt -> Ptr DimT -> AFDtype -> IO AFErr)
  -> IO (Array a)
rand dims f = mask_ $ do
  ptr <- alloca $ \ptrPtr -> do
    withArray (fromIntegral <$> dims) $ \dimArray -> do
      throwAFError =<< f ptrPtr n dimArray typ
      peek ptrPtr
  Array <$>
    newForeignPtr
      af_release_array_finalizer
        ptr
      where
        n = fromIntegral (length dims)
        typ = afType (Proxy @ a)

-- | Generate random 'Array'
randn
  :: forall a
   . AFType a
  => [Int]
  -- ^ Dimensions of random array
  -> IO (Array a)
randn dims = rand @a dims af_randn

-- | Generate random uniform 'Array'
randu
  :: forall a . AFType a
  => [Int]
  -- ^ Dimensions of random array
  -> IO (Array a)
randu dims = rand @a dims af_randu

-- | Generate random 'Array' from uniform distribution
randomUniform
  :: forall a . AFType a
  => [Int]
  -- ^ Dimensions of random array
  -> RandomEngine
  -- ^ 'RandomEngine' argument
  -> IO (Array a)
randomUniform dims eng =
  randEng @a dims af_random_uniform eng

-- | Generate random 'Array' from normal distribution
randomNormal
  :: forall a
   . AFType a
  => [Int]
  -- ^ Dimensions of random array
  -> RandomEngine
  -- ^ 'RandomEngine' argument
  -> IO (Array a)
randomNormal dims eng =
  randEng @a dims af_random_normal eng
