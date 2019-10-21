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
-- License     : BSD3
-- Maintainer  : David Johnson <djohnson.m@gmail.com>
-- Stability   : Experimental
-- Portability : GHC
--
-- 'RandomEngine' generation, Random 'Array' generation.
--
-- @
-- {-\# LANGUAGE TypeApplications \#-}
-- module Main where
--
-- import 'ArrayFire'
--
-- main :: IO ()
-- main = do
--   seed <- 'getSeed'
--   -- ^ Retrieves seed
--   engine <- 'createRandomEngine' 'Mersenne' seed
--   -- ^ Creates engine
--   array <- 'randomUniform' [3,3] engine
--   -- ^ Creates random Array from engine with uniformly distributed data
--   print array
--
--   print =<< 'randu' @'Double' [2,2]
--   -- ^ Shorthand for creating random 'Array'
-- @
-- @
-- ArrayFire 'Array'
-- [3 3 1 1]
--    0.4446     0.1143     0.4283
--    0.5725     0.1456     0.9182
--    0.1915     0.1643     0.5997
-- @
-- @
-- ArrayFire 'Array'
-- [2 2 1 1]
--    0.6010     0.0278
--    0.9806     0.2126
-- @
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
import ArrayFire.Internal.Types
import ArrayFire.Internal.Defines
import ArrayFire.Internal.Random
import ArrayFire.FFI

-- | Create random number generator object.
--
-- @
-- >>> engine <- createRandomEngine 100 Philox
-- @
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
--
-- @
-- >>> nextEngine <- retainRandomEngine engine
-- @
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
--
-- @
-- >>> setRandomEngine engine Philox
-- @
setRandomEngine
  :: RandomEngine
  -- ^ 'RandomEngine' as input
  -> RandomEngineType
  -- ^ 'RandomEngineType' to set 'RandomEngine' to
  -> IO ()
setRandomEngine r t =
  r `inPlaceEng` (`af_random_engine_set_type_` (fromRandomEngine t))

-- | Retrieves 'RandomEngine'
--
-- @
-- >>> randomEngineType <- getRandomEngineType engine
-- @
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
--
-- @
-- >>> randomEngineSetSeed engine 100
-- @
randomEngineSetSeed
  :: RandomEngine
  -- ^ 'RandomEngine' argument
  -> Int
  -- ^ Seed
  -> IO ()
randomEngineSetSeed r t =
  r `inPlaceEng` (`af_random_engine_set_seed_` (fromIntegral t))

-- | Retrieve default 'RandomEngine'
--
-- @
-- >>> engine <- getDefaultRandomEngine
-- @
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
--
-- @
-- >>> setDefaultRandomEngineType Philox
-- @
setDefaultRandomEngineType
  :: RandomEngineType
  -- ^ 'RandomEngine' type
  -> IO ()
setDefaultRandomEngineType n =
  afCall (af_set_default_random_engine_type (fromRandomEngine n))

-- | Retrieve seed of 'RandomEngine'
--
-- @
-- >>> seed <- randomEngineGetSeed engine
-- @
randomEngineGetSeed
  :: RandomEngine
  -- ^ RandomEngine argument
  -> IO Int
randomEngineGetSeed r =
  fromIntegral <$>
    r `infoFromRandomEngine` af_random_engine_get_seed

-- | Set random seed
--
-- @
-- >>> setSeed 100
-- @
setSeed :: Int -> IO ()
setSeed = afCall . af_set_seed . fromIntegral

-- | Retrieve random seed
--
-- @
-- >>> seed <- getSeed
-- @
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

rand
  :: forall a . AFType a
  => [Int]
  -- ^ Dimensions
  -> (Ptr AFArray -> CUInt -> Ptr DimT -> AFDtype -> IO AFErr)
  -> IO (Array a)
rand dims f = mask_ $ do
  ptr <- alloca $ \ptrPtr -> do
    zeroOutArray ptrPtr
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
--
-- >>> randn @Double [2,2]
--
-- ArrayFire Array
-- [2 2 1 1]
--    0.9428    -0.9523
--   -1.0564    -0.4199
--
randn
  :: forall a
   . AFType a
  => [Int]
  -- ^ Dimensions of random array
  -> IO (Array a)
randn dims = rand @a dims af_randn

-- | Generate random uniform 'Array'
--
-- >>> randu @Double [2,2]
--
-- ArrayFire Array
-- [2 2 1 1]
--    0.6010     0.0278
--    0.9806     0.2126
--
randu
  :: forall a . AFType a
  => [Int]
  -- ^ Dimensions of random array
  -> IO (Array a)
randu dims = rand @a dims af_randu

-- | Generate random uniform 'Array'
--
-- >>> randonUniform @Double [2,2] =<< getDefaultRandomEngine
--
-- ArrayFire Array
-- [2 2 1 1]
--    0.0655     0.5497
--    0.2864     0.3410
--
randomUniform
  :: forall a . AFType a
  => [Int]
  -- ^ Dimensions of random array
  -> RandomEngine
  -- ^ 'RandomEngine' argument
  -> IO (Array a)
randomUniform dims eng =
  randEng @a dims af_random_uniform eng

-- | Generate random uniform 'Array'
--
-- >>> randonNormal @Double [2,2] =<< getDefaultRandomEngine
--
-- ArrayFire Array
-- [2 2 1 1]
--   -1.1850    -0.2946
--   -0.7206    -0.6813
--
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
