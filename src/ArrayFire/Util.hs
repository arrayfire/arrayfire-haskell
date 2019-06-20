{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}
--------------------------------------------------------------------------------
-- |
-- Module      : ArrayFire.Util
-- Copyright   : David Johnson (c) 2019-2020
-- License     : BSD 3
-- Maintainer  : David Johnson <djohnson.m@gmail.com>
-- Stability   : Experimental
-- Portability : GHC
--
--------------------------------------------------------------------------------
module ArrayFire.Util where

import Control.Exception

import Data.Proxy
import Foreign.C.String
import Foreign.ForeignPtr
import Foreign.Marshal            hiding (void)
import Foreign.Storable
import System.IO.Unsafe

import ArrayFire.Internal.Util

import ArrayFire.Exception
import ArrayFire.FFI
import ArrayFire.Types

type Version = (Int,Int,Int)

-- | Retrieve version for ArrayFire API
getVersion :: IO Version
getVersion =
  alloca $ \x ->
    alloca $ \y ->
      alloca $ \z -> do
        throwAFError =<< af_get_version x y z
        (,,) <$> (fromIntegral <$> peek x)
             <*> (fromIntegral <$> peek y)
             <*> (fromIntegral <$> peek z)

-- | Prints array to stdout
printArray :: Array a -> IO ()
printArray (Array fptr) =
  mask_ . withForeignPtr fptr $ \ptr ->
    throwAFError =<< af_print_array ptr

-- | Gets git revision of ArrayFire
getRevision :: IO String
getRevision = peekCString =<< af_get_revision


-- | Saves 'Array' to disk
printArrayGen
  :: String
  -> Array a
  -> Int
  -> IO ()
printArrayGen s (Array fptr) (fromIntegral -> prec) = do
  mask_ . withForeignPtr fptr $ \ptr ->
    withCString s $ \cstr ->
      throwAFError =<< af_print_array_gen cstr ptr prec

-- | Saves 'Array' to disk
saveArray
  :: Int
  -> String
  -> Array a
  -> String
  -> Bool
  -> IO ()
saveArray (fromIntegral -> idx) key (Array fptr) filename (fromIntegral . fromEnum -> append) = do
  mask_ . withForeignPtr fptr $ \ptr ->
    alloca $ \ptrIdx -> do
      poke ptrIdx idx
      withCString key $ \keyCstr ->
        withCString filename $ \filenameCstr ->
          throwAFError =<<
            af_save_array ptrIdx keyCstr
              ptr filenameCstr append

-- | Reads Array by index
readArrayIndex
  :: String
  -> Int
  -> IO (Array a)
readArrayIndex str (fromIntegral -> idx) =
  withCString str $ \cstr ->
    createArray' (\p -> af_read_array_index p cstr idx)

-- | Reads Array by key
readArrayKey
  :: String
  -> String
  -> IO (Array a)
readArrayKey fn key =
  withCString fn $ \fcstr ->
    withCString key $ \kcstr ->
      createArray' (\p -> af_read_array_key p fcstr kcstr)

-- | Reads Array
readArrayKeyCheck
  :: String
  -> String
  -> IO Int
readArrayKeyCheck a b =
  withCString a $ \acstr ->
    withCString b $ \bcstr ->
      fromIntegral <$> 
        afCall1 (\p -> af_read_array_key_check p acstr bcstr)

-- | Convert ArrayFire Array to String, used for 'Show' instance
arrayString :: Array a -> String
arrayString a = arrayToString "ArrayFire Array" a 4 False

-- | Convert ArrayFire Array to String
arrayToString
  :: String
  -> Array a
  -> Int
  -> Bool
  -> String
arrayToString expr (Array fptr) (fromIntegral -> prec) (fromIntegral . fromEnum -> trans) =
  unsafePerformIO . mask_ . withForeignPtr fptr $ \aptr ->
    withCString expr $ \expCstr ->
      alloca $ \ocstr -> do
        throwAFError =<< af_array_to_string ocstr expCstr aptr prec trans
        peekCString =<< peek ocstr

-- af_err af_example_function(af_array* out, const af_array in, const af_someenum_t param);

-- | Retrieve size of ArrayFire data type
getSizeOf :: forall a . AFType a => Proxy a -> Int
getSizeOf proxy =
  unsafePerformIO . mask_ . alloca $ \csize -> do
    throwAFError =<< af_get_size_of csize (afType proxy)
    fromIntegral <$> peek csize
