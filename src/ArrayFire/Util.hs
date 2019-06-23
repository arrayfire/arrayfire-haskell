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
-- Various utilities for working with the ArrayFire C library
--
-- @
-- import qualified ArrayFire as A
-- import           Control.Monad
--
-- main :: IO ()
-- main = do
--   let arr = A.constant [1,1,1,1] 10
--   idx <- A.saveArray "key" arr "file.array" False
--   foundIndex <- A.readArrayKeyCheck "file.array" "key"
--   when (idx == foundIndex) $ do
--     array <- A.readArrayKey "file.array" "key"
--     print array
-- @
-- @
-- ArrayFire Array
-- [ 1 1 1 1 ]
--         10
-- @
--------------------------------------------------------------------------------
module ArrayFire.Util where

import Control.Exception

import Data.Proxy
import Foreign.C.String
import Foreign.ForeignPtr
import Foreign.Marshal         hiding (void)
import Foreign.Storable
import System.IO.Unsafe

import ArrayFire.Internal.Util

import ArrayFire.Exception
import ArrayFire.FFI
import ArrayFire.Types

-- | Type alias for ArrayFire API version
type Version = (Int,Int,Int)

-- | Retrieve version for ArrayFire API
--
-- @
-- >>> 'print' '=<<' 'getVersion'
-- @
-- @
-- (3.6.4)
-- @
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
--
-- @
-- >>> 'printArray' ('constant' \@'Double' [1] 1)
-- @
-- @
-- ArrayFire Array
--   [ 1 1 1 1 ]
--       1.0
-- @
printArray
  :: Array a
  -- ^ Input 'Array'
  -> IO ()
printArray (Array fptr) =
  mask_ . withForeignPtr fptr $ \ptr ->
    throwAFError =<< af_print_array ptr

-- | Gets git revision of ArrayFire
--
-- @
-- >>> 'putStrLn' '=<<' 'getRevision'
-- @
-- @
-- 1b8030c5
-- @
getRevision :: IO String
getRevision = peekCString =<< af_get_revision

-- | Prints 'Array' with error codes
--
-- @
-- >>> 'printArrayGen' "test" ('constant' \@'Double' [1] 1) 2
-- @
-- @
-- ArrayFire Array
--   [ 1 1 1 1 ]
--       1.00
-- @
printArrayGen
  :: String
  -- ^  is the expression or name of the array
  -> Array a
  -- ^  is the input array
  -> Int
  -- ^ precision for the display
  -> IO ()
printArrayGen s (Array fptr) (fromIntegral -> prec) = do
  mask_ . withForeignPtr fptr $ \ptr ->
    withCString s $ \cstr ->
      throwAFError =<< af_print_array_gen cstr ptr prec

-- | Saves 'Array' to disk
--
-- Save an array to a binary file.
-- The 'saveArray' and 'readArray' functions are designed to provide store and read access to arrays using files written to disk.
-- <http://arrayfire.org/docs/group__stream__func__save.htm>
-- @
-- >>> 'saveArray' "my array" ('constant' \@'Double' [1] 1) "array.file" 'True'
-- @
-- @
-- 0
-- @
saveArray
  :: String
  -- ^ An expression used as tag/key for the 'Array' during 'readArray'
  -> Array a
  -- ^ Input 'Array'
  -> FilePath
  -- ^ Path that 'Array' will be saved
  -> Bool
  -- ^ Used to append to an existing file when 'True' and create or overwrite an existing file when 'False'
  -> IO Int
  -- ^ The index location of the 'Array' in the file
saveArray key (Array fptr) filename (fromIntegral . fromEnum -> append) = do
  mask_ . withForeignPtr fptr $ \ptr ->
    alloca $ \ptrIdx -> do
      withCString key $ \keyCstr ->
        withCString filename $ \filenameCstr -> do
          throwAFError =<<
            af_save_array ptrIdx keyCstr
              ptr filenameCstr append
          fromIntegral <$> peek ptrIdx

-- | Reads Array by index
--
-- The 'saveArray' and 'readArray' functions are designed to provide store and read access to arrays using files written to disk.
-- <http://arrayfire.org/docs/group__stream__func__save.htm>
--
-- @
-- >>> 'readArrayIndex' "array.file" 0
-- @
-- @
-- ArrayFire Array
--   [ 1 1 1 1 ]
--          10.0000
-- @
readArrayIndex
  :: FilePath
  -- ^ Path to 'Array' location
  -> Int
  -- ^ Index into 'Array'
  -> IO (Array a)
readArrayIndex str (fromIntegral -> idx) =
  withCString str $ \cstr ->
    createArray' (\p -> af_read_array_index p cstr idx)

-- | Reads 'Array' by key
--
-- @
-- >>> 'readArrayKey' "array.file" "my array"
-- @
-- @
-- ArrayFire 'Array'
--    [ 1 1 1 1 ]
--        10.0000
-- @
readArrayKey
  :: FilePath
  -- ^ Path to 'Array'
  -> String
  -- ^ Key of 'Array' on disk
  -> IO (Array a)
  -- ^ Returned 'Array'
readArrayKey fn key =
  withCString fn $ \fcstr ->
    withCString key $ \kcstr ->
      createArray' (\p -> af_read_array_key p fcstr kcstr)

-- | Reads Array, checks if a key exists in the specified file
--
-- When reading by key, it may be a good idea to run this function first to check for the key and then call the readArray using the index.
-- <http://arrayfire.org/docs/group__stream__func__read.htm#ga31522b71beee2b1c06d49b5aa65a5c6f>
--
-- @
-- >>> 'readArrayCheck' "array.file" "my array"
-- @
-- @
-- 0
-- @
readArrayKeyCheck
  :: FilePath
  -- ^ Path to file
  -> String
  -- ^ Key
  -> IO Int
  -- ^ is the tag/name of the array to be read. The key needs to have an exact match.
readArrayKeyCheck a b =
  withCString a $ \acstr ->
    withCString b $ \bcstr ->
      fromIntegral <$>
        afCall1 (\p -> af_read_array_key_check p acstr bcstr)

-- | Convert ArrayFire 'Array' to 'String', used for 'Show' instance.
--
-- @
-- >>> 'putStrLn' '$' 'arrayString' ('constant' \@'Double' 10 [1,1,1,1])
-- @
-- @
-- ArrayFire 'Array'
--    [ 1 1 1 1 ]
--        10.0000
-- @
arrayString
  :: Array a
  -- ^ Input 'Array'
  -> String
  -- ^ 'String' representation of 'Array'
arrayString a = arrayToString "ArrayFire Array" a 4 False

-- | Convert ArrayFire Array to String
--
-- @
-- >>> 'putStrLn' '$' 'arrayToString' "ArrayFire Array" ('constant' \@'Double' 10 [1,1,1,1]) 4 'False'
-- @
-- @
-- ArrayFire 'Array'
--    [ 1 1 1 1 ]
--        10.0000
-- @
arrayToString
  :: String
  -- ^ Name of 'Array'
  -> Array a
  -- ^ 'Array' input
  -> Int
  -- ^ Precision of 'Array' values.
  -> Bool
  -- ^ If 'True', performs takes the transpose before rendering to 'String'
  -> String
  -- ^ 'Array' rendered to 'String'
arrayToString expr (Array fptr) (fromIntegral -> prec) (fromIntegral . fromEnum -> trans) =
  unsafePerformIO . mask_ . withForeignPtr fptr $ \aptr ->
    withCString expr $ \expCstr ->
      alloca $ \ocstr -> do
        throwAFError =<< af_array_to_string ocstr expCstr aptr prec trans
        peekCString =<< peek ocstr

-- | Retrieve size of ArrayFire data type
--
-- @
-- >>> 'getSizeOf' ('Proxy' \@ 'Double')
-- @
-- @
-- 8
-- @
getSizeOf
  :: forall a . AFType a
  => Proxy a
  -- ^ Witness of Haskell type that mirrors ArrayFire type.
  -> Int
  -- ^ Size of ArrayFire type
getSizeOf proxy =
  unsafePerformIO . mask_ . alloca $ \csize -> do
    throwAFError =<< af_get_size_of csize (afType proxy)
    fromIntegral <$> peek csize
