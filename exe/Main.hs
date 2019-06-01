{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
module Main where

import Control.Exception
import Prelude hiding (sum, product)
import Control.Concurrent
import Data.Complex

import ArrayFire.Internal.Defines

import ArrayFire.Util
import ArrayFire.Types
import ArrayFire.Exception
import ArrayFire.Device
import ArrayFire.Data
import ArrayFire.Arith
import ArrayFire.Algorithm
import ArrayFire.Array
import ArrayFire.LAPACK
import ArrayFire.Backend
import ArrayFire.Statistics
import ArrayFire.Random
import ArrayFire.Graphics
import ArrayFire.BLAS

main :: IO ()
main = do
  -- Info things
  -- print =<< getVersion
  -- getInfo
  -- print =<< errorToString afErrNoMem
  -- putStrLn =<< getInfoString
  -- print =<< getDeviceCount
  -- print =<< getDevice
  -- print =<< getRevision

  -- Create and print an array
  -- arr1 <- constant 1 1 1 f64
  -- arr2 <- constant 2 1 1 f64
  -- r <- addArray arr1 arr2 True
  -- printArray r

  -- print =<< isLAPACKAvailable
  -- print =<< getActiveBackend

  -- w <- createWindow 300 300 "hey"
  -- showWindow w
  -- threadDelay (secs 10)

  array <- constant @2 99999
  printArray array

  array0 :: Array Bool <- randn @'(2,2)
  printArray array0

  array1 :: Array Bool <- randn @'(2,2)
  printArray array1

  printArray ((array1 `add` array0) True)

--  printArray (add array 1)

  -- putStrLn "got eeem"
  -- print =<< getDataPtr x

  -- x <- constant 10 1 1 f64
  -- printArray =<< mean x 0

  -- print =<< isLAPACKAvailable
  -- print =<< getActiveBackend
  -- print =<< getDeviceCount
  -- print =<< getDevice
  -- putStrLn "info"
  -- getInfo
  -- putStrLn "info string"
  -- putStrLn =<< getInfoString
  -- print =<< getVersion


secs :: Int -> Int
secs = (*1000000)
