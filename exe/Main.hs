module Main where

import Control.Concurrent

import ArrayFire.Internal.Defines

import ArrayFire.Util
import ArrayFire.Exception
import ArrayFire.Device
import ArrayFire.Data
import ArrayFire.Arith
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

  -- -- Create and print an array
  -- arr1 <- constant 1 1 1 f64
  -- arr2 <- constant 2 1 1 f64
  -- r <- addArray arr1 arr2 True
  -- printArray r

  -- print =<< isLAPACKAvailable
  -- print =<< getActiveBackend

  -- w <- createWindow 300 300 "hey"
  -- showWindow w
  -- threadDelay (secs 10)

  a1 <- randn 1 1 1 f64
  a2 <- randn 1 1 1 f64
  a3 <- matmul a1 a2 afMatNone afMatNone
  printArray a3

  -- x <- constant 10 1 1 f64
  -- printArray =<< mean x 0

secs :: Int -> Int
secs = (*1000000)
