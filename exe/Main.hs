{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE DataKinds           #-}
module Main where

import ArrayFire
import Control.Concurrent
import Prelude            hiding (sum, product)
-- import GHC.RTS

foreign import ccall safe "test_bool"
  testBool :: IO ()

foreign import ccall safe "test_window"
  testWindow :: IO ()

main :: IO ()
main = do
  -- testWindow
  -- ks <- randn @Double [100,100]
  -- saveArray "key" ks "array.txt" False
  -- !ks' <- readArrayKey "array.txt" "key"
  -- print ks'

--   info >> putStrLn "ok" >> afInit
--   -- Info things
--   print =<< getSizeOf (Proxy @ Double)
--   print =<< getVersion
--   print =<< getRevision
--   -- getInfo
--   -- print =<< errorToString afErrNoMem
--   putStrLn =<< getInfoString
--   print =<< getDeviceCount
--   print =<< getDevice

--   -- Create and print an array
--   -- arr1 <- constant 1 1 1 f64
--   -- arr2 <- constant 2 1 1 f64
--   -- r <- addArray arr1 arr2 True
--   -- printArray r

--   -- print =<< isLAPACKAvailable
--   -- print =<< getAvailableBackends
--   -- print =<< getActiveBackend
--   -- print =<< getAvailableBackends

--   -- array <- constant @'(10,10) 200
--   -- putStrLn "backend id"
--   -- print (getBackendID array)
--   -- putStrLn "device id"
--   -- print (getDeviceID array)

--   -- array <- randu @'(9,9,9) @Double
--   -- printArray array  -- printArray (mean array 0)

-- --  printArray (add array 1)

--   -- putStrLn "got eeem"
--   -- print =<< getDataPtr x

--   -- x <- constant 10 1 1 f64
--   -- printArray =<< mean x 0

-- --  print =<< isLAPACKAvailable

--   a <- randu @'(3,3) @Float
--   b <- randu @'(3,3) @Float
--   printArray ((a `matmul` b) None None)
--     `catch` (\(e :: AFException) -> do
--                 putStrLn "got one"
--                 print e)

  putStrLn "create window"
  window <- createWindow 200 200 "hey"
  putStrLn "set visibility"
  setVisibility window True
  putStrLn "show window"
  showWindow window
  threadDelay (secs 10)

--   -- print =<< getActiveBackend
--   -- print =<< getDeviceCount
--   -- print =<< getDevice
--   -- putStrLn "info"
--   -- getInfo
--   -- putStrLn "info string"
--   -- putStrLn =<< getInfoString
--   -- print =<< getVersion


secs :: Int -> Int
secs = (*1000000)
