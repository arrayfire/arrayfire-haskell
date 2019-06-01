module ArrayFire.Algorithm where

import Foreign.C.String
import Foreign.Marshal
import Foreign.Storable
import Prelude                    hiding (sum)

import ArrayFire.Exception
import ArrayFire.Internal.Algorithm
import ArrayFire.Internal.Defines

sum
  :: AFArray
  -> Int
  -> IO AFArray
sum arr1 n = do
  alloca $ \arr -> do
    r <- af_sum arr arr1 n
    putStrLn =<< errorToString r
    peek arr

-- af_sum_nan :: Ptr AFArray -> AFArray -> Int -> Double -> IO AFErr
sumNaN
  :: AFArray
  -> Int
  -> Double
  -> IO AFArray
sumNaN a n d = do
  alloca $ \arr -> do
    r <- af_sum_nan arr a n d
    putStrLn =<< errorToString r
    peek arr

product
  :: AFArray
  -> Int
  -> IO AFArray
product arr1 n = do
  alloca $ \arr -> do
    r <- af_product arr arr1 n
    putStrLn =<< errorToString r
    peek arr

productNaN
  :: AFArray
  -> Int
  -> Double
  -> IO AFArray
productNaN a n d = do
  alloca $ \arr -> do
    r <- af_product_nan arr a n d
    putStrLn =<< errorToString r
    peek arr

min
  :: AFArray
  -> Int
  -> IO AFArray
min arr1 n = do
  alloca $ \arr -> do
    r <- af_min arr arr1 n
    putStrLn =<< errorToString r
    peek arr

max
  :: AFArray
  -> Int
  -> IO AFArray
max arr1 n = do
  alloca $ \arr -> do
    r <- af_max arr arr1 n
    putStrLn =<< errorToString r
    peek arr

allTrue
  :: AFArray
  -> Int
  -> IO AFArray
allTrue arr1 n = do
  alloca $ \arr -> do
    r <- af_all_true arr arr1 n
    putStrLn =<< errorToString r
    peek arr

anyTrue
  :: AFArray
  -> Int
  -> IO AFArray
anyTrue arr1 n = do
  alloca $ \arr -> do
    r <- af_any_true arr arr1 n
    putStrLn =<< errorToString r
    peek arr

-- anyCount
--   :: AFArray
--   -> Int
--   -> IO AFArray
-- anyCount arr1 n = do
--   alloca $ \arr -> do
--     r <- af_any_count arr arr1 n
--     putStrLn =<< errorToString r
--     peek arr


setUnique
  :: AFArray
  -> Bool
  -> IO AFArray
setUnique a k = do
  alloca $ \arr -> do
    r <- af_set_unique arr a k
    putStrLn =<< errorToString r
    peek arr

setUnion
  :: AFArray
  -> AFArray
  -> Bool
  -> IO AFArray
setUnion a b k = do
  alloca $ \arr -> do
    r <- af_set_union arr a b k
    putStrLn =<< errorToString r
    peek arr


setIntersect
  :: AFArray
  -> AFArray
  -> Bool
  -> IO AFArray
setIntersect a b k = do
  alloca $ \arr -> do
    r <- af_set_intersect arr a b k
    putStrLn =<< errorToString r
    peek arr
