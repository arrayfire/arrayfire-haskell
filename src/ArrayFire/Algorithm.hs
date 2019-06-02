module ArrayFire.Algorithm where

import Foreign.Marshal
import Foreign.Storable
import Foreign.C.String

import ArrayFire.Exception
import ArrayFire.Types
import ArrayFire.FFI
import ArrayFire.Internal.Algorithm
import ArrayFire.Internal.Defines

sum
  :: AFType a
  => Array a
  -> Int
  -> Array a
sum x n = x `op1` (\p a -> af_sum p a n)

sumNaN
  :: AFType a
  => Array a
  -> Int
  -> Double
  -> Array a
sumNaN n i d = n `op1` (\p a -> af_sum_nan p a i d)

product
  :: AFType a
  => Array a
  -> Int
  -> Array a
product x n = x `op1` (\p a -> af_product p a n)

productNaN
  :: AFType a
  => Array a
  -> Int
  -> Double
  -> Array a
productNaN n i d = n `op1` (\p a -> af_product_nan p a i d)

min
  :: AFType a
  => Array a
  -> Int
  -> Array a
min x n = x `op1` (\p a -> af_min p a n)

max
  :: AFType a
  => Array a
  -> Int
  -> Array a
max x n = x `op1` (\p a -> af_max p a n)

allTrue
  :: AFType a
  => Array a
  -> Int
  -> Array a
allTrue x n = x `op1` (\p a -> af_all_true p a n)

anyTrue
  :: AFType a
  => Array a
  -> Int
  -> Array a
anyTrue x n = x `op1` (\p a -> af_any_true p a n)

count
  :: AFType a
  => Array a
  -> Int
  -> Array a
count x n = x `op1` (\p a -> af_count p a n)

sumAll
  :: AFType a
  => Array a
  -> (Double, Double)
sumAll _ = undefined

sumNaNAll
  :: AFType a
  => Array a
  -> Double
  -> (Double, Double)
sumNaNAll _ _ = undefined

productAll
  :: AFType a
  => Array a
  -> (Double, Double)
productAll _ = undefined

productNaNAll
  :: AFType a
  => Array a
  -> Double
  -> (Double, Double)
productNaNAll _ _ = undefined

minAll
  :: AFType a
  => Array a
  -> (Double, Double)
minAll = undefined

maxAll
  :: AFType a
  => Array a
  -> (Double, Double)
maxAll = undefined

allTrueAll
  :: AFType a
  => Array a
  -> (Double, Double)
allTrueAll = undefined

anyTrueAll
  :: AFType a
  => Array a
  -> (Double, Double)
anyTrueAll = undefined

countAll
  :: AFType a
  => Array a
  -> (Double, Double)
countAll = undefined

imin
  :: AFType a
  => Array a
  -> Int -- * dimension
  -> (Array a, Array a)
imin = undefined

imax
  :: AFType a
  => Array a
  -> Int -- * dimension
  -> (Array a, Array a)
imax = undefined

iminAll
  :: AFType a
  => Array a
  -> Int -- * dimension
  -> Double
  -> (Array a, Array a)
iminAll = undefined

imaxAll
  :: AFType a
  => Array a
  -> Int -- * dimension
  -> Double
  -> (Array a, Array a)
imaxAll = undefined

accum
  :: AFType a
  => Array a
  -> Int -- * dimension
  -> Array a
accum x n = x `op1` (\x y -> af_accum x y n)


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
