{-# LANGUAGE ViewPatterns #-}
module ArrayFire.Algorithm where

import ArrayFire.Types
import ArrayFire.FFI
import ArrayFire.Internal.Algorithm

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
sumAll = (`infoFromArray2` af_sum_all)

sumNaNAll
  :: AFType a
  => Array a
  -> Double
  -> (Double, Double)
sumNaNAll a d = infoFromArray2 a (\p g x -> af_sum_nan_all p g x d)

productAll
  :: AFType a
  => Array a
  -> (Double, Double)
productAll = (`infoFromArray2` af_product_all)

productNaNAll
  :: AFType a
  => Array a
  -> Double
  -> (Double, Double)
productNaNAll a d = infoFromArray2 a (\p x y -> af_product_nan_all p x y d)

minAll
  :: AFType a
  => Array a
  -> (Double, Double)
minAll = (`infoFromArray2` af_min_all)

maxAll
  :: AFType a
  => Array a
  -> (Double, Double)
maxAll = (`infoFromArray2` af_max_all)

allTrueAll
  :: AFType a
  => Array a
  -> (Double, Double)
allTrueAll = (`infoFromArray2` af_all_true_all)

anyTrueAll
  :: AFType a
  => Array a
  -> (Double, Double)
anyTrueAll = (`infoFromArray2` af_any_true_all)

countAll
  :: AFType a
  => Array a
  -> (Double, Double)
countAll = (`infoFromArray2` af_count_all)

imin
  :: AFType a
  => Array a
  -> Int
  -> (Array a, Array a)
imin a n = op2p a (\x y z -> af_imin x y z n)

imax
  :: AFType a
  => Array a
  -> Int
  -> (Array a, Array a)
imax a n = op2p a (\x y z -> af_imax x y z n)

iminAll
  :: AFType a
  => Array a
  -> (Double, Double, Int)
iminAll a = do
  let (x,y,fromIntegral -> z) = a `infoFromArray3` af_imin_all
  (x,y,z)

imaxAll
  :: AFType a
  => Array a
  -> (Double, Double, Int)
imaxAll a = do
  let (x,y,fromIntegral -> z) = a `infoFromArray3` af_imax_all
  (x,y,z)

accum
  :: AFType a
  => Array a
  -> Int
  -> Array a
accum a n = a `op1` (\x y -> af_accum x y n)

scan
  :: AFType a
  => Array a
  -> Int
  -> BinaryOp
  -> Bool
  -> Array a
scan a d op batch =
  a `op1` (\x y -> af_scan x y d (toBinaryOp op) batch)

scanByKey
  :: AFType a
  => Array a
  -> Array a
  -> Int
  -> BinaryOp
  -> Bool
  -> Array a
scanByKey a b d op batch =
  op2 a b (\x y z -> af_scan_by_key x y z d (toBinaryOp op) batch)

-- Clashes with Haskell's "where" keyword, prefix by `af`
afWhere
  :: AFType a
  => Array a
  -> Array a
afWhere = (`op1` af_where)

diff1
  :: AFType a
  => Array a
  -> Int
  -> Array a
diff1 a n = a `op1` (\p x -> af_diff1 p x n)

diff2
  :: AFType a
  => Array a
  -> Int
  -> Array a
diff2 a n = a `op1` (\p x -> af_diff2 p x n)

sort
  :: AFType a
  => Array a
  -> Int
  -> Bool
  -> Array a
sort a (fromIntegral -> n) b =
  a `op1` (\p x -> af_sort p x n b)

sortIndex
  :: AFType a
  => Array a
  -> Int
  -> Bool
  -> (Array a, Array a)
sortIndex a (fromIntegral -> n) b =
  a `op2p` (\p1 p2 p3 -> af_sort_index p1 p2 p3 n b)

sortByKey
  :: AFType a
  => Array a
  -> Array a
  -> Int
  -> Bool
  -> (Array a, Array a)
sortByKey a1 a2 (fromIntegral -> n) b =
  op2p2 a1 a2 (\w x y z -> af_sort_by_key w x y z n b)

setUnique
  :: AFType a
  => Array a
  -> Bool
  -> Array a
setUnique a b =
  op1 a (\x y -> af_set_unique x y b)

setUnion
  :: AFType a
  => Array a
  -> Array a
  -> Bool
  -> Array a
setUnion a1 a2 b =
  op2 a1 a2 (\x y z -> af_set_union x y z b)

setIntersect
  :: AFType a
  => Array a
  -> Array a
  -> Bool
  -> Array a
setIntersect a1 a2 b =
  op2 a1 a2 (\x y z -> af_set_intersect x y z b)
