module ArrayFire.Arith where

import Foreign.Marshal
import Foreign.Storable
import Foreign.C.String

import ArrayFire.Exception
import ArrayFire.Internal.Arith
import ArrayFire.Internal.Defines

addArray
  :: AFArray
  -> AFArray
  -> Batch
  -> IO AFArray
addArray arr1 arr2 batch = do
  putStrLn "making constant array"
  alloca $ \arr -> do
    r <- af_add arr arr1 arr2 batch
    putStrLn =<< errorToString r
    peek arr


