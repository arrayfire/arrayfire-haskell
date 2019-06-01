module ArrayFire.Array where

import Control.Monad

import Foreign.Marshal hiding (void)
import Foreign.Storable
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C.String

import ArrayFire.Internal.Array
import ArrayFire.Exception
import ArrayFire.Internal.Defines
import ArrayFire.Internal.Data
import ArrayFire.Internal.Arith

import Data.Vector.Storable

eval :: AFArray -> IO ()
eval arr = () <$ af_eval arr

flat :: AFArray -> IO AFArray
flat arr1 = do
  alloca $ \ptr -> do
    void $ af_flat ptr arr1
    peek ptr

getDataPtr :: AFArray -> IO (Vector Double)
getDataPtr arr = do
  ptr <- mallocBytes (4 * 8)
  af_get_data_ptr (castPtr ptr) arr
  fptr <- newForeignPtr_ ptr
  pure $ unsafeFromForeignPtr0 fptr 4
