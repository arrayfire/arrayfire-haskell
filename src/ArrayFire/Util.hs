module ArrayFire.Util where

import Control.Monad
import Control.Exception
import Foreign.Marshal hiding (void)
import Foreign.Storable
import Foreign.ForeignPtr

import ArrayFire.Internal.Util
import ArrayFire.Internal.Defines

import ArrayFire.Types

type Version = (Int,Int,Int)

getVersion :: IO Version
getVersion = do
  alloca $ \x ->
    alloca $ \y ->
      alloca $ \z -> do
        r <- af_get_version x y z
        when (r /= afSuccess) (error "oops")
        x <- (,,) <$> peek x <*> peek y <*> peek z
        pure x

printAFArray :: AFArray -> IO ()
printAFArray a = () <$ af_print_array a

printArray :: Array a -> IO ()
printArray (Array fptr) =
  void (withForeignPtr fptr af_print_array)

getRevision = af_get_revision
