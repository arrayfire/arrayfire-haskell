module ArrayFire.Util where

import Foreign.Marshal
import Foreign.Storable

import ArrayFire.Internal.Util
import ArrayFire.Internal.Defines

type Version = (Int,Int,Int)

getVersion :: IO (Either String Version)
getVersion = do
  alloca $ \x ->
    alloca $ \y ->
      alloca $ \z -> do
        r <- af_get_version x y z
        if r /= afSuccess
          then pure $ Left (show r)
          else do
            x <- (,,) <$> peek x <*> peek y <*> peek z
            pure (Right x)

printArray :: AFArray -> IO ()
printArray a = () <$ af_print_array a

getRevision = af_get_revision
