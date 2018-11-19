module ArrayFire.Statistics where

import Foreign.Marshal
import Foreign.Storable
import Foreign.C.String

-- import ArrayFire.Internal.Util
import ArrayFire.Internal.Statistics
import ArrayFire.Exception
import ArrayFire.Internal.Defines

mean arr dimt = do
  alloca $ \ptr -> do
    print =<< af_mean ptr arr dimt
    peek ptr
