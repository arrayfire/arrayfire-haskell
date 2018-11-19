module ArrayFire.Array where

import Foreign.Marshal
import Foreign.Storable
import Foreign.C.String

import ArrayFire.Internal.Array
import ArrayFire.Exception
import ArrayFire.Internal.Defines

eval :: AFArray -> IO ()
eval arr = () <$ af_eval arr
