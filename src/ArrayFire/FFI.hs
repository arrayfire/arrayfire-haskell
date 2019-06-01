{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module ArrayFire.FFI where

import ArrayFire.Exception
import ArrayFire.Types

import ArrayFire.Internal.Defines
import ArrayFire.Internal.Array

import Control.Exception
import Control.Monad
import Foreign.ForeignPtr
import Foreign.Storable
import Foreign.Ptr
import Foreign.C
import Foreign.Marshal.Alloc
import System.IO.Unsafe

op2
  :: Array a
  -> Array a
  -> (Ptr AFArray -> AFArray -> AFArray -> IO AFErr)
  -> Array a
op2 (Array fptr1) (Array fptr2) op =
  unsafePerformIO $ do
    withForeignPtr fptr1 $ \ptr1 ->
      withForeignPtr fptr2 $ \ptr2 -> do
        ptr <-
          alloca $ \ptrInput -> do
            exitCode <- op ptrInput ptr1 ptr2
            unless (exitCode == afSuccess) $ do
              let AFErr afExceptionCode = exitCode
                  afExceptionType = toAFExceptionType exitCode
              afExceptionMsg <- errorToString exitCode
              throwIO AFException {..}
            peek ptrInput
        fptr <- newForeignPtr af_release_array_finalizer ptr
        pure (Array fptr)
