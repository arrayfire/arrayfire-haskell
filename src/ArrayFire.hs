--------------------------------------------------------------------------------
-- |
-- Module      : ArrayFire
-- Copyright   : David Johnson (c) 2019-2020
-- License     : BSD3
-- Maintainer  : David Johnson <djohnson.m@gmail.com>
-- Stability   : Experimental
-- Portability : GHC
--
-- <<https://user-images.githubusercontent.com/875324/59819703-0fbaf980-92f7-11e9-8f53-adebea590bfb.png>>
--
--------------------------------------------------------------------------------
module ArrayFire
  ( -- * Tutorial
    -- $tutorial
    module ArrayFire.Algorithm
  , module ArrayFire.Arith
  , module ArrayFire.Array
  , module ArrayFire.Backend
  , module ArrayFire.BLAS
  , module ArrayFire.Data
  , module ArrayFire.Device
  , module ArrayFire.Features
  , module ArrayFire.Graphics
  , module ArrayFire.Image
  , module ArrayFire.Index
  , module ArrayFire.LAPACK
  , module ArrayFire.Random
  , module ArrayFire.Signal
  , module ArrayFire.Sparse
  , module ArrayFire.Statistics
  , module ArrayFire.Types
  , module ArrayFire.Util
  , module ArrayFire.Vision
  , module Foreign.C.Types
  , module Data.Int
  , module Data.Word
  , module Data.Complex
  ) where

import ArrayFire.Algorithm
import ArrayFire.Arith
import ArrayFire.Array
import ArrayFire.Backend
import ArrayFire.BLAS
import ArrayFire.Data
import ArrayFire.Device
import ArrayFire.Features
import ArrayFire.Graphics
import ArrayFire.Image
import ArrayFire.Index
import ArrayFire.LAPACK
import ArrayFire.Random
import ArrayFire.Signal
import ArrayFire.Sparse
import ArrayFire.Statistics
import ArrayFire.Types
import ArrayFire.Util
import ArrayFire.Vision
import ArrayFire.Orphans ()
import Foreign.C.Types
import Data.Int
import Data.Complex
import Data.Word

-- $tutorial
--
-- @
-- module Main where
--
-- import qualified ArrayFire as A
--
-- main :: IO ()
-- main = print $ A.matrix @Double (2,2) [[1,2],[3,4]]
-- @
--
-- * Exception Handling
--
-- @
-- {\-\# LANGUAGE TypeApplications \#\-}
-- module Main where
--
-- import qualified ArrayFire         as A
-- import           Control.Exception ( catch )
--
-- main :: IO ()
-- main = A.printArray action \`catch\` (\\(e :: A.AFException) -> print e)
--   where
--     action =
--       A.matrix \@Double (3,3) [[[1..],[1..],[1..]]]
--         \`A.mul\` A.matrix \@Double (2,2) [[1..],[1..]]
-- @
--
-- The above operation is invalid since the matrix multiply has improper dimensions. The caught exception produces the following error:
--
-- > AFException {afExceptionType = SizeError, afExceptionCode = 203, afExceptionMsg = "Invalid input size"}
--


