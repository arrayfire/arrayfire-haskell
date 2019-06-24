{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE ViewPatterns         #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE TypeFamilies         #-}
--------------------------------------------------------------------------------
-- |
-- Module      : ArrayFire.Types
-- Copyright   : David Johnson (c) 2019-2020
-- License     : BSD3
-- Maintainer  : David Johnson <djohnson.m@gmail.com>
-- Stability   : Experimental
-- Portability : GHC
--
-- ArrayFire Types
--
-- @
-- module Main where
--
-- import ArrayFire
--
-- main :: IO ()
-- main = print =<< getAvailableBackends
-- @
--
-- @
-- [nix-shell:~\/arrayfire]$ .\/main
-- [CPU,OpenCL]
-- @
--------------------------------------------------------------------------------
module ArrayFire.Types
  ( AFException         (..)
  , AFExceptionType     (..)
  , Array
  , Window
  , RandomEngine
  , Features
  , AFType              (..)
  , TopK                (..)
  , Backend             (..)
  , MatchType           (..)
  , BinaryOp            (..)
  , MatProp             (..)
  , HomographyType      (..)
  , RandomEngineType    (..)
  , Cell                (..)
  , MarkerType          (..)
  , InterpType          (..)
  , Connectivity        (..)
  , CSpace              (..)
  , YccStd              (..)
  , MomentType          (..)
  , CannyThreshold      (..)
  , FluxFunction        (..)
  , DiffusionEq         (..)
  , IterativeDeconvAlgo (..)
  , InverseDeconvAlgo   (..)
  , Seq                 (..)
  , Index               (..)
  , NormType            (..)
  , ConvMode            (..)
  , ConvDomain          (..)
  , BorderType          (..)
  , Storage             (..)
  , AFDType             (..)
  ) where

import ArrayFire.Exception
import ArrayFire.Internal.Types
