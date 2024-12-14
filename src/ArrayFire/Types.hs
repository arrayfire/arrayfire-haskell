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
-- Maintainer  : David Johnson <code@dmj.io>
-- Stability   : Experimental
-- Portability : GHC
--
-- Various Types related to the ArrayFire API
--
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
  , AFDtype             (..)
  , ColorMap            (..)
  ) where

import ArrayFire.Exception
import ArrayFire.Internal.Types
import ArrayFire.Internal.Defines
