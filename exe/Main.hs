{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE DataKinds           #-}
module Main where

import ArrayFire
import Control.Concurrent
import Control.Exception

import Prelude            hiding (sum, product)

main :: IO ()
main = print newArray `catch` (\(e :: AFException) -> print e)
  where
    newArray = matrix @Double (2,2) [ [1..], [1..] ] * matrix @Double (2,2) [ [2..], [2..] ]
