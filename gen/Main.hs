{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import           Control.Monad
import           Data.Char
import           Data.Either

import           Data.Maybe
import           Data.Text             (Text)
import qualified Data.Text             as T
import qualified Data.Text.IO          as T
import           System.Directory
import           System.Environment
import           System.Exit
import           Text.Printf

import           Lex
import           Parse
import           Print
import           Types

main :: IO ()
main = mapM_ writeToDisk =<< getDirectoryFiles

getDirectoryFiles :: IO [String]
getDirectoryFiles = do
  filter (`notElem` exclude) <$> listDirectory "include"
    where
      exclude = [ "defines.h"
                , "complex.h"
                , "extra.h"
                ]

writeToDisk :: String -> IO ()
writeToDisk fileName = do
  bindings
    <- map run
     . drop 1
     . filter (not . T.null)
     . T.lines <$> T.readFile ("include/" <> fileName)
  case partitionEithers bindings of
    (failures, successes) -> do
      if length failures > 0
        then do
          mapM_ print (listToMaybe failures)
          printf "%s failed to generate bindings\n" fileName
        else do
          let name = makeName (reverse . drop 2 . reverse $ fileName)
          T.writeFile (makePath name) $
            file name <> T.intercalate "\n" (genBinding <$> successes)
          printf "Wrote bindings to %s\n" (makePath name)

-- | Filename remappings
makeName :: String -> String
makeName n
  | n == "lapack" = "LAPACK"
  | n == "blas" = "BLAS"
  | n == "cuda" = "CUDA"
  | otherwise = n

makePath :: String -> String
makePath s =
  printf "src/ArrayFire/Internal/%s.hsc" (capitalName (makeName s))

file :: String -> Text
file a = T.pack $ printf
  "{-# LANGUAGE CPP #-}\n\
  \module ArrayFire.Internal.%s where\n\n\
  \import ArrayFire.Internal.Defines\n\
  \import ArrayFire.Internal.Types\n\
  \import Foreign.Ptr\n\
  \import Foreign.C.Types\n\n\
  \#include \"af/%s.h\"\n\
  \" (capitalName a) (if lowerCase a == "exception"
                        then "defines"
                        else lowerCase a)

capitalName, lowerCase :: [Char] -> [Char]
capitalName (x:xs) = toUpper x : xs
lowerCase (x:xs) = map toLower (x:xs)
