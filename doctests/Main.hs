module Main (main) where

import ArrayFire
import Build_doctests     (flags, pkgs, module_sources)
import System.Environment
import Test.DocTest       (doctest)
import Data.List.Split

main :: IO ()
main = do
  print $ 1 + (1 :: Array Int)
  moreFlags <- drop 1 . splitOn " " <$> getEnv "NIX_TARGET_LDFLAGS"
  mapM_ print moreFlags
  mapM_ print (flags ++ pkgs ++ module_sources ++ moreFlags)
  doctest (moreFlags ++ flags ++ pkgs ++ module_sources)
