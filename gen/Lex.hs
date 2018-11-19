{-# LANGUAGE OverloadedStrings #-}
module Lex where

import           Control.Arrow
import           Data.Text (Text)
import qualified Data.Text as T

import           Data.Char
import           Types

symbols :: String
symbols = " *();,"

lex :: Text -> [Token]
lex = go NameMode
  where
    tokenize ' ' = []
    tokenize '*' = [Star]
    tokenize '(' = [LParen]
    tokenize ')' = [RParen]
    tokenize ';' = [Semi]
    tokenize ',' = [Comma]
    tokenize _ = []
    go TokenMode xs = do
      case T.uncons xs of
        Nothing -> []
        Just (c,cs)
          | isAlpha c -> go NameMode (T.cons c cs)
          | otherwise -> tokenize c ++ go TokenMode cs
    go NameMode xs = do
      let (match, rest) = partition xs
      if match == "const"
        then [] ++ go TokenMode rest
        else Id match : go TokenMode rest

partition :: Text -> (Text,Text)
partition =
  T.takeWhile (`notElem` symbols) &&&
    T.dropWhile (`notElem` symbols)
