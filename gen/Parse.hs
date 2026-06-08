{-# LANGUAGE OverloadedStrings #-}
module Parse where

import           Prelude       hiding (lex)
import           Control.Monad
import           Data.Text     (Text)
import qualified Data.Text     as T
import           Text.Parsec

import           Lex
import           Types

parseAST :: Parser AST
parseAST = do
 afapi
 type' <- getType
 funcName <- name
 lparen
 params <- getParam `sepBy` comma
 rparen >> semi
 pure (AST type' funcName params)

getParam :: Parser Type
getParam = do
  t <- getType
  t <$ name

getType :: Parser Type
getType = do
  typeName <- name
  stars <- msum [ try (rep x) | x <- [3,2..0] ]
  pure (Type typeName stars)
    where
      rep n = replicateM_ n star >> pure n

run :: Text -> Either ParseError AST
run txt = parse parseAST mempty (lex txt)

afapi,lparen,rparen,semi,star,comma :: Parser ()
lparen = tok' LParen
rparen = tok' RParen
afapi  = tok' (Id "AFAPI") <|> pure ()
comma  = tok' Comma
semi   = tok' Semi
star   = tok' Star

tok :: Token -> Parser Token
tok x = tokenPrim show ignore
  (\t -> if x == t then Just x else Nothing)

tok' :: Token -> Parser ()
tok' x = tokenPrim show ignore
  (\t -> if x == t then Just () else Nothing)

name :: Parser Name
name = tokenPrim show ignore go
  where
    go (Id x) = Just (Name x)
    go _ = Nothing

ignore x _ _ = x
