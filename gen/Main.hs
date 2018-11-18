{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import           Control.Arrow
import           Control.Concurrent
import           Control.Monad
import           Data.Char
import           Data.Either
import           Data.Functor.Identity
import           Data.Maybe
import           Data.Text             (Text)
import qualified Data.Text             as T
import qualified Data.Text.IO          as T
import           Prelude               hiding (lex)
import           System.Directory
import           System.Environment
import           System.Exit
import           Text.Parsec
import           Text.Printf

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
     . drop 2
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

makeName :: String -> String
makeName n
  | n == "lapack" = "LAPACK"
  | n == "blas" = "BLAS"
  | n == "cuda" = "CUDA"
  | otherwise = n

makePath :: String -> String
makePath s = printf "src/ArrayFire/Internal/%s.hsc" (capitalName s)

file :: String -> Text
file a = T.pack $ printf
  "module ArrayFire.Internal.%s where\n\n\
  \import ArrayFire.Internal.Defines\n\n\
  \import Data.Word\n\n\
  \import Data.Int\n\n\
  \#include \"%s.h\"\n\n\
  \#include \"extra.h\"\n\n\
  \import Foreign.Ptr\n\n" (capitalName a) (lowerCase a)

capitalName, lowerCase :: [Char] -> [Char]
capitalName (x:xs) = toUpper x : xs
lowerCase (x:xs) = map toLower (x:xs)

camelize :: Text -> Text
camelize = T.concat . map go . T.splitOn "_"
  where
    go "af" = "AF"
    go xs = T.cons (toUpper c) cs
      where
        c = T.head xs
        cs = T.tail xs

isPtr (Type _ x) = x > 0

genBinding :: AST -> Text
genBinding (AST type' name params) =
    header <> dumpBody <> dumpOutput
  where
    dumpOutput | isPtr type' = "IO (" <> printType type' <> ")"
               | otherwise = "IO " <> printType type'
    header = T.pack $ printf "foreign import ccall unsafe \"%s\"\n    %s :: " name name
    dumpBody = printTypes params

printTypes :: [Type] -> Text
printTypes [] = mempty
printTypes [x] = printType x <> " -> "
printTypes (x:xs) =
  mconcat [
    printType x
  , " -> "
  , printTypes xs
  ]

printType (Type (Name x) 0) = showType x
printType (Type (Name x) 1) = "Ptr " <> showType x
printType (Type t n) = "Ptr (" <> printType (Type t (n-1)) <> ")"

showType :: Text -> Text
showType "void"     = "()"
showType "unsigned" = "Word32"
showType "dim_t"    = "Word64"
showType "size_t"   = "Word"
showType "uintl"    = "Word64"
showType "intl"     = "Int64"
showType x = camelize x

foo = "AFAPI af_err af_stdev(af_array *out, const af_array in, const dim_t dim);"

data Token
  = Id Text
  | Star
  | LParen
  | RParen
  | Comma
  | Semi
  deriving (Show, Eq)

symbols :: String
symbols = " *();,"

data Mode
  = TokenMode
  | NameMode
  deriving (Eq, Show)

f = mapM_ print $ lex
 "AFAPI af_err af_create_indexers(af_index_t** indexers);"

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

type Parser = ParsecT [Token] () Identity

type Params = [Type]

data AST = AST Type Name [Type]
  deriving (Show)

data Type = Type Name Int
  deriving (Show)

newtype Name = Name Text
  deriving (Show, Eq, PrintfArg)

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
