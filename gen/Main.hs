{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import           Control.Applicative
import           Control.Monad
import           Data.Attoparsec.Text
import qualified Data.Attoparsec.Text as A
import           Data.Char
import           Data.Maybe
import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Data.Text.IO         as T
import           System.Environment
import           Text.Printf

main :: IO ()
main = do
  arg <- reverse . Prelude.takeWhile (/='/') . drop 2 . reverse
       . fromMaybe (error "Please enter C header file")
       . listToMaybe <$> getArgs
  ls <- T.lines <$> T.readFile arg
  forM_ ls $ \input ->
    unless (T.null input || "#include" `T.isInfixOf` input) $ do
      result <- either error genBinding (parseInput parser input)
      T.writeFile (path arg) (file arg <> result)

path :: String -> String
path s = printf "src/Data/Array/Fire/Internal/%s.hsc" s

file :: String -> Text
file a = T.pack $ printf
  "module Data.Array.Fire.Internal.%s where\n\n\
  \import Data.Array.Fire.Internal.Defines\n\n\
  \#include \"%s.h\"\n\n\
  \import Foreign.Ptr\n\n" (capitalName a) (lowerCase a)

capitalName, lowerCase :: [Char] -> [Char]
capitalName (x:xs) = toUpper x : xs
lowerCase (x:xs) = toLower x : xs

type Output = Name

newtype Name = Name Text
  deriving (Show, Eq, PrintfArg)

data AST = AST Output Name Params
  deriving (Show)

type Params = [Param]

data Param = Param Type Name
  deriving (Show)

type IsPtr = Bool

data Type = Type IsPtr TypeValue
  deriving (Show)

newtype TypeValue = TypeName Text
  deriving (Show)

parser :: Parser AST
parser = do
  whitespace
  a <- AST <$> parseOutput
           <*> parseName
           <*> parseParams
  whitespace
  pure a

whitespace = many (char ' ')

parseOutput :: Parser Output
parseOutput = do
  result <- string "AFAPI af_err"
  whitespace
  pure (Name "AFError")

parseName :: Parser Name
parseName = do
  result <- A.takeWhile (/='(')
  pure $ Name $ T.strip result

parseParams :: Parser Params
parseParams = do
  char '('
  params <- parseParam `A.sepBy1` (char ',' >> whitespace)
  char ')'
  char ';'
  pure params

parseParam :: Parser Param
parseParam = do
  parseModifier
  type' <- parseType
  name <- parseParamName
  pure $ Param type' name
    where
      parseModifier = do
        r <- (Just <$> string "const") <|> pure Nothing
        whitespace
        pure ()

parseParamName =
  Name <$> A.takeWhile (`notElem` (",)" :: String))

parseType = do
  typeValue <- getTypeValue
  isPtr <- (True <$ char '*') <|> pure False
  pure $ Type isPtr typeValue

getTypeValue = do
  name <- A.takeWhile (`notElem` (" " :: String))
  whitespace
  pure $ TypeName (camelize name)

camelize :: Text -> Text
camelize = T.concat . map go . T.splitOn "_"
  where
    go "af" = "AF"
    go xs = T.cons (toUpper c) cs
      where
        c = T.head xs
        cs = T.tail xs

parseInput :: Parser AST -> Text -> Either String AST
parseInput = parseOnly

genBinding :: AST -> IO Text
genBinding (AST (Name output) name params) =
    pure (header <> dumpBody <> dumpOutput)
  where
    dumpOutput = "IO " <> output
    header = T.pack $ printf "foreign import ccall unsafe \"%s\"\n    %s :: " name name
    dumpBody = T.concat $ map toParam params
      where
        toParam (Param (Type True (TypeName t)) _) = "Ptr " <> t <> " -> "
        toParam (Param (Type False (TypeName t)) _) = t <> " -> "

-- test :: Text
-- test =
--   "AFAPI af_err af_stdev(af_array *out, const af_array in, const dim_t dim);"
