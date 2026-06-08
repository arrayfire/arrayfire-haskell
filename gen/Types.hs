{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Types where

import           Data.Functor.Identity
import           Data.Text             (Text)
import qualified Data.Text             as T
import           Text.Parsec
import           Text.Printf

type Parser = ParsecT [Token] () Identity

type Params = [Type]

data AST = AST Type Name [Type]
  deriving (Show)

data Type = Type Name Int
  deriving (Show)

newtype Name = Name Text
  deriving (Show, Eq, PrintfArg)

data Mode
  = TokenMode
  | NameMode
  deriving (Eq, Show)

data Token
  = Id Text
  | Star
  | LParen
  | RParen
  | Comma
  | Semi
  deriving (Show, Eq)
