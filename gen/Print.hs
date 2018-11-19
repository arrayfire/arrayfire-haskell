{-# LANGUAGE OverloadedStrings #-}
module Print where

import           Data.Char
import           Data.Text   (Text)
import qualified Data.Text   as T
import           Text.Printf

import           Types

import Parse

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

-- | Additional mappings, very important for CodeGen
showType :: Text -> Text
showType "char"     = "CChar"
showType "void"     = "()"
showType "unsigned" = "CUInt"
showType "dim_t"    = "DimT"
showType "af_someenum_t" = "AFSomeEnum"
showType "size_t"   = "CSize"
showType "uintl"    = "UIntL"
showType "intl"     = "IntL"
showType "af_index_t" = "AFIndex"
showType "af_cspace_t" = "AFCSpace"
showType "afcl_platform" = "AFCLPlatform"
showType x = camelize x
