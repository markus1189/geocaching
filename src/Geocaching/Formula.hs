{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Geocaching.Formula (expr) where

import Data.Functor.Identity
import Text.Parsec
import Text.Parsec.Token
import Text.Parsec.Language
import Text.Parsec.Expr
import qualified Text.Parsec.Char as P

expr :: Stream String Identity Char => ParsecT String u Identity Integer
expr = buildExpressionParser table term <?> "expression"

table :: (Integral n, Stream String Identity Char) => OperatorTable String u Identity n
table = [[Infix ((*) <$ reservedOp haskell "*") AssocLeft]]

term :: Stream String Identity Char => ParsecT String u Identity Integer
term = parens haskell expr <|> natural haskell
