{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Geocaching.Formula (expr,eval) where

import           Control.Applicative (empty)
import           Data.Functor (void)
import           Data.Map (Map)

import           Text.Megaparsec
import           Text.Megaparsec.Text
import           Text.Megaparsec.Expr
import qualified Text.Megaparsec.Lexer as L

data Expr = Mul Expr Expr
          | Add Expr Expr
          | Sub Expr Expr
          | Div Expr Expr
          | Negate Expr
          | Var String
          | Lit Integer
          deriving (Show,Eq)

sc :: Parser ()
sc = L.space (void spaceChar) lineCmnt empty
  where lineCmnt  = L.skipLineComment "#"

symbol :: String -> Parser String
symbol = L.symbol sc

expr :: Parser Expr
expr = sc *> makeExprParser term table <?> "expression"

term :: Parser Expr
term = parens expr <|> (Lit <$> integer) <?> "term"

integer :: Parser Integer
integer = L.lexeme sc L.integer

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

table :: [[Operator Parser Expr]]
table = [[Prefix (Negate <$ symbol "-")]
        ,[InfixL (Mul <$ symbol "*")]
        ,[InfixL (Add <$ symbol "+")]
        ,[InfixL (Sub <$ symbol "-")]
        ,[InfixL (Div <$ symbol "/")]]

eval :: Map String Integer -> Expr -> Either String Integer
eval = undefined
