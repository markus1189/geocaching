{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Geocaching.Formula (expr,eval) where

import           Data.Functor.Identity
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
import           Text.Parsec
import qualified Text.Parsec.Char as P
import           Text.Parsec.Expr
import           Text.Parsec.Language
import           Text.Parsec.Token

lang :: TokenParser st
lang = haskell

expr :: Stream String Identity Char => ParsecT String u Identity Expr
expr = buildExpressionParser table term <?> "expression"

table :: OperatorTable String u Identity Expr
table = [[Prefix (Negate <$ reservedOp lang "-")]
        ,[Infix (Mul <$ reservedOp lang "*") AssocLeft, Infix (Div <$ reservedOp lang "/") AssocLeft]
        ,[Infix (Add <$ reservedOp lang "+") AssocLeft, Infix (Sub <$ reservedOp lang "-") AssocLeft]
        ]

term :: Stream String Identity Char => ParsecT String u Identity Expr
term = (parens lang expr <|> brackets lang expr <|> braces lang expr) <|> Lit <$> natural lang <|> variable

variable :: ParsecT String u Identity Expr
variable = Var . pure <$> P.upper

data Expr = Mul Expr Expr
          | Add Expr Expr
          | Sub Expr Expr
          | Div Expr Expr
          | Var String
          | Lit Integer
          | Negate Expr
          deriving (Show,Eq)

eval :: Map String Integer -> Expr -> Either String Integer
eval xs (Mul lhs rhs) = (*) <$> eval xs lhs <*> eval xs rhs
eval xs (Add lhs rhs) = (+) <$> eval xs lhs <*> eval xs rhs
eval xs (Div lhs rhs) = div <$> eval xs lhs <*> eval xs rhs
eval xs (Sub lhs rhs) = (-) <$> eval xs lhs <*> eval xs rhs
eval xs (Negate e) = negate <$> eval xs e
eval _ (Lit i) = pure i
eval xs (Var x) = fromMaybe (Left ("Variable '" ++ x ++ "' not found: " ++ show xs)) $ Right <$> Map.lookup x xs
