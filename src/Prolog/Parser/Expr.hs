module Prolog.Parser.Expr where

import Control.Applicative((<*))
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language


data Expr = Lit Char | A1 Expr | A2 Expr | B Expr Expr
  deriving (Show)

expr :: Parsec String () Expr
expr = buildExpressionParser table  (fmap Lit digit)

prefix p = Prefix . chainl1 p $ return (.)

table =
  [ [prefix $ (char ',' >> return A1) <|> (char '.' >> return A2)]
  , [Infix   (char '*' >> return B) AssocNone]
  , [prefix $ (char ',' >> return A1)] 
  ]