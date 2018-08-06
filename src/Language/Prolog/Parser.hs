module Language.Prolog.Parser where

import qualified Text.Parsec                   as P
import qualified Text.Parsec.Char              as P
import           Text.Parsec.Expr               ( Operator(..)
                                                , Assoc(..)
                                                , buildExpressionParser
                                                )
import           Text.Parsec.Token              ( TokenParser(..)
                                                , makeTokenParser
                                                )
import           Text.ParserCombinators.Parsec
import           Text.Parsec                    ( Parsec )
import qualified Data.Map                      as M
import           Control.Monad                  ( void )


import           Language.Prolog.Syntax
import           Language.Prolog.Simplifier

-- NUMBER PARSER


parseInt :: Parser Number
parseInt = do
  a <- integer
  return . Int $ read a

parseFloat :: Parser Number
parseFloat = do
  a <- integer
  b <- decimal
  c <- exponent
  return $ Float $ read (a ++ b ++ c)
 where
  decimal  = char '.' <:> number
  exponent = option "" $ oneOf "eE" <:> integer

parseNumber' :: Parser Number
parseNumber' = try parseFloat <|> parseInt

-- ATOM PARSER

parseSymbolic :: Parser Atom
parseSymbolic = do
  a <- lower
  b <- many (alphaNum <|> char '_')
  return $ Symbolic (a : b)

parseQuoted :: Parser Atom
parseQuoted = do
  char '\''
  a <- many1 alphaNum <|> many1 (oneOf "!+-*/\\^~:.?#$&[]")
  char '\''
  return $ Symbolic a

-- parseSpecial :: Parser Atom
-- parseSpecial = Symbolic <$> many1 (oneOf "+-*/\\^~:.?#$&")

parseListAtom :: Parser Atom
parseListAtom = Symbolic <$> string "[]"

parseAtom' :: Parser Atom
parseAtom' = parseSymbolic <|> parseQuoted <|> parseListAtom

-- VARIABLE PARSER

parseNamed :: Parser Variable
parseNamed = do
  a <- upper
  b <- many (alphaNum <|> char '_')
  let var = a : b
  return $ Named var

parseAnonymous :: Parser Variable
parseAnonymous = char '_' >> return Anonymous

parseVariable' :: Parser Variable
parseVariable' = parseNamed <|> parseAnonymous


-- TERM PARSER

parseAtom :: Parser Term
parseAtom = lexeme $ AtomTerm <$> parseAtom'

parseNumber :: Parser Term
parseNumber = lexeme $ NumberTerm <$> parseNumber'

parseVariable :: Parser Term
parseVariable = lexeme $ VariableTerm <$> parseVariable'

parseCompound :: Parser Term
parseCompound = lexeme $ do
  a <- parseAtom'
  b <- between (char '(' >> whitespace) (char ')') parseArgs
  return $ CompoundTerm a b

parseArgs :: Parser [Term]
parseArgs = lexeme $ sepBy parseTerm' (lexeme $ char ',')

parseString :: Parser Term
parseString = lexeme $ toTerm <$> between (char '"') (char '"') (many alphaNum)
 where
  toTerm []  = AtomTerm $ Symbolic "[]"
  toTerm [x] = CompoundTerm
    (Symbolic "!")
    [AtomTerm $ Symbolic [x], AtomTerm $ Symbolic "[]"]
  toTerm (x : xs) =
    CompoundTerm (Symbolic "!") [AtomTerm $ Symbolic [x], toTerm xs]



parseList :: Parser Term
parseList = lexeme $ do
  lexeme $ char '['

  a <- parseArgs
  b <- option (AtomTerm $ Symbolic "[]") $ (lexeme $ char '|') >> parseTerm'
  char ']'
  return $ toTerm a b
 where
  toTerm []       z = z
  toTerm [x     ] z = CompoundTerm (Symbolic "!") [x, z]
  toTerm (x : xs) z = CompoundTerm (Symbolic "!") [x, toTerm xs z]


parseCut :: Parser Term
parseCut = char '!' >> return Cut

parseTerm' :: Parser Term
parseTerm' =
  lexeme
    $   try parseCompound
    <|> try parseList
    <|> parseAtom
    <|> parseNumber
    <|> parseVariable
    <|> parseString
    <|> parseCut

-- CLAUSE PARSER

parseFact :: Parser Clause
parseFact = do
  a <- parseTerm'
  char '.'
  return $ Fact a

parseRule :: Parser Clause
parseRule = do
  a <- parseTerm'
  lexeme $ string ":-"
  b <- parseBody
  char '.'
  return $ Rule a b


parseClause' :: Parser Clause
parseClause' = lexeme $ do
  a <- try parseRule <|> parseFact
  return a

-- BODY PARSE


parseBody :: Parser [Term]
parseBody = lexeme $ sepBy parseTarget (lexeme $ char ',')



parseTerm :: Parser Term
parseTerm = lexeme parseTerm'

parseTarget :: Parser Term
parseTarget = buildExpressionParser table term

term :: Parser Term
term = P.between (char '(' >> whitespace) (char ')') parseTarget <|> parseTerm

table =
  [ [ makeOperator Infix AssocLeft "\\"
    , makeOperator Infix AssocLeft "^"
    , makeOperator Infix AssocLeft "**"
    ]
  , [ makeOperator Infix AssocLeft "*"
    , makeOperator Infix AssocLeft "/"
    , makeOperator Infix AssocLeft "rem"
    , makeOperator Infix AssocLeft "mod"
    ]
  , [makeOperator Infix AssocLeft "+", makeOperator Infix AssocLeft "-"]
  , [ makeOperator Infix AssocLeft "="
    , makeOperator Infix AssocLeft "is"
    , makeOperator Infix AssocLeft "=="
    , makeOperator Infix AssocLeft "\\="
    , makeOperator Infix AssocLeft "<"
    , makeOperator Infix AssocLeft "=<"
    , makeOperator Infix AssocLeft ">"
    , makeOperator Infix AssocLeft ">="
    ]
  ]

makeOperator op assoc name = op
  (makeFunc name <$ try (string name >> whitespace))
  assoc
  where makeFunc name a b = CompoundTerm (Symbolic name) [a, b]

-- parseBody :: Parser Body 
-- parseBody = parseDisjunctive <|> parseParentheses

-- parseParentheses :: Parser Body 
-- parseParentheses = lexeme $ between (lexeme $ char '(') (char ')') parseBody

-- parseDisjunctive :: Parser Body 
-- parseDisjunctive = lexeme $ do 
--   a <- sepBy1 (parseConjuctive <|> parseParentheses) (lexeme $ char ';') 
--   return $ Disjunctive a

-- parseConjuctive :: Parser Body 
-- parseConjuctive = lexeme $ do 
--   a <- sepBy1 (parseTerm <|> parseParentheses) (lexeme $ char ',')
--   return $ Conjunctive a

-- parseTerm :: Parser Body 
-- parseTerm = Element <$> parseTerm'

-- PARSER PROGRAM

parseProgram :: Parser Program
parseProgram = do
  p <- lexeme $ many parseClause'
  return $ Program p []


parseProgram_ :: Parser Program
parseProgram_ = do
  lexeme $ string "predicates"
  p <- lexeme $ many (try parseClause')
  lexeme $ string "goal"
  q <- lexeme $ many1 parseTerm'
  lexeme $ char '.'
  return $ Program p q

-- HELPERS

whitespace :: Parser ()
whitespace = void $ many $ oneOf " \n\t"

lexeme :: Parser a -> Parser a
lexeme p = p <* whitespace

(<:>) a b = (:) <$> a <*> b

plus = char '+' >> number
minus = char '-' <:> number
number = many1 digit
integer = plus <|> minus <|> number
