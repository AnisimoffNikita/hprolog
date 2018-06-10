module Prolog.Parser.Term
  where 

import Text.ParserCombinators.Parsec

import qualified Text.Parsec as P
import qualified Text.Parsec.Char as P
import Text.Parsec.Expr (Operator(..), Assoc(..), buildExpressionParser)
import Text.Parsec.Token (TokenParser(..), makeTokenParser)
import qualified Text.Parsec.String (Parser)
import Control.Monad (void)

import Prolog.Syntax



-- NUMBER PARSER

parseInt :: Parser Number
parseInt = do 
  a <- plus <|> minus <|> number
  return . Int $ read a
      
parseFloat :: Parser Number
parseFloat = do 
  a <- number
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
  return $ Symbolic (a:b)

parseQuoted :: Parser Atom 
parseQuoted = do 
  char '\''
  a <- many anyChar
  char '\''
  return $ Quoted a

parseSpecial :: Parser Atom
parseSpecial = Special <$> many1 (oneOf "+-*/\\^~:.?#$&")

parseAtom' :: Parser Atom 
parseAtom' = parseSymbolic <|> parseQuoted <|> parseSpecial

-- VARIABLE PARSER

parseNamed :: Parser Variable
parseNamed = do 
  a <- upper
  b <- many (alphaNum <|> char '_')
  return $ Named (a:b)

parseAnonymous :: Parser Variable
parseAnonymous = char '_' >> return Anonymous

parseVariable' :: Parser Variable 
parseVariable' = parseNamed <|> parseAnonymous


-- TERM PARSER

parseAtom :: Parser Term
parseAtom = lexeme $ Atom <$> parseAtom'

parseNumber :: Parser Term
parseNumber = lexeme $ Number <$> parseNumber'

parseVariable :: Parser Term 
parseVariable = lexeme $ Variable <$> parseVariable'

parseCompound :: Parser Term 
parseCompound = lexeme $ do 
  a <- parseAtom'
  b <- between (char '(' >> whitespace) (char ')') parseArgs
  return $ CompoundTerm a b

parseArgs :: Parser [Term]
parseArgs = lexeme $ do  
  arg <- parseTerm'
  args <- parseNextArg
  return (arg:args)

parseNextArg :: Parser [Term]
parseNextArg = (char ',' >> whitespace >> parseArgs) <|> return []


parseString :: Parser Term 
parseString = lexeme $ toTerm <$> between (char '"')(char '"') (many alphaNum)
  where 
    toTerm [] = Atom $ Symbolic "[]"
    toTerm (x:[]) = CompoundTerm (Symbolic ".") [Atom $ Symbolic [x], Atom $ Symbolic "[]"]
    toTerm (x:xs) = CompoundTerm (Symbolic ".") [Atom $ Symbolic [x], toTerm xs]

parseList :: Parser Term 
parseList = lexeme $ do 
  lexeme $ char '['
  
  a <- parseArgs

  lexeme $ char '|'

  b <- parseTerm'

  char ']'

  return $ toTerm a b
  where 
    toTerm [] z = z
    toTerm (x:[]) z = CompoundTerm (Symbolic ".") [x, z]
    toTerm (x:xs) z = CompoundTerm (Symbolic ".") [x, toTerm xs z]


parseCut :: Parser Term 
parseCut = char '!' >> return Cut

parseTerm' :: Parser Term 
parseTerm' = try parseCompound <|> parseAtom <|> parseNumber <|> parseVariable <|> parseString <|> parseList


-- CLAUSE PARSER

parseFact :: Parser Clause 
parseFact = do 
  a <- parseTerm'
  char '.'
  return $ Fact a

parseRule :: Parser Clause 
parseRule =  do 
  a <- parseTerm'
  lexeme $ string ":-"
  b <- parseBody
  char '.'
  return $ Rule a b

parseClause' :: Parser Clause
parseClause' = lexeme $ parseRule <|> parseFact 

-- BODY PARSE

parseBody = expr

parseTerm :: Parser Term 
parseTerm = lexeme $ parseTerm'

expr :: Parser Term
expr = buildExpressionParser table term

term :: Parser Term
term = (P.between (char '(' >> whitespace) (char ')') expr) <|> parseTerm

table = [ [ makeOperator Infix AssocLeft ":"]
        , [ makeOperator Infix AssocLeft "@"]
        , [ makeOperator Infix AssocLeft "\\"
          , makeOperator Infix AssocLeft "^"
          , makeOperator Infix AssocLeft "**"
          ]
        , [ makeOperator Infix AssocLeft "*"
          , makeOperator Infix AssocLeft "/"
          , makeOperator Infix AssocLeft "rem"
          , makeOperator Infix AssocLeft "mod"
          ]
        , [ makeOperator Infix AssocLeft "+"
          , makeOperator Infix AssocLeft "-"
          ]
        , [ makeOperator Infix AssocLeft "="
          , makeOperator Infix AssocLeft "is"
          , makeOperator Infix AssocLeft "=="
          , makeOperator Infix AssocLeft "\\="
          , makeOperator Infix AssocLeft "<"
          , makeOperator Infix AssocLeft "=<"
          , makeOperator Infix AssocLeft ">"
          , makeOperator Infix AssocLeft ">="
          ]
        , [ makeOperator Infix AssocLeft ","]
        , [ makeOperator Infix AssocLeft "->"]
        , [ makeOperator Infix AssocLeft ";"]
        ]

makeOperator op assoc name = op (makeFunc name <$ (try $ string name >> whitespace)) assoc
  where 
    makeFunc name a b = CompoundTerm (Symbolic name) [a, b]

-- PARSER PROGRAM



-- HELPERS

whitespace :: Parser ()
whitespace = void $ many $ oneOf " \n\t"

lexeme :: Parser a -> Parser a
lexeme p = p <* whitespace


(<++>) a b = (++) <$> a <*> b
(<:>) a b = (:) <$> a <*> b

plus   = char '+' >> number
minus  = char '-' <:> number
number = many1 digit
integer = plus <|> minus <|> number