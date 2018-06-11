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
  a <- many alphaNum
  char '\''
  return $ Symbolic a

parseSpecial :: Parser Atom
parseSpecial = Symbolic <$> many1 (oneOf "+-*/\\^~:.?#$&")

parseListAtom :: Parser Atom
parseListAtom = Symbolic <$> string "[]"

parseAtom' :: Parser Atom 
parseAtom' = parseSymbolic <|> parseQuoted <|> parseSpecial <|> parseListAtom

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
parseString = lexeme $ toTerm <$> between (char '"')(char '"') (many alphaNum)
  where 
    toTerm [] = AtomTerm $ Symbolic "[]"
    toTerm (x:[]) = CompoundTerm (Symbolic ".") [AtomTerm $ Symbolic [x], AtomTerm $ Symbolic "[]"]
    toTerm (x:xs) = CompoundTerm (Symbolic ".") [AtomTerm $ Symbolic [x], toTerm xs]

parseList :: Parser Term 
parseList = lexeme $ do 
  lexeme $ char '['

  a <- parseArgs
  b <- option (AtomTerm $ Symbolic "[]") $ (lexeme $ char '|') >> parseTerm'
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


parseBody :: Parser Body 
parseBody = parseDisjunctive <|> parseParentheses

parseParentheses :: Parser Body 
parseParentheses = lexeme $ between (lexeme $ char '(') (char ')') parseBody

parseDisjunctive :: Parser Body 
parseDisjunctive = lexeme $ do 
  a <- sepBy1 (parseConjuctive <|> parseParentheses) (lexeme $ char ';') 
  return $ Disjunctive a

parseConjuctive :: Parser Body 
parseConjuctive = lexeme $ do 
  a <- sepBy1 (parseTerm <|> parseParentheses) (lexeme $ char ',')
  return $ Conjunctive a

parseTerm :: Parser Body 
parseTerm = Term <$> parseTerm'

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