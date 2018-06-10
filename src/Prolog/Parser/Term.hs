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
  char '['

  char '|'

  char ']'

  undefined

parseTerm' :: Parser Term 
parseTerm' = try parseCompound <|> parseAtom <|> parseNumber <|> parseVariable <|> parseString <|> parseList


-- PREDICATE PARSER

parsePredicate'' :: Parser Predicate
parsePredicate'' = do 
  a <- parseAtom'
  b <- parseArgs 
  return $ Predicate a b

parseCut :: Parser Predicate 
parseCut = char '!' >> return Cut

parsePredicate' :: Parser Predicate 
parsePredicate' = lexeme $ parsePredicate'' <|> parseCut 

-- CLAUSE PARSER

parseFact :: Parser Clause 
parseFact = do 
  a <- parsePredicate'
  char '.'
  return $ Fact a

parseRule :: Parser Clause 
parseRule =  do 
  a <- parsePredicate'
  lexeme $ string ":-"
  b <- parseBody
  char '.'
  return $ Rule a b

parseClause' :: Parser Clause
parseClause' = lexeme $ parseRule <|> parseFact 

-- BODY PARSE

parseBody = expr

parsePredicate :: Parser Predicate 
parsePredicate = parsePredicate'

expr :: Parser Predicate
expr = buildExpressionParser table term

term :: Parser Predicate
term = (P.between (char '(' >> whitespace) (char ')') expr) <|> parsePredicate

-- difficult type :(
table = [ [makeOperator Infix AssocLeft ","]
        ]

makeOperator op assoc name = op (makeFunc name <$ string name) assoc
  where 
    makeFunc name a b = Predicate (Symbolic name) [a, b]

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