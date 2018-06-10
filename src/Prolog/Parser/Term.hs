module Prolog.Parser.Term
  where 

import Text.ParserCombinators.Parsec

import qualified Text.Parsec as P
import qualified Text.Parsec.Char as P
import qualified Text.Parsec.Expr as P
import qualified Text.Parsec.Token as P
import qualified Text.Parsec.String (Parser)

import Prolog.Syntax

(<++>) a b = (++) <$> a <*> b
(<:>) a b = (:) <$> a <*> b

plus   = char '+' >> number
minus  = char '-' <:> number
number = many1 digit
integer = plus <|> minus <|> number

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
parseNumber' = parseFloat <|> parseInt


parseSymbolic :: Parser Atom 
parseSymbolic = do 
  a <- lower
  b <- many (alphaNum <|> char '_')
  return $ Symbolic (a:b)

parseQuoted :: Parser Atom 
parseQuoted = do 
  a <- char '\'' >> many (alphaNum <|> char '_' <|> newline)
  char '\''
  return $ Quoted a


parseAtom' :: Parser Atom 
parseAtom' = parseSymbolic <|> parseQuoted

parseNamed :: Parser Variable
parseNamed = do 
  a <- upper
  b <- many (alphaNum <|> char '_')
  return $ Named (a:b) 0

parseAnonymous :: Parser Variable
parseAnonymous = char '_' >> return Anonymous

parseVariable' :: Parser Variable 
parseVariable' = parseNamed <|> parseAnonymous


parseAtom :: Parser Const
parseAtom = Atom <$> parseAtom'

parseNumber :: Parser Const
parseNumber = Number <$> parseNumber'


parseString :: Parser Const
parseString = do 
  a <- char '\"' >> many anyChar
  char '\"'
  return $ String a

parseConst' :: Parser Const 
parseConst' = parseAtom <|> parseNumber <|> parseString



parseConst :: Parser Term 
parseConst = Const <$> parseConst'

parseVariable :: Parser Term 
parseVariable = Variable <$> parseVariable'

parseCut :: Parser Term 
parseCut = char '!' >> (return Cut)

parseCompound :: Parser Term 
parseCompound = do 
  a <- parseAtom'
  char '('
  b <- parseArgs 
  char ')'
  return $ CompoundTerm a b

parseArgs :: Parser [Term]
parseArgs = do  
  skipMany space
  arg <- try parseCompound <|> parseVariable <|> parseConst
  skipMany space
  args <- parseNextArg
  return (arg:args)

parseNextArg :: Parser [Term]
parseNextArg = (char ',' >> parseArgs) <|> return []

parseTerm' :: Parser Term 
parseTerm' = parseConst <|> parseVariable <|> parseCompound <|> parseCut

