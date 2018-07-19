module Language.Prolog.IO 
where 

import qualified Language.Prolog.Syntax        as Syntax
import           Language.Prolog.Parser
import           Text.ParserCombinators.Parsec (runParser, char)

import           Debug.Trace
import           System.IO


readClauses :: FilePath -> IO (Either String Syntax.Program)
readClauses filename = do
  text <- readFile filename
  let parsed = runParser parseProgram () "" text
  case parsed of 
    (Right clauses) -> return . Right $ clauses 
    (Left err) -> return . Left $ show err
  
getQuestion :: String -> IO (Either String [Syntax.Term])
getQuestion question = do
  let parsed = runParser parseBody () "" question
  case parsed of 
    (Right question) -> return . Right $ question 
    (Left err) -> return . Left $ show err