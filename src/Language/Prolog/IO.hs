module Language.Prolog.IO 
  where 

import qualified Language.Prolog.Syntax        as Syntax
import           Language.Prolog.Parser
import           Language.Prolog.Algorithm (SearchTree)
import           Text.ParserCombinators.Parsec (runParser, char)
import           Language.Prolog.Printer.Graphviz

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


readProgram :: FilePath -> IO (Either String Syntax.Program)
readProgram filename = do
  text <- readFile filename
  let parsed = runParser parseProgram_ () "" text
  case parsed of 
    (Right clauses) -> return $ Right clauses
    (Left err) -> return . Left $ show err


resultRead :: FilePath -> IO ()
resultRead filename = do 
  x <- readProgram filename
  case x of 
    Left err -> putStrLn err 
    _ -> putStrLn "ok"


createTree :: SearchTree -> FilePath -> IO ()
createTree tree image = do 
  let dot = showTree tree 
  createImage (image, dot)
  return ()

