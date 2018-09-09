{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}
module Language.Prolog
  ( module X
  , run
  ) where

import Language.Prolog.Algorithm as X
import Language.Prolog.IO as X
import System.Console.CmdArgs
import System.IO
import Debug.Trace

data PrologMode
  = Tree | Result | SteppedResult
  deriving (Show, Data, Typeable)

data Prolog = Prolog
  { file :: FilePath
  , mode :: PrologMode
  , output :: FilePath
  } deriving (Show, Data, Typeable)

config :: Prolog
config = Prolog
  { file = def &= args &= typFile
  , mode = Result &= typ "MODE" &= help "result format"
  , output = "tmp" &= typFile &= help "tree image path"}

run :: IO ()
run = do
  Prolog{..} <- cmdArgs config
  if file == ""
    then putStrLn "file not specified"
    else
      case mode of
        Result -> printResult file
        SteppedResult -> printSteppedResult file
        Tree -> printTree file output

runTest :: IO()
runTest = do

  let
    file = "p6.pro"
    mode = Tree
    output = "test.t"
  if file == ""
    then putStrLn "file not specified"
    else
      case mode of
        Result -> printResult file
        SteppedResult -> printSteppedResult file
        Tree -> printTree file output

printResult :: FilePath -> IO()
printResult filepath = do
  x <- readProgram filepath
  case x of
    Left err -> putStrLn err
    Right program -> do
      result <- return $ search program
      print result

printSteppedResult :: FilePath -> IO ()
printSteppedResult filepath = do
  x <- readProgram filepath
  case x of
    Left err -> putStrLn err
    Right program -> do
      let result = search program
      hSetBuffering stdin NoBuffering
      printNextResult result
  where
    printNextResult [] = putStrLn "done"
    printNextResult r@(x:xs) = do
      print x
      putStrLn "press n for the next result"
      putStrLn "press s to stop"
      c <- getChar
      putStrLn ""
      case c of
        'n' -> printNextResult xs
        's' -> putStrLn "done"

printTree :: FilePath -> FilePath -> IO()
printTree filepath image = do
  x <- readProgram filepath
  case x of
    Left err -> putStrLn err
    Right program -> do
      result <- return $ search_ program
      print result
      createTree result image
      return ()
