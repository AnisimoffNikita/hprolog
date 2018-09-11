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
  = Tree | Result
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
        Tree -> printTree file output

printResult :: FilePath -> IO()
printResult filepath = do
  x <- readProgram filepath
  case x of
    Left err -> putStrLn err
    Right program -> do
      result <- return $ search program
      print result

printTree :: FilePath -> FilePath -> IO()
printTree filepath image = do
  x <- readProgram filepath
  case x of
    Left err -> putStrLn err
    Right program -> do
      result <- return $ search_ program
      createTree result image
      return ()
