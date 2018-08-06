{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}
module Main where

import Language.Prolog
import System.Console.CmdArgs
import System.Timeout

data PrologMode 
  = Tree | Result | SteppedResult
  deriving (Show, Data, Typeable)

data Prolog = Prolog 
  { file :: FilePath
  , timeout_ :: Int 
  , mode :: PrologMode
  , output :: FilePath
  } deriving (Show, Data, Typeable)

config = Prolog
  { file = def &= args &= typFile
  , timeout_ = 60 &= typ "NUM" &= help "timeout"
  , mode = Result &= typ "MODE" &= help "result format"  
  , output = "tmp" &= typFile &= help "tree image path"}

main = do 
  Prolog{..} <- cmdArgs config
  if file == "" 
    then putStrLn "file not specified"
    else 
      case mode of 
        Result -> printResult file timeout_
        SteppedResult -> printSteppedResult file timeout_
        Tree -> printTree file output timeout_

printResult :: FilePath -> Int -> IO()
printResult filepath to = do 
  x <- readProgram filepath
  case x of 
    Left err -> putStrLn err
    Right program -> do 
      x <- return $ search program
      print x 
      -- case x of 
      --   Just result -> print result 
      --   Nothing -> putStrLn "time is out"

printSteppedResult :: FilePath -> Int -> IO ()
printSteppedResult filepath to = do 
  x <- readProgram filepath
  case x of 
    Left err -> putStrLn err
    Right program -> do 
      let result = search program
      printNextResult result 
  where 
    printNextResult [] = putStrLn "done"
    printNextResult r@(x:xs) = do 
      print x
      putStrLn "press n for the next result"
      putStrLn "press s to stop"
      c <- getChar
      case c of 
        'n' -> printNextResult xs
        's' -> putStrLn "done"
        _ -> printNextResult r



printTree :: FilePath -> FilePath -> Int -> IO()
printTree filepath image to = do 
  x <- readProgram filepath
  case x of 
    Left err -> putStrLn err
    Right program -> do 
      x <- return $ search_ program
      print x
      -- case x of 
      --   Just result -> createTree result image >> return ()
      --   Nothing -> putStrLn "time is out"
    
      



