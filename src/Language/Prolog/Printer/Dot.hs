module Language.Prolog.Printer.Dot
  where

import Data.List (intercalate)

import Language.Prolog

dotFile :: String -> SearchTree -> IO ()
dotFile fn tree = writeFile fn $ printer tree

printResult :: Substitution -> String
printResult = show

printResolvent :: Resolvent -> String
printResolvent terms = intercalate "\n" $ map show terms

printer :: SearchTree -> String
printer t = "digraph G{\n" ++ printer' t ++ "\n}"

printer' :: SearchTree -> String
printer' t@(Node _ result resolvent trees) = (intercalate "\n" $ map f trees) ++ "\n\n" ++ rest
  where
    header = makeHeader t
    f tree = header ++ "\n->\n" ++ makeHeader tree
    rest :: String
    rest = concatMap printer' trees
printer' (Ok _ result) = "\"OK: " ++ printResult result ++ "\""
printer' (Fail _ ) = ""

makeHeader (Node _  result resolvent trees) = "\"" ++ printResult result ++ "\n\n" ++ printResolvent resolvent ++ "\""
makeHeader (Fail _) = "\"Fail: " -- ++ show t1 ++ "/=" ++ show t2 ++ "\""
makeHeader x = printer' x
