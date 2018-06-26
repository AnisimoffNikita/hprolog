module Main where

import Text.ParserCombinators.Parsec
import Text.Parsec (ParsecT, State)
import qualified Data.Map as M
import           Control.Monad.Writer
import           Control.Monad.State
-- import           Control.Monad.Trans.State
import           Control.Monad.Trans.Maybe

import Prolog.Syntax
import Prolog.Parser
import Prolog.Unification

main :: IO ()
main = print "!"


s = "f(1,1):-!.f(N,X):-dec(N,NN),f(NN,XX),mult(XX,N,X)."

Right a = runParser parseProgram  (M.empty, 0) "" s
Right q = runParser parseBody  (M.empty, 0) "" "f(3,X)"
Right q' = runParser parseTerm'  (M.empty, 0) "" "loves(X, mia)"

Program cl = a 
a' = Program $ predefinedDec ++ predefinedMult ++ cl
