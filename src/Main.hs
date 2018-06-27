module Main where

import Text.ParserCombinators.Parsec
import Text.Parsec (ParsecT, State)
import qualified Data.Map as M
import           Control.Monad.Writer
import           Control.Monad.State
-- import           Control.Monad.Trans.State
import           Control.Monad.Trans.Maybe

import qualified Prolog.Syntax as Syn
import Prolog.Semantics
import Prolog.Parser
import Prolog.Unification

main :: IO ()
main = print "!"


s = "f(1,1):-!.f(N,X):-dec(N,M),f(M,Y),mult(Y,N,X)."

Right a = runParser parseProgram () "" s
Right q = runParser parseBody  () "" "append([1,2],[1,2],X)"
Right q' = runParser parseTerm'  () "" "f(3,R)"

Syn.Program cl = a 
a' = Syn.Program $ predefinedDec ++ predefinedMult ++ cl



predefinedMult = concatMap
  (\x -> map
    (\y -> Syn.Fact
      (Syn.CompoundTerm
        (Syn.Symbolic "mult")
        [Syn.NumberTerm (Syn.Int y), Syn.NumberTerm (Syn.Int x), Syn.NumberTerm (Syn.Int (y * x))]
      )
    )
    [0 .. 10]
  )
  [0 .. 10]

predefinedDec = map
  (\x -> Syn.Fact
    (Syn.CompoundTerm (Syn.Symbolic "dec")
                  [Syn.NumberTerm (Syn.Int x), Syn.NumberTerm (Syn.Int (x - 1))]
    )
  )
  [0 .. 10]
