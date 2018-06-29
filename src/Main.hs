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

main :: IO ()
main = print "!"


s' = "append([], L, L). append([X|L1], L2, [X|L3]):- append(L1, L2, L3)."
s0 = "f(1,1).f(N,X):-dec(N,NN),f(NN,XX),mult(XX,N,X)."

s = "loves(vincent, mia). loves(marcellus, mia). loves(pumpkin, honey_bunny). loves(honey_bunny, pumpkin). jealous(X, Y) :-loves(X, Z),loves(Y, Z)."

f = "f(1,1) :- !.f(N,X):- M is N - 1, f(M,Y), X is Y*N."

Right a = runParser parseProgram () "" f
Right q = runParser parseTerm'  () "" "f(3,X)"

Syn.Program cl = a 
a' = Syn.Program $ cl



u = ConstTerm (Int 4)
i = ConstTerm (Int 3)
o = ConstTerm (Int 5)

p = CompoundTerm "*" [i, o]
l = CompoundTerm "-" [u, p]

-- predefinedMult = concatMap
--   (\x -> map
--     (\y -> Syn.Fact
--       (Syn.CompoundTerm
--         (Syn.Symbolic "mult")
--         [Syn.NumberTerm (Syn.Int y), Syn.NumberTerm (Syn.Int x), Syn.NumberTerm (Syn.Int (y * x))]
--       )
--     )
--     [0 .. 2]
--   )
--   [0 .. 2]

-- predefinedDec = map
--   (\x -> Syn.Fact
--     (Syn.CompoundTerm (Syn.Symbolic "dec")
--                   [Syn.NumberTerm (Syn.Int x), Syn.NumberTerm (Syn.Int (x - 1))]
--     )
--   )
--   [1 .. 2]


--  dot = printer $ search a' [q]