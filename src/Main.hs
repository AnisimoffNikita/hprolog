module Main where

import Prolog.Syntax
import Prolog.Parser.Term
-- import Prolog.Parser

-- program = 
--   Program 
--     [ Clause (CompoundTerm (Symbolic "f") [Const (Number (Int 1)), Variable (Named  "X" 0)] ) 
--       (Or [])
--     , Clause (CompoundTerm (Symbolic "f") [Variable (Named "N" 0), Variable (Named "X" 0)] ) 
--       (Or 
--         [ And
--           [ CompoundTerm (Symbolic "dec") [Variable (Named "N" 0), Variable (Named "NewN" 0)]
--           , CompoundTerm (Symbolic "f") [Variable (Named "N" 0), Variable (Named "NewRes" 0)]
--           , CompoundTerm (Symbolic "mul") [Variable (Named "N" 0), Variable (Named  "NewRes" 0)]
--           ]
--         ])
--     ]

main :: IO ()
main = print "!"
