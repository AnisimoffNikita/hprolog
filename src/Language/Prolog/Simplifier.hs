module Language.Prolog.Simplifier where 

import Language.Prolog.Syntax

simplify :: Body -> Body
simplify (Conjunctive [x]) = simplify x
simplify (Disjunctive [x]) = simplify x
simplify x = x