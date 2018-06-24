module Prolog.Simplifier where 

import Prolog.Syntax

simplify :: Body -> Body
simplify (Conjunctive [x]) = simplify x
simplify (Disjunctive [x]) = simplify x
simplify x = x