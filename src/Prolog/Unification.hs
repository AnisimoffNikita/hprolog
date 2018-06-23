module Prolog.Unification where

import           Prolog.Syntax
import           Data.List                      ( find )
import           Control.Monad                  ( zipWithM )


unification :: Term -> Term -> Maybe [(Term, Term)]
unification t1 t2 = case (t1, t2) of
  (VariableTerm Anonymous    , _                         ) -> Just []
  (VariableTerm (Named n  i ), AtomTerm x                ) -> Just [(t1, t2)]
  (VariableTerm (Named n  i ), NumberTerm x              ) -> Just [(t1, t2)]
  (VariableTerm (Named n1 i1), VariableTerm (Named n2 i2)) -> Just [(t1, t2)]
  (_                         , VariableTerm (Named n i)  ) -> unification t2 t1
  (VariableTerm (Named n i), CompoundTerm f args) -> case find (f i) args of
    Just _  -> Nothing
    Nothing -> Just [(t1, t2)]
   where
    f i (VariableTerm (Named _ j)) = i == j
    f i _                          = False

  (CompoundTerm f1 args1, CompoundTerm f2 args2) ->
    if f1 == f2 && length args1 == length args2
      then concat <$> zipWithM unification args1 args2
      else Nothing
  (t1, t2) -> if t1 == t2 then Just [] else Nothing
