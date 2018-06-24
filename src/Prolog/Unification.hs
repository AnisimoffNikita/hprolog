{-# LANGUAGE TypeOperators #-}
module Prolog.Unification where

import           Prolog.Syntax
import           Data.List                      ( find )
import           Control.Monad                  ( zipWithM )
import           Debug.Trace

type Checks = [Check]
type Results = [Result]

data Result = Term := Term deriving (Show)
data Check = Term :? Term deriving (Show)

unification :: Checks -> Results -> Maybe Results
unification []              work = Just work
unification (t :? p : rest) work = do
  (stack', work') <- unification' t p
  case work' of
    Just (t := p) -> do
      updatedStack <- updateEquals t p rest
      updatedWork  <- updateResult t p work
      unification updatedStack (t := p : updatedWork)
    Nothing -> unification (stack' ++ rest) work

updateEquals :: Term -> Term -> Checks -> Maybe Checks
updateEquals t p equals = Just $ map f equals
  where f (t' :? p') = replaceOccurrence t p t' :? replaceOccurrence t p p'

updateResult :: Term -> Term -> Results -> Maybe Results
updateResult t p results = concat <$> mapM f results
 where
  f x@(t' := p') =
    let t'' = replaceOccurrence t p t'
        p'' = replaceOccurrence t p p'
    in  if t' == t'' && p' == p'' then Just [x] else unification [t'' :? p''] []

replaceOccurrence :: Term -> Term -> Term -> Term
replaceOccurrence t q (CompoundTerm f args) = CompoundTerm f (map update args)
 where
  update p@(CompoundTerm f args) = replaceOccurrence t q p
  update p                       = if t == p then q else p
replaceOccurrence t q p = if t == p then q else p

unification' :: Term -> Term -> Maybe (Checks, Maybe Result)
unification' t p = case (t, p) of
  (Cut                     , _           ) -> Just ([], Nothing)
  (VariableTerm Anonymous  , _           ) -> Just ([], Nothing)
  (VariableTerm (Named n i), AtomTerm x  ) -> Just ([], Just (t := p))
  (VariableTerm (Named n i), NumberTerm x) -> Just ([], Just (t := p))
  (VariableTerm (Named n1 i1), VariableTerm (Named n2 i2)) ->
    Just ([], Just (t := p))
  (_, VariableTerm (Named n i)) -> unification' p t
  (VariableTerm (Named n i), CompoundTerm f args) -> case find (f i) args of
    Just _  -> Nothing
    Nothing -> Just ([], Just (t := p))
   where
    f i (VariableTerm (Named _ j)) = i == j
    f i _                          = False
  (CompoundTerm f1 args1, CompoundTerm f2 args2) ->
    if f1 == f2 && length args1 == length args2
      then Just (zipWith (:?) args1 args2, Nothing)
      else Nothing
  (t, p) -> if t == p then Just ([], Nothing) else Nothing


search :: Program -> Question -> Maybe Results 
search (Program clauses) = do 
  undefined
