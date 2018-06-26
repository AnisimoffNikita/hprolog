{-# LANGUAGE TypeOperators #-}
module Prolog.Unification where

import           Prolog.Syntax
import           Data.List                      ( find )
import           Control.Monad                  ( zipWithM )
import           Debug.Trace
import           Control.Monad.Writer
import           Control.Monad.State
-- import           Control.Monad.Trans.State
import           Control.Monad.Trans.Maybe

import Prolog.Simplifier

type Targets = [Target]
type Result = [ResultPair]

data ResultPair = Term := Term deriving (Show, Eq)
data Target = Term :? Term deriving (Show, Eq)



unification :: Targets -> Result -> Maybe Result
unification []              work = Just work
unification (t :? p : rest) work = do
  (stack', work') <- unificateTerms t p
  case work' of
    Just (t := p) -> do
      updatedStack <- updateEquals t p rest
      updatedWork  <- updateResult t p work
      unification updatedStack (t := p : updatedWork)
    Nothing -> unification (stack' ++ rest) work

updateEquals :: Term -> Term -> Targets -> Maybe Targets
updateEquals t p equals = Just $ map f equals
  where f (t' :? p') = replaceOccurrence t p t' :? replaceOccurrence t p p'

updateResult :: Term -> Term -> Result -> Maybe Result
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

unificateTerms :: Term -> Term -> Maybe (Targets, Maybe ResultPair)
unificateTerms t p = case (t, p) of
  (Cut                     , _           ) -> Just ([], Nothing)
  (VariableTerm Anonymous  , _           ) -> Just ([], Nothing)
  (VariableTerm (Named n i), AtomTerm x  ) -> Just ([], Just (t := p))
  (VariableTerm (Named n i), NumberTerm x) -> Just ([], Just (t := p))
  (VariableTerm (Named n1 i1), VariableTerm (Named n2 i2)) ->
    Just ([], Just (t := p))
  (_, VariableTerm (Named n i)) -> unificateTerms p t
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


data SearchTree
  = Leaf (Maybe Result)
  | Node Result Resolvent [SearchTree]
  deriving (Show)

data Resolvent 
  = ResolventNil 
  | Resolvent Term [Term] Resolvent
  deriving (Show)
  

search :: Program -> Question -> SearchTree
search (Program clauses) question = Node [] resolvent trees
  where
    terms = bodyToTerms question
    questionTerm = AtomTerm (Symbolic "Start")
    resolvent = (Resolvent questionTerm terms ResolventNil) 
    trees = search' clauses resolvent []

search' :: [Clause] -> Resolvent -> Result -> [SearchTree]
search' _ ResolventNil result = [Leaf (Just result)]
search' clauses (Resolvent _ [] resolvent) result = search' clauses resolvent result
search' clauses resolvent@(Resolvent f (Cut:_) _) result = [Leaf (Just result)]
search' clauses (Resolvent f (t:ts) resolvent) result = y
  where 
    x = unificateClausesTerm clauses t result 
    y = map z x 
    z Nothing = Leaf Nothing
    z (Just (f', b, result')) = 
      let 
        resolvent' = Resolvent f' b (Resolvent f ts resolvent)
        trees = search' clauses resolvent' result'
      in Node result resolvent' trees


unificateClausesTerm :: [Clause] -> Term -> Result -> [Maybe (Term, [Term], Result)]
unificateClausesTerm clauses term result =
  filter (Nothing /=) $ map (\x -> unificateClauseTerm x term result) clauses

unificateClauseTerm :: Clause -> Term -> Result -> Maybe (Term, [Term], Result)
unificateClauseTerm (Rule t1 b) t2 result = do
  x <- unification [t1 :? t2] result
  return (t1, bodyToTerms b, x)
unificateClauseTerm (Fact t1) t2 result = do
  x <- unification [t1 :? t2] result
  return (t1, [], x)


bodyToTerms :: Body -> [Term]
bodyToTerms b = res
  where
    body' = simplify b
    extract' (Element t) = t
    extract (Element t) acc = t:acc
    extract (Conjunctive t) acc = map extract' t ++ acc
    extract _ acc = acc
    res = foldr extract [] [body']



predefinedMult = concatMap 
  (\x -> map 
    (\y -> Fact (CompoundTerm (Symbolic "mult") [NumberTerm (Int y), NumberTerm (Int x),  NumberTerm (Int (y*x)) ]  )) 
    [0..10]) 
  [0..10]

predefinedDec = map (\x -> Fact (CompoundTerm (Symbolic "dec") [NumberTerm (Int x),  NumberTerm (Int (x-1)) ] ))  [0..10]