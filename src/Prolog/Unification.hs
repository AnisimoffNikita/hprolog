{-# LANGUAGE TypeOperators #-}
module Prolog.Unification where

import qualified Prolog.Syntax                 as Syntax
import           Prolog.Semantics
import           Data.List                      ( find )
import           Control.Monad                  ( zipWithM )
import           Debug.Trace
import qualified Data.Map                      as M
import           Control.Monad.Writer
import           Control.Monad.State
import           Control.Monad.Trans.Maybe



data ResultPair
  = Variable := Term
  deriving (Show, Eq)
data Target 
  = Term :? Term 
  deriving (Show, Eq)

type Targets = [Target]
type Result = [ResultPair]


data SearchTree
  = Leaf (Maybe Result)
  | Node Result Resolvent [SearchTree]
  deriving (Show)

type Resolvent = [Term]


search :: Syntax.Program -> Syntax.Question -> SearchTree
search (Syntax.Program clauses) question = Node result resolvent trees
  where
    result = []
    resolvent = []
    trees = search' clauses resolvent result

search' :: [Syntax.Clause] -> Resolvent -> Result -> [SearchTree]
search' _ [] result = [Leaf (Just result)]
search' clauses (t:ts) result = y
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
  return (t1, b, x)
unificateClauseTerm (Fact t1) t2 result = do
  x <- unification [t1 :? t2] result
  return (t1, [], x)



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

updateEquals :: Variable -> Term -> Targets -> Maybe Targets
updateEquals t p equals = Just $ map f equals
  where f (t' :? p') = replaceOccurrence t p t' :? replaceOccurrence t p p'

updateResult :: Variable -> Term -> Result -> Maybe Result
updateResult t p results = concat <$> mapM f results
 where
  f x@(t' := p') =
    let p'' = replaceOccurrence t p p'
    in  if p' == p'' then Just [x] else unification [VariableTerm t' :? p''] []

replaceOccurrence :: Variable -> Term -> Term -> Term
replaceOccurrence t q (CompoundTerm f args) = CompoundTerm f (map update args)
 where
  update p@(CompoundTerm f args) = replaceOccurrence t q p
  update p@(VariableTerm p'    ) = if t == p' then q else p
  update p                       = p
replaceOccurrence t q p@(VariableTerm p') = if t == p' then q else p
replaceOccurrence _ _ p                   = p



unificateTerms :: Term -> Term -> Maybe (Targets, Maybe ResultPair)
unificateTerms t p = case (t, p) of
  (Cut                   , _                  ) -> Just ([], Nothing)
  (VariableTerm Anonymous, _                  ) -> Just ([], Nothing)
  (VariableTerm x        , ConstTerm _        ) -> Just ([], Just (x := p))
  (VariableTerm x        , VariableTerm _     ) -> Just ([], Just (x := p))
  (VariableTerm x        , CompoundTerm f args) -> 
    case find (t ==) args of
      Just _  -> Nothing
      Nothing -> Just ([], Just (x := p))

  (_, VariableTerm _) -> unificateTerms p t

  (CompoundTerm f1 args1, CompoundTerm f2 args2) ->
    if f1 == f2 && length args1 == length args2
      then Just (zipWith (:?) args1 args2, Nothing)
      else Nothing
  (t, p) -> if t == p then Just ([], Nothing) else Nothing




