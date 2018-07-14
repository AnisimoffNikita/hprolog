{-# LANGUAGE FlexibleInstances #-}
module Prolog.Prolog where

import           Control.Monad.State
import           Control.Monad.Writer
import           Data.Maybe                     ( fromMaybe, isJust )
import qualified Data.Map                      as M
import           Data.List                      ( intercalate
                                                , find
                                                )
import           Prolog.Helper
import qualified Prolog.Syntax                 as Syntax
import           Prolog.Semantics
import           Prolog.Math

import           Debug.Trace

data PairSubstitution
  = Variable := Term
  deriving (Show, Eq)

type Substitution = [PairSubstitution ]


data PairTarget
  = Term :? Term
  deriving (Show, Eq)

type Target = [PairTarget]


data SearchTree
  = Ok Substitution
  | Fail Term Term
  | Node Substitution Resolvent [SearchTree]
  deriving (Show)

initSearchTree :: Resolvent -> [SearchTree] -> SearchTree
initSearchTree = Node []


data Resolvent
  = Resolvent (Maybe TermInfo) [Term] Resolvent
  | EmptyResolvent
  deriving (Show)

initResolvent :: [Term] -> Resolvent
initResolvent question = Resolvent Nothing question EmptyResolvent


type Prolog a = SemanticsStateT (State (Maybe TermInfo)) a

runProlog :: Prolog a -> SemanticsData -> a
runProlog m semanticsState =
  evalState (evalSemanticsStateT m semanticsState) Nothing


cut :: Maybe TermInfo -> Prolog ()
cut = lift . put

isCutted :: Prolog (Maybe TermInfo)
isCutted = lift get

resetCut :: Prolog ()
resetCut = lift $ put Nothing

prolog :: Prolog a -> a
prolog m = runProlog m initSemanticsState

search :: Syntax.Program -> Syntax.Question -> SearchTree
search (Syntax.Program clauses) question = prolog $ do
  resolvent <- initResolvent <$> mapM semanticsTerm question
  branches  <- search' clauses resolvent []
  return $ initSearchTree resolvent branches


-- table = [(CompoundTerm "is" [WildCard, WildCard], isHandler)]

-- isHandler :: [Term] -> Resolvent -> Substitution -> Prolog [SearchTree]
-- isHandler args (Resolvent info (term:terms) resolvent) substitution = do 


search' :: [Syntax.Clause] -> Resolvent -> Substitution -> Prolog [SearchTree]
search' _ EmptyResolvent substitution = return [Ok substitution]

search' sclauses (Resolvent func [] resolvent) substitution =
  search' sclauses resolvent substitution

search' sclauses (Resolvent func (Cut : rest) resolvent) substitution = do
  trees <- search' sclauses (Resolvent func rest resolvent) substitution
  cut func
  return trees

search' sclauses (Resolvent func (term : rest) resolvent) substitution = do
  cutted <- isCutted 
  if cutted == func
    then search' sclauses resolvent substitution
    else do 
      resetCut
      handleTerm sclauses term (Resolvent func rest resolvent) substitution


handleTerm = undefined

-- search' sclauses (Resolvent func (term : rest) resolvent) result = do
--   isCutted <- 

-- search' sclauses (CompoundTerm "is" [var, formula] : ts) result = do 
--   let r = eval formula
--       t = traceShow  formula $ r >>= (\x -> unification [var :? x] result)
--   case t of 
--     Nothing -> return (Nothing, [Fail var formula])
--     Just result' -> do
--         let resolvent' = updateResolvent ts result'
--         (cutted, trees) <- search' sclauses resolvent' result'
--         return (cutted, [Node result' resolvent' trees])

-- search' sclauses (t : ts) result = do
--   clauses <- mapM semanticsClause' sclauses
--   let x = zip (unificateClausesTerm clauses t result) clauses
--       z (Nothing          , Rule p _) = return (Nothing, Fail p t)
--       z (Nothing          , Fact p  ) = return (Nothing, Fail p t)
--       z (Just (b, result'), _       ) = do
--         let resolvent' = updateResolvent (b ++ ts) result'
--         (cutted, trees) <- search' sclauses resolvent' result'
--         return (cutted, Node result' resolvent' trees)
--   flags <- mapM z x
--   let
--     cutInfo = termInfo t 
--     trees = map snd $ takeWhile' check flags
--     check (x, _) = maybe True (/= cutInfo) x
--     needCut = if length flags /= length trees then Just $ termInfo t else Nothing

--   return (needCut, trees)


-- updateResolvent :: Resolvent -> Result -> Resolvent
-- updateResolvent resolvent result = map (updateTarget result) resolvent

updateTarget :: Substitution -> Term -> Term
updateTarget result term = term'
 where
  term' = foldl updater term result
  updater term (v' := t') = replaceOccurrence v' t' term

unificateClausesTerm
  :: [Clause] -> Term -> Substitution -> [Maybe ([Term], Substitution)]
unificateClausesTerm clauses term result =
  map (\x -> unificateClauseTerm x term result) clauses

unificateClauseTerm
  :: Clause -> Term -> Substitution -> Maybe ([Term], Substitution)
unificateClauseTerm (Rule t1 b) t2 result = do
  x <- unification [t1 :? t2] result
  return (b, x)
unificateClauseTerm (Fact t1) t2 result = do
  x <- unification [t1 :? t2] result
  return ([], x)


unification :: Target -> Substitution -> Maybe Substitution
unification []              work = Just work
unification (t :? p : rest) work = do
  (stack', work') <- unificateTerms t p
  case work' of
    Just (t := p) -> do
      updatedStack <- updateEquals t p rest
      updatedWork  <- updateResult t p work
      unification updatedStack (t := p : updatedWork)
    Nothing -> unification (stack' ++ rest) work

updateEquals :: Variable -> Term -> Target -> Maybe Target
updateEquals t p equals = Just $ map f equals
  where f (t' :? p') = replaceOccurrence t p t' :? replaceOccurrence t p p'

updateResult :: Variable -> Term -> Substitution -> Maybe Substitution
updateResult t p results = concat <$> mapM f results
 where
  f x@(t' := p') =
    let p'' = replaceOccurrence t p p'
    in  if p' == p'' then Just [x] else unification [VariableTerm t' :? p''] []

replaceOccurrence :: Variable -> Term -> Term -> Term
replaceOccurrence t q (CompoundTerm f args) = CompoundTerm f (map update args)
  where update = replaceOccurrence t q
replaceOccurrence t q p@(VariableTerm p') = if t == p' then q else p
replaceOccurrence _ _ p                   = p


unificateTerms :: Term -> Term -> Maybe (Target, Maybe PairSubstitution)
unificateTerms t p = case (t, p) of
  (VariableTerm Anonymous, _                  ) -> Just ([], Nothing)
  (VariableTerm x        , ConstTerm _        ) -> Just ([], Just (x := p))
  (VariableTerm x        , VariableTerm y     ) -> Just ([], Just (x := p))
  (VariableTerm x        , CompoundTerm f args) -> case find (t ==) args of
    Just _  -> Nothing
    Nothing -> Just ([], Just (x := p))

  (_, VariableTerm _) -> unificateTerms p t

  (CompoundTerm f1 args1, CompoundTerm f2 args2) ->
    if f1 == f2 && length args1 == length args2
      then Just (zipWith (:?) args1 args2, Nothing)
      else Nothing
  (Cut, _  ) -> error "cut unification"
  (_  , Cut) -> error "cut unification"
  (t  , p  ) -> if t == p then Just ([], Nothing) else Nothing

