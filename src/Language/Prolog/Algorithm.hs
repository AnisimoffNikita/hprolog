module Language.Prolog.Algorithm 
  where 


import           Control.Monad.State
import           Data.List                      ( find
                                                )
import qualified Data.Map                      as M
import           Debug.Trace

import           Language.Prolog.Helper
import qualified Language.Prolog.Syntax        as Syntax
import           Language.Prolog.Semantics
import           Language.Prolog.Math
import           Language.Prolog.Bool

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

itemResolvent :: TermInfo -> [Term] -> Resolvent
itemResolvent term body = Resolvent (Just term) body EmptyResolvent

type Cutting = Maybe TermInfo

type Prolog a = State Int a

runProlog :: Prolog a -> Int -> a
runProlog = evalState


getNext :: Prolog Int
getNext = get

setNext :: Int -> Prolog ()
setNext = put

semantics :: SemanticsState a -> Prolog a
semantics m = do
  id <- getNext
  let (x, (next, _)) = runSemanticsState m id
  setNext next
  return x

semanticsClause_ :: Syntax.Clause -> Prolog Clause
semanticsClause_ sclause = semantics $ semanticsClause sclause

semanticsTerm_ :: Syntax.Term -> Prolog Term
semanticsTerm_ sterm = semantics $ semanticsTerm sterm

search_ :: Syntax.Program -> Syntax.Question -> SearchTree
search_ (Syntax.Program clauses) question = runProlog
  (do
    resolvent     <- initResolvent <$> mapM semanticsTerm_ question
    (_, branches) <- search' clauses resolvent []
    return $ initSearchTree resolvent branches
  )
  0


getAnswers :: SearchTree -> [Substitution]
getAnswers (Node _ _ branches) = concatMap getAnswers branches
getAnswers (Fail _ _) = []
getAnswers (Ok substitution) = [substitution]

search :: Syntax.Program -> Syntax.Question -> [Substitution]
search (Syntax.Program clauses) question = runProlog
  (do
    id <- getNext
    let (resolvent', (next, vars)) = runSemanticsState (mapM semanticsTerm question) id
        resolvent = initResolvent resolvent'
    setNext next
    (_, branches) <- search' clauses resolvent []
    let final = getAnswers $ initSearchTree resolvent branches 
        result = map (filter (\((Variable _ variable) := _) -> M.member variable vars )) final
    return result
  )
  0

search'
  :: [Syntax.Clause]
  -> Resolvent
  -> Substitution
  -> Prolog (Cutting, [SearchTree])
search' _ EmptyResolvent substitution = return (Nothing, [Ok substitution])

search' sclauses (Resolvent func [] resolvent) substitution =
  search' sclauses resolvent substitution

search' sclauses (Resolvent func (Cut : rest) resolvent) substitution = do
  (_, branches) <- search' sclauses (Resolvent func rest resolvent) substitution
  return (func, branches)

search' sclauses (Resolvent func (p@(CompoundTerm "<" [ConstTerm (Int x), ConstTerm (Int y)]) : rest) resolvent) substitution
  = if x < y
    then search' sclauses (Resolvent func rest resolvent) substitution
    else return (Nothing, [Fail p p])


search' sclauses (Resolvent func (term : rest) resolvent) substitution = do
  let resolvent' = (Resolvent func rest resolvent)
  case term of
    CompoundTerm "is" [_, _] -> isHandler term sclauses resolvent' substitution
    CompoundTerm "<" [_, _] -> boolHandler term sclauses resolvent' substitution
    CompoundTerm ">" [_, _] -> boolHandler term sclauses resolvent' substitution
    CompoundTerm "=<" [_, _] -> boolHandler term sclauses resolvent' substitution
    CompoundTerm ">=" [_, _] -> boolHandler term sclauses resolvent' substitution
    CompoundTerm "==" [_, _] -> boolHandler term sclauses resolvent' substitution
    CompoundTerm "\\=" [_, _] -> boolHandler term sclauses resolvent' substitution
    CompoundTerm "=" [_, _] -> explicitUnification term sclauses resolvent' substitution
    CompoundTerm "trace" [x] -> traceHandler term sclauses resolvent' substitution
    _ ->
      defaultHandler term sclauses (Resolvent func rest resolvent) substitution

traceHandler
  :: Term
  -> [Syntax.Clause]
  -> Resolvent
  -> Substitution
  -> Prolog (Cutting, [SearchTree])
traceHandler (CompoundTerm "trace" [x]) sclauses resolvent substitution =
  traceShow x search' sclauses resolvent substitution 

explicitUnification
  :: Term
  -> [Syntax.Clause]
  -> Resolvent
  -> Substitution
  -> Prolog (Cutting, [SearchTree])
explicitUnification (CompoundTerm "=" [x, y]) sclauses resolvent substitution = do 
  let t = unification [x :? y] substitution
  case t of 
    Nothing -> return (Nothing, [Fail x y])
    Just result' -> do
        let resolvent' = updateResolvent resolvent result'
        (cutted, trees) <- search' sclauses resolvent' result'
        return (cutted, [Node result' resolvent' trees])

isHandler
  :: Term
  -> [Syntax.Clause]
  -> Resolvent
  -> Substitution
  -> Prolog (Cutting, [SearchTree])
isHandler (CompoundTerm "is" [var, formula]) sclauses resolvent substitution = do 
  let r = eval formula
      t = r >>= (\x -> unification [var :? x] substitution)
  case t of 
    Nothing -> return (Nothing, [Fail var formula])
    Just result' -> do
        let resolvent' = updateResolvent resolvent result'
        (cutted, trees) <- search' sclauses resolvent' result'
        return (cutted, [Node result' resolvent' trees])

boolHandler
  :: Term
  -> [Syntax.Clause]
  -> Resolvent
  -> Substitution
  -> Prolog (Cutting, [SearchTree])
boolHandler bool sclauses resolvent substitution = do 
  let r = evalBool bool
  case r of 
    Just True -> search' sclauses resolvent substitution
    _ -> return (Nothing, [])

defaultHandler
  :: Term
  -> [Syntax.Clause]
  -> Resolvent
  -> Substitution
  -> Prolog (Cutting, [SearchTree])
defaultHandler term sclauses resolvent substitution = do
  clauses <- mapM semanticsClause_ sclauses
  let unfications =
        zip (unificateClausesTerm clauses term substitution) clauses
      f (Nothing, Rule p _) = return (Nothing, Fail p term)
      f (Nothing, Fact p) = return (Nothing, Fail p term)
      f (Just (func', terms', substitution'), _) = do
        let resolvent'  = Resolvent (Just func') terms' resolvent
            resolvent'' = updateResolvent resolvent' substitution'
        (cutted, branches) <- search' sclauses resolvent'' substitution'
        return (cutted, Node substitution' resolvent'' branches)
  branches <- mapM f unfications
  let
    cutInfo   = termInfo term
    branches' = map snd $ takeWhile' check branches
    check (x, _) = x /= Just cutInfo
    needCut = if length branches /= length branches
      then Just $ termInfo term
      else Nothing
  return (needCut, branches')


updateResolvent :: Resolvent -> Substitution -> Resolvent
updateResolvent EmptyResolvent                   result = EmptyResolvent
updateResolvent (Resolvent func terms resolvent) result = Resolvent
  func
  (map (updateTarget result) terms)
  (updateResolvent resolvent result)

updateTarget :: Substitution -> Term -> Term
updateTarget result term = term'
 where
  term' = foldl updater term result
  updater term (v' := t') = replaceOccurrence v' t' term

unificateClausesTerm
  :: [Clause]
  -> Term
  -> Substitution
  -> [Maybe (TermInfo, [Term], Substitution)]
unificateClausesTerm clauses term result =
  map (\x -> unificateClauseTerm x term result) clauses

unificateClauseTerm
  :: Clause -> Term -> Substitution -> Maybe (TermInfo, [Term], Substitution)
unificateClauseTerm (Rule t1 b) t2 result = do
  x <- unification [t1 :? t2] result
  return (termInfo t1, b, x)
unificateClauseTerm (Fact t1) t2 result = do
  x <- unification [t1 :? t2] result
  return (termInfo t1, [], x)


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
