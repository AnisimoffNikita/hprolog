{-# LANGUAGE FlexibleInstances #-}
module Language.Prolog.Algorithm where


import           Control.Monad.State
import           Data.List (find, elem, intercalate)
import qualified Data.Map as M
import           Debug.Trace
import           Language.Prolog.Helper
import qualified Language.Prolog.Syntax as Syntax
import           Language.Prolog.Semantics
import           Language.Prolog.Math
import           Language.Prolog.Bool

data OneSubstitution
  = Variable := Term
  deriving (Show, Eq)

type Substitution = [OneSubstitution]

instance {-# OVERLAPPING #-} Show Substitution where
  show x = intercalate "\n" (map show x)

data OneTarget
  = Term :? Term
  deriving (Show, Eq)

type Target = [OneTarget]

data SearchTree
  = Ok (Maybe OneTarget) Substitution
  | Fail (Maybe OneTarget)
  | Node (Maybe OneTarget) Substitution Resolvent [SearchTree]
  deriving (Show)

initSearchTree :: Resolvent -> [SearchTree] -> SearchTree
initSearchTree = Node Nothing []

data Resolvent
  = Resolvent (Maybe Term) [Term] Resolvent
  | EmptyResolvent

instance Show Resolvent where
  show (Resolvent _ terms rest) = intercalate "\n" (map show full)
    where
      full = terms ++ getTerms rest
      getTerms (Resolvent _ terms rest) = terms ++ getTerms rest
      getTerms EmptyResolvent = []
  show EmptyResolvent = ""

initResolvent :: [Term] -> Resolvent
initResolvent question = Resolvent Nothing question EmptyResolvent

type Cutting = Maybe Term

type PrologM a = State VarID a

runPrologM :: PrologM a -> VarID -> a
runPrologM = evalState

getNextId :: PrologM VarID
getNextId = get

setNextId :: VarID -> PrologM ()
setNextId = put

semantics :: SemanticsState a -> PrologM a
semantics m = do
  id <- getNextId
  let (x, (next, _)) = runSemanticsState m id
  setNextId next
  return x

semanticsClause_ :: Syntax.Clause -> PrologM Clause
semanticsClause_ sclause = semantics $ semanticsClause sclause

semanticsTerm_ :: Syntax.Term -> PrologM Term
semanticsTerm_ sterm = semantics $ semanticsTerm sterm

search_ :: Syntax.Program -> SearchTree
search_ (Syntax.Program clauses question) = runPrologM
  (do
    resolvent     <- initResolvent <$> mapM semanticsTerm_ question
    (_, branches) <- search' clauses resolvent []
    return $ initSearchTree resolvent branches
  )
  0

getAnswers :: SearchTree -> [Substitution]
getAnswers (Node _ _ _ branches) = concatMap getAnswers branches
getAnswers (Fail _             ) = []
getAnswers (Ok _ substitution  ) = [substitution]

search :: Syntax.Program -> [Substitution]
search (Syntax.Program clauses question) = runPrologM
  (do
    id <- getNextId
    let (resolvent', (next, vars')) =
          runSemanticsState (mapM semanticsTerm question) id
        resolvent = initResolvent resolvent'
        vars = M.foldlWithKey (\acc vn vid -> Variable vid vn : acc) [] vars'
    setNextId next

    (_, branches) <- search' clauses resolvent []
    let final  = getAnswers $ initSearchTree resolvent branches
        result = map (filter (\(var := _) -> elem var vars)) final
    return result
  )
  0

search'
  :: [Syntax.Clause]
  -> Resolvent
  -> Substitution
  -> PrologM (Cutting, [SearchTree])
search' _ EmptyResolvent substitution =
  return (Nothing, [Ok Nothing substitution])
search' sclauses (Resolvent func [] resolvent) substitution =
  search' sclauses resolvent substitution
search' sclauses (Resolvent func (Cut : rest) resolvent) substitution = do
  (_, branches) <- search' sclauses (Resolvent func rest resolvent) substitution
  return (func, branches)
search' sclauses (Resolvent func (term : rest) resolvent) substitution = do
  let resolvent' = (Resolvent func rest resolvent)
  case term of
    CompoundTerm "is" [_, _] -> isHandler term sclauses resolvent' substitution
    CompoundTerm "<" [_, _] ->
      boolHandler term sclauses resolvent' substitution
    CompoundTerm ">" [_, _] ->
      boolHandler term sclauses resolvent' substitution
    CompoundTerm "=<" [_, _] ->
      boolHandler term sclauses resolvent' substitution
    CompoundTerm ">=" [_, _] ->
      boolHandler term sclauses resolvent' substitution
    CompoundTerm "==" [_, _] ->
      boolHandler term sclauses resolvent' substitution
    CompoundTerm "\\=" [_, _] ->
      boolHandler term sclauses resolvent' substitution
    CompoundTerm "=" [_, _] ->
      explicitUnification term sclauses resolvent' substitution
    CompoundTerm "trace" [x] ->
      traceHandler term sclauses resolvent' substitution
    _ ->
      defaultHandler term sclauses (Resolvent func rest resolvent) substitution

traceHandler
  :: Term
  -> [Syntax.Clause]
  -> Resolvent
  -> Substitution
  -> PrologM (Cutting, [SearchTree])
traceHandler (CompoundTerm "trace" [x]) = traceShow x search'

explicitUnification
  :: Term
  -> [Syntax.Clause]
  -> Resolvent
  -> Substitution
  -> PrologM (Cutting, [SearchTree])
explicitUnification (CompoundTerm "=" [x, y]) sclauses resolvent substitution =
  do
    let t = unification [x :? y] substitution
    case t of
      Nothing      -> return (Nothing, [Fail (Just $ x :? y)])
      Just result' -> do
        let resolvent' = updateResolvent resolvent result'
        (cutted, trees) <- search' sclauses resolvent' result'
        return (cutted, [Node (Just $ x :? y) result' resolvent' trees])

isHandler
  :: Term
  -> [Syntax.Clause]
  -> Resolvent
  -> Substitution
  -> PrologM (Cutting, [SearchTree])
isHandler (CompoundTerm "is" [var, formula]) sclauses resolvent substitution =
  do
    let r = eval formula
        t = r >>= (\x -> unification [var :? x] substitution)
    case t of
      Nothing      -> return (Nothing, [Fail (Just $ var :? formula)])
      Just result' -> do
        let resolvent' = updateResolvent resolvent result'
        (cutted, trees) <- search' sclauses resolvent' result'
        return (cutted, [Node Nothing result' resolvent' trees])

boolHandler
  :: Term
  -> [Syntax.Clause]
  -> Resolvent
  -> Substitution
  -> PrologM (Cutting, [SearchTree])
boolHandler bool sclauses resolvent substitution = do
  let r = evalBool bool
  case r of
    Just True -> search' sclauses resolvent substitution
    _         -> return (Nothing, [])

defaultHandler
  :: Term
  -> [Syntax.Clause]
  -> Resolvent
  -> Substitution
  -> PrologM (Cutting, [SearchTree])
defaultHandler term sclauses resolvent substitution = do
  clauses <- mapM semanticsClause_ sclauses
  let
    unfications = zip (unificateClausesTerm clauses term substitution) clauses
    f (Nothing, Rule p _) = return (Nothing, Fail (Just $ p :? term))
    f (Nothing, Fact p) = return (Nothing, Fail (Just $ p :? term))
    f (Just (func', terms', substitution'), _) = do
      let resolvent'  = Resolvent (Just func') terms' resolvent
          resolvent'' = updateResolvent resolvent' substitution'
      (cutted, branches) <- search' sclauses resolvent'' substitution'
      return
        (cutted, Node (Just $ term :? func') substitution' resolvent'' branches)
  branches <- mapM f unfications
  let
    cutInfo   = termInfo term
    branches' = map snd $ takeWhile' check branches
    check (x, _) = fmap termInfo x /= Just cutInfo
    needCut = if length branches /= length branches then Just term else Nothing
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
  :: [Clause] -> Term -> Substitution -> [Maybe (Term, [Term], Substitution)]
unificateClausesTerm clauses term result =
  map (\x -> unificateClauseTerm x term result) clauses

unificateClauseTerm
  :: Clause -> Term -> Substitution -> Maybe (Term, [Term], Substitution)
unificateClauseTerm (Rule t1 b) t2 result = do
  x <- unification [t1 :? t2] result
  return (t1, b, x)
unificateClauseTerm (Fact t1) t2 result = do
  x <- unification [t1 :? t2] result
  return (t1, [], x)


-- unification :: Target -> Substitution -> Maybe Substitution
-- unification []              work = Just work
-- unification (t :? p : rest) work = do
--   (stack', work') <- unificateTerms t p
--   case work' of
--     Just (t := p) -> do
--       updatedStack <- updateEquals t p rest
--       updatedWork  <- updateResult t p work
--       unification updatedStack (t := p : updatedWork)
--     Nothing -> unification (stack' ++ rest) work


unification :: Target -> Substitution -> Maybe Substitution
unification []                 substitution = Just substitution
unification (t :? p : targets) substitution = do
  (newTargets, newSubstitution) <- unificateTerms t p
  case newSubstitution of
    Just (t := p) -> do
      updatedTargets       <- updateEquals t p targets
      updatedSubstitution  <- updateResult t p substitution
      unification updatedTargets (t := p : updatedSubstitution)
    Nothing -> unification (newTargets ++ targets) substitution


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


unificateTerms :: Term -> Term -> Maybe (Target, Maybe OneSubstitution)
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
