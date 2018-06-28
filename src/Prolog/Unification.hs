{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
module Prolog.Unification where

import qualified Prolog.Syntax                 as Syntax
import           Prolog.Semantics
import           Control.Monad                  ( zipWithM )
import           Debug.Trace
import qualified Data.Map                      as M
import           Control.Monad.Writer
import           Control.Monad.State
import           Control.Monad.Trans.Maybe
import Data.List (dropWhileEnd, intercalate, find)


data ResultPair
  = Variable := Term
  | Variable :=: Variable
  deriving (Show, Eq)
data Target 
  = Term :? Term 
  deriving (Show, Eq)

type Targets = [Target]
type Result = [ResultPair]

instance {-# OVERLAPPING #-} Show [ResultPair] where 
  show result = "{" ++ intercalate ", " (map show result) ++ "}"

data SearchTree
  = Leaf (Maybe Result)
  | Node Result Resolvent [SearchTree]
  deriving (Show)

type Resolvent = [Term]

semanticsClause' clause = do 
  x <- semanticsClause clause
  (next, _) <- get 
  put (next, M.empty)
  return x


search :: Syntax.Program -> Syntax.Question -> SearchTree
search (Syntax.Program clauses) question = Node result resolvent tree
  where
    result = []
    resolvent = evalState (mapM semanticsTerm question) (0, M.empty)
    trees = search' clauses resolvent result
    (_, tree) = evalState (search' clauses resolvent result) (1000, M.empty)

search' :: [Syntax.Clause] -> Resolvent -> Result -> SemanticsState (Bool, [SearchTree])
search' _ [] result = return (False, [Leaf (Just result)])
search' sclauses (Cut:ts) result = do 
  (_, trees) <- search' sclauses ts result 
  return (True, trees)
search' sclauses (t:ts) result = do 
  clauses <- mapM semanticsClause' sclauses
  let 
    x = unificateClausesTerm clauses t result 
    z Nothing = return (False, Leaf Nothing)
    z (Just (b, result')) = do 
      let 
        resolvent' = updateResolvent (b ++ ts) result'
      (cutted, trees) <- search' sclauses resolvent' result'
      return (cutted, Node result' resolvent' trees)
  flags <- mapM z x 
  let trees = map snd $ takeWhile' (\(x,_) -> not x)  flags

  if null trees 
    then return (length flags == length trees, [Leaf Nothing])
    else return (length flags == length trees, trees)

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' f [] = []
takeWhile' f (x:xs) = if not . f $ x then [x] else x : takeWhile' f xs

updateResolvent :: Resolvent -> Result -> Resolvent 
updateResolvent resolvent result = map (updateTarget result) resolvent

updateTarget :: Result -> Term -> Term 
updateTarget result term = term'
  where 
    term' = foldl updater term result 
    updater term (v' := t') = replaceOccurrence v' t' term

unificateClausesTerm :: [Clause] -> Term -> Result -> [Maybe ([Term], Result)]
unificateClausesTerm clauses term result =
  map (\x -> unificateClauseTerm x term result) clauses

unificateClauseTerm :: Clause -> Term -> Result -> Maybe ([Term], Result)
unificateClauseTerm (Rule t1 b) t2 result = do
  x <- unification [t1 :? t2] result
  return (b, x)
unificateClauseTerm (Fact t1) t2 result = do
  x <- unification [t1 :? t2] result
  return ([], x)



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
      let 
        (VariableTerm t'')  = replaceOccurrence t p (VariableTerm t')
        p'' = replaceOccurrence t p p'
      in if p' == p'' && t' == t'' then Just [x] else unification [VariableTerm t'' :? p''] []

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
  (_                   , Cut                  ) -> Just ([], Nothing)
  (VariableTerm Anonymous, _                  ) -> Just ([], Nothing)
  (VariableTerm x        , ConstTerm _        ) -> Just ([], Just (x := p))
  (VariableTerm x        , VariableTerm y     ) -> Just ([], Just (x := p))
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



printResult :: Result -> String
printResult = show

printResolvent :: Resolvent -> String 
printResolvent r = intercalate "\n" $ map show r


printer :: SearchTree -> String 
printer t@(Node result resolvent trees) = (intercalate "\n" $ map f trees) ++ "\n\n" ++ rest
  where 
    header = makeHeader t
    f tree = header ++ "\n->\n" ++ makeHeader tree
    rest :: String
    rest = concatMap printer trees
printer (Leaf (Just result)) = "\"" ++ printResult result ++ "\""
printer (Leaf Nothing) = "\"fail\""

makeHeader (Node result resolvent trees) = "\"" ++ printResult result ++ "\n" ++ printResolvent resolvent ++ "\""
makeHeader x = printer x