module Prolog.Semantics where 

import qualified Prolog.Syntax as S

import Data.Functor.Identity
import Control.Monad.State
import qualified Data.Map as M
import Data.List (dropWhileEnd, intercalate, find)


data Program
  = Program [Clause]
  deriving (Show)

data Clause
  = Fact Term
  | Rule Term [Term]
  deriving (Eq)

instance Show Clause where 
  show (Fact head) = show head ++ "."
  show (Rule head body) = show head ++ "(" ++ intercalate ", " (map show body)  ++ ")" ++ "."


data Term 
  = ConstTerm Const 
  | VariableTerm Variable 
  | CompoundTerm String [Term]
  | Cut
  | WildCard
  deriving (Eq)


instance Show Term where 
  show (ConstTerm x) = show x
  show (VariableTerm x) = show x 
  show (CompoundTerm f terms) = f ++ "(" ++ intercalate ", " (map show terms)  ++ ")"
  show Cut = "!"


data Const 
  = Atom String 
  | Int Int 
  | Float Float 
  deriving (Eq)

instance Show Const where
  show (Atom name) = name
  show (Int x) = show x 
  show (Float x) = show x 


data Variable 
  = Variable ID String
  | Anonymous 
  deriving (Eq)

instance Show Variable where
  show (Variable id name) = name ++ "_" ++ show id
  show Anonymous = "_"

type ID = Int

data TermInfo 
  = TermInfo String Int
  | NoT
  deriving (Eq,Show)


type SemanticsData = (Int, M.Map String Int) 

type SemanticsStateT m a = StateT SemanticsData m a

evalSemanticsStateT :: (Monad m) => SemanticsStateT m a -> SemanticsData -> m a
evalSemanticsStateT  = evalStateT 

runSemanticsStateT :: (Monad m) => SemanticsStateT m a -> SemanticsData -> m (a, SemanticsData)
runSemanticsStateT  = runStateT 

execSemanticsStateT :: (Monad m) => SemanticsStateT m a -> SemanticsData -> m SemanticsData
execSemanticsStateT  = execStateT 

initSemanticsState :: SemanticsData
initSemanticsState = (0 :: Int, M.empty)




termInfo :: Term -> TermInfo 
termInfo term = 
  case term of 
    CompoundTerm s args' -> TermInfo s (length args')
    ConstTerm (Atom s) -> TermInfo s 0
    _ -> error "semantic termInfo is not implemented"

semanticsClause :: (Monad m) => S.Clause -> SemanticsStateT m Clause 
semanticsClause (S.Rule term terms) = do
  term' <- semanticsTerm term
  terms' <- mapM semanticsTerm terms
  return $ Rule term' terms'
semanticsClause (S.Fact term) = do 
  term' <- semanticsTerm term 
  return $ Fact term'



semanticsTerm :: (Monad m) => S.Term -> SemanticsStateT m Term  
semanticsTerm (S.AtomTerm a) = semanticsAtom a 
semanticsTerm (S.NumberTerm a) = semanticsNumber a 
semanticsTerm (S.VariableTerm a) = semanticsVariable a
semanticsTerm (S.CompoundTerm a t) = semanticsCompoundTerm a t
semanticsTerm S.Cut = return  Cut


semanticsAtom :: (Monad m) => S.Atom -> SemanticsStateT m Term 
semanticsAtom (S.Symbolic s) = return $ ConstTerm (Atom s)

semanticsNumber :: (Monad m) => S.Number -> SemanticsStateT m Term 
semanticsNumber (S.Int s) = return $ ConstTerm (Int s)
semanticsNumber (S.Float s) = return $ ConstTerm (Float s)


semanticsVariable :: (Monad m) => S.Variable -> SemanticsStateT m Term 
semanticsVariable (S.Named x) = do
  (next, vars) <- get 
  let id = M.lookup x vars
  case id of 
    Just id' -> return $ VariableTerm (Variable id' x)
    Nothing -> do 
      let 
        id' = next
        vars' = M.insert x id' vars
        next' = id' + 1
      put (next', vars')
      return $ VariableTerm (Variable id' x)
semanticsVariable S.Anonymous = return $ VariableTerm Anonymous


semanticsCompoundTerm :: (Monad m) => S.Atom -> [S.Term]-> SemanticsStateT m Term 
semanticsCompoundTerm (S.Symbolic f) terms = do 
  terms' <- mapM semanticsTerm terms 
  return $ CompoundTerm f terms'