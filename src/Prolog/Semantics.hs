module Prolog.Semantics where 

import qualified Prolog.Syntax as S

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
  | Cut String Int
  deriving (Eq)

instance Show Term where 
  show (ConstTerm x) = show x
  show (VariableTerm x) = show x 
  show (CompoundTerm f terms) = f ++ "(" ++ intercalate ", " (map show terms)  ++ ")"
  show (Cut _ _) = "!"


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
  deriving (Eq)


type SemanticsState = State (Int, M.Map String Int)


termInfo :: Term -> TermInfo 
termInfo term = 
  case term of 
    CompoundTerm s args' -> TermInfo s (length args')
    ConstTerm (Atom s) -> TermInfo s 0
    _ -> error "semantic termInfo is not implemented"

semanticsClause :: S.Clause -> SemanticsState Clause 
semanticsClause (S.Rule term terms) = do
  term' <- semanticsTerm term
  let 
    cutInfo = termInfo term' 
  terms' <- mapM (semanticsTerm' cutInfo) terms
  return $ Rule term' terms'
semanticsClause (S.Fact term) = do 
  term' <- semanticsTerm term 
  return $ Fact term'

semanticsTerm' :: TermInfo -> S.Term -> SemanticsState Term  
semanticsTerm' _ (S.AtomTerm a) = semanticsAtom a 
semanticsTerm' _ (S.NumberTerm a) = semanticsNumber a 
semanticsTerm' _ (S.VariableTerm a) = semanticsVariable a
semanticsTerm' _ (S.CompoundTerm a t) = semanticsCompoundTerm a t
semanticsTerm' (TermInfo f arity) S.Cut = return $ Cut f arity

semanticsTerm :: S.Term -> SemanticsState Term  
semanticsTerm = semanticsTerm' (TermInfo "" 0)

semanticsAtom :: S.Atom -> SemanticsState Term 
semanticsAtom (S.Symbolic s) = return $ ConstTerm (Atom s)

semanticsNumber :: S.Number -> SemanticsState Term 
semanticsNumber (S.Int s) = return $ ConstTerm (Int s)
semanticsNumber (S.Float s) = return $ ConstTerm (Float s)


semanticsVariable :: S.Variable -> SemanticsState Term 
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


semanticsCompoundTerm :: S.Atom -> [S.Term]-> SemanticsState Term 
semanticsCompoundTerm (S.Symbolic f) terms = do 
  terms' <- mapM semanticsTerm terms 
  return $ CompoundTerm f terms'