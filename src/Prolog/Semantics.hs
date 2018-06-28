module Prolog.Semantics where 

import qualified Prolog.Syntax as S

import Control.Monad.State
import qualified Data.Map as M

data Program
  = Program [Clause]
  deriving (Show)

data Clause
  = Fact Term
  | Rule Term [Term]
  deriving (Show, Eq)

data Term 
  = ConstTerm Const 
  | VariableTerm Variable 
  | CompoundTerm String [Term]
  | Cut
  deriving (Show, Eq)

data Const 
  = Atom String 
  | Int Int 
  | Float Float 
  deriving (Show, Eq)

data Variable 
  = Variable ID 
  | Anonymous 
  deriving (Show, Eq)

type ID = Int


-- semanticsProgram :: S.Program -> Program 
-- semanticsProgram (S.Program clauses) = Program (map semanticsClause clauses) 

type SemanticsState = State (Int, M.Map String Int)

semanticsClause :: S.Clause -> SemanticsState Clause 
semanticsClause (S.Rule term terms) = do 
  term' <- semanticsTerm term 
  terms' <- mapM semanticsTerm terms
  return $ Rule term' terms'
semanticsClause (S.Fact term) = do 
  term' <- semanticsTerm term 
  return $ Fact term'

semanticsTerm :: S.Term-> SemanticsState Term  
semanticsTerm (S.AtomTerm a) = semanticsAtom a 
semanticsTerm (S.NumberTerm a) = semanticsNumber a 
semanticsTerm (S.VariableTerm a) = semanticsVariable a
semanticsTerm (S.CompoundTerm a t) = semanticsCompoundTerm a t
semanticsTerm S.Cut = return Cut

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
    Just id' -> return $ VariableTerm (Variable id')
    Nothing -> do 
      let 
        id' = next
        vars' = M.insert x id' vars
        next' = id' + 1
      put (next', vars')
      return $ VariableTerm (Variable id')
semanticsVariable S.Anonymous = return $ VariableTerm Anonymous


semanticsCompoundTerm :: S.Atom -> [S.Term]-> SemanticsState Term 
semanticsCompoundTerm (S.Symbolic f) terms = do 
  terms' <- mapM semanticsTerm terms
  return $ CompoundTerm f terms'