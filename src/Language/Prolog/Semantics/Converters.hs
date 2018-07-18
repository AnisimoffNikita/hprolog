module Language.Prolog.Semantics.Converters where

import           Data.Map                      as M
import           Control.Monad.State

import           Language.Prolog.Semantics.Types
import qualified Language.Prolog.Syntax        as S



termInfo :: Term -> TermInfo
termInfo term = case term of
  CompoundTerm s args' -> TermInfo s (length args')
  ConstTerm (Atom s)   -> TermInfo s 0
  _                    -> error "semantic termInfo is not implemented"


type SemanticsData = (VarID, M.Map String Int)

type SemanticsState a = State SemanticsData a

evalSemanticsState :: SemanticsState a -> SemanticsData -> a
evalSemanticsState = evalState

runSemanticsState :: SemanticsState a -> Int -> (a, SemanticsData)
runSemanticsState m s = runState m (initSemanticsState s)

execSemanticsState :: SemanticsState a -> SemanticsData -> SemanticsData
execSemanticsState = execState


initSemanticsState :: Int -> SemanticsData
initSemanticsState id = (id, M.empty)


semanticsClause :: S.Clause -> SemanticsState Clause
semanticsClause sclause = case sclause of
  S.Rule term terms -> do
    term'  <- semanticsTerm term
    terms' <- mapM semanticsTerm terms
    return $ Rule term' terms'
  S.Fact term -> Fact <$> semanticsTerm term

semanticsTerm :: S.Term -> SemanticsState Term
semanticsTerm (S.AtomTerm     a  ) = semanticsAtom a
semanticsTerm (S.NumberTerm   a  ) = semanticsNumber a
semanticsTerm (S.VariableTerm a  ) = semanticsVariable a
semanticsTerm (S.CompoundTerm a t) = semanticsCompoundTerm a t
semanticsTerm S.Cut                = return Cut


semanticsAtom :: S.Atom -> SemanticsState Term
semanticsAtom (S.Symbolic s) = return $ ConstTerm (Atom s)

semanticsNumber :: S.Number -> SemanticsState Term
semanticsNumber (S.Int   s) = return $ ConstTerm (Int s)
semanticsNumber (S.Float s) = return $ ConstTerm (Float s)


semanticsVariable :: S.Variable -> SemanticsState Term
semanticsVariable (S.Named x) = do
  (next, vars) <- get
  let id = M.lookup x vars
  case id of
    Just id' -> return $ VariableTerm (Variable id' x)
    Nothing  -> do
      let id'   = next
          vars' = M.insert x id' vars
          next' = id' + 1
      put (next', vars')
      return $ VariableTerm (Variable id' x)
semanticsVariable S.Anonymous = return $ VariableTerm Anonymous


semanticsCompoundTerm :: S.Atom -> [S.Term] -> SemanticsState Term
semanticsCompoundTerm (S.Symbolic f) terms =
  CompoundTerm f <$> mapM semanticsTerm terms
