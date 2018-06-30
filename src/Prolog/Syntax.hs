{-# LANGUAGE FlexibleInstances #-}
module Prolog.Syntax where

import Data.List

data Program
  = Program [Clause]
  deriving (Show)

data Clause
  = Fact Term
  | Rule Term [Term]
  deriving (Show, Eq)

data Term
  = AtomTerm Atom
  | NumberTerm Number
  | VariableTerm Variable
  | CompoundTerm Atom [Term]
  | Cut
  deriving (Show, Eq)



data Variable
  = Named String
  | Anonymous
  deriving (Show, Eq)

data Atom
  = Symbolic String
  deriving (Show, Eq)

data Number
  = Int Int
  | Float Float
  deriving (Show, Eq)


data Body 
  = Disjunctive [Body]
  | Conjunctive [Body]
  | Element Term
  deriving (Show, Eq)

type Question = [Term]

data TermInfo = TermInfo String Int

termInfo :: Term -> TermInfo 
termInfo term = 
  case term of 
    CompoundTerm (Symbolic s) args' -> TermInfo s (length args')
    AtomTerm (Symbolic s) -> TermInfo s 0
    _ -> error "syntax termInfo is not implemented"