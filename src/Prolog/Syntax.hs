module Prolog.Syntax where

import Data.List

data Program
  = Program [Clause] Term
  deriving (Show)

data Clause
  = Fact Term
  | Rule Term Body
  deriving (Show)

data Term
  = AtomTerm Atom
  | NumberTerm Number
  | VariableTerm Variable
  | CompoundTerm Atom [Term]
  | Cut
  deriving (Show)

data Variable
  = Named String
  | Anonymous
  deriving (Show)

data Atom
  = Symbolic String
  deriving (Show)

data Number
  = Int Int
  | Float Float
  deriving (Show)


data Body 
  = Conjunctive [Body]
  | Disjunctive [Body]
  | Term Term
  deriving (Show)
