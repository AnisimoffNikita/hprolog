module Prolog.Syntax where 

import Data.List

data Program 
  = Program [Clause] Predicate
  deriving (Show)

data Clause 
  = Fact Predicate
  | Rule Predicate Body
  deriving (Show)

data Predicate 
  = Predicate Atom [Term]
  | Cut
  deriving (Show)

data Term 
  = Atom Atom 
  | Number Number
  | Variable Variable
  | CompoundTerm Atom [Term]
  deriving (Show)

data Variable 
  = Named String
  | Anonymous 
  deriving (Show)

data Atom 
  = Symbolic String
  | Quoted String
  | Special String
  | Struct
  deriving (Show)

data Number 
  = Int Int 
  | Float Float 
  deriving (Show)
