module Language.Prolog.Semantics.Types
  where

import           Data.List                      ( intercalate )

data Program
  = Program [Clause] Question
  deriving (Show)

data Clause
  = Fact Term
  | Rule Term [Term]
  deriving (Eq, Ord)

instance Show Clause where
  show (Fact head) = show head ++ "."
  show (Rule head body) = show head ++ "(" ++ intercalate ", " (map show body)  ++ ")" ++ "."

data Term
  = ConstTerm Const
  | VariableTerm Variable
  | CompoundTerm String [Term]
  | Cut Int
  deriving (Eq, Ord)

type Question = [Term]

instance Show Term where
  show (ConstTerm x) = show x
  show (VariableTerm x) = show x
  show (CompoundTerm "." [x,y]) = "[" ++ showList x y ++ "]"
    where
      showList h (CompoundTerm "." [x, y]) = show h ++ "," ++ showList x y
      showList h (ConstTerm (Atom "[]")) = show h
      showList h t = show h ++ "|" ++ show t
  show (CompoundTerm f terms) = f ++ "(" ++ intercalate ", " (map show terms)  ++ ")"
  show (Cut x) = "!_" ++ show x

data Const
  = Atom String
  | Int Int
  | Float Float
  deriving (Eq, Ord)

instance Show Const where
  show (Atom name) = name
  show (Int x) = show x
  show (Float x) = show x

data Variable
  = Variable VarID String
  | Anonymous
  deriving (Eq, Ord)

instance Show Variable where
  show (Variable id name) = name ++ "_" ++ show id
  show Anonymous = "_"

type VarID = Int

data TermInfo
  = TermInfo String Int
  deriving (Eq,Show)
