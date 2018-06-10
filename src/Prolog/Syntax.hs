module Prolog.Syntax where 

import Data.List

type VarID = Int

data Number 
  = Int Int 
  | Float Float 


data Atom 
  = Symbolic String
  | Quoted String

data Variable 
  = Named String VarID
  | Anonymous 


data Const 
  = Atom Atom 
  | Number Number
  | String String

data Term 
  = Const Const
  | Variable Variable
  | CompoundTerm Atom [Term]
  | Cut


-- data Op
--   = TrueIf Term Op 



-- data Program 
--   = Program [Clause]

-- data Clause 
--   = Fact Predicate
--   | Rule Predicate [Predicate] 

-- data Predicate 
--   = Atom Atom 
--   | Functor Atom [Terms]







instance Show Number where 
  show (Int i) = show i 
  show (Float f) = show f

instance Show Atom where 
  show (Symbolic s) = s
  show (Quoted s) = "'" ++ s ++ "'"


instance Show Variable where 
  show (Named x _) = x
  show Anonymous = "_"


instance Show Const where 
  show (Atom a) = show a 
  show (Number x) = show x
  show (String s) = "\"" ++ s ++ "\"" 

instance Show Term where 
  show (Const c) = show c
  show (Variable v) = show v 
  show Cut = "!"
  show (CompoundTerm a ts) = show a ++ "(" ++ (intercalate "," . map show $ ts) ++ ")"
-- data Clause 
--   = Clause 
--   { head :: Term
--   , body :: Body
--   }


-- instance Show Clause where 
--   show Clause{..} = show head ++
--     if emptyBody body then "."
--     else ":-" ++ show body ++ "."


-- newtype Program 
--   = Program [Clause]

-- instance Show Program where
--   show (Program clauses) = intercalate "\n" . map show $ clauses


