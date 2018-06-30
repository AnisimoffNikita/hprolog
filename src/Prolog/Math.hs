{-# LANGUAGE RankNTypes #-}
module Prolog.Math where

import           Prolog.Semantics
import qualified Data.Map                      as M
-- import           Data.Bifunctor
-- import           Data.Biapplicative

type BinFunc a = a -> a -> a
type UnFunc a = a -> a

data Expression a
  = Binary (BinFunc a) (Expression a) (Expression a)
  | Unary (UnFunc a) (Expression a)
  | Value a

unaryIntDB :: (Num a) => M.Map String (a -> a)
unaryIntDB = M.fromList 
  [("abs", abs)]

unaryFloatDB :: (Floating a) => M.Map String (a -> a)
unaryFloatDB = M.fromList 
  [("sin", sin)]

binaryIntDB :: (Num a) => M.Map String (a -> a -> a)
binaryIntDB = M.fromList 
  [("+", (+))
  ,("-", (-))
  ,("*", (*))]

binaryFloatDB :: (Floating a) => M.Map String (a -> a -> a)
binaryFloatDB = M.fromList 
  [("+", (*))
  ,("-", (-))
  ,("*", (*))]

termToExpressionInt :: Term -> Maybe (Expression Int)
termToExpressionInt (CompoundTerm func [x, y]) =
  Binary 
    <$> M.lookup func binaryIntDB
    <*> termToExpressionInt x 
    <*> termToExpressionInt y
termToExpressionInt (CompoundTerm func [x]) =
  Unary 
    <$> M.lookup func unaryIntDB
    <*> termToExpressionInt x 
termToExpressionInt (ConstTerm (Int   x)) = Just (Value x)
termToExpressionInt (ConstTerm (Float x)) = Nothing
termToExpressionInt _ = Nothing


termToExpressionFloat :: Term -> Maybe (Expression Float)
termToExpressionFloat (CompoundTerm func [x, y]) =
  Binary 
    <$> M.lookup func binaryFloatDB
    <*> termToExpressionFloat x 
    <*> termToExpressionFloat y
termToExpressionFloat (CompoundTerm func [x]) =
  Unary 
    <$> M.lookup func unaryFloatDB
    <*> termToExpressionFloat x 
termToExpressionFloat (ConstTerm (Int   x)) = Nothing
termToExpressionFloat (ConstTerm (Float x)) = Just (Value x)
termToExpressionFloat _ = Nothing


eval' :: Expression a -> a
eval' (Binary f a b) = f (eval' a) (eval' b)
eval' (Unary f a) = f (eval' a)
eval' (Value a) = a


eval :: Term -> Maybe Term 
eval expr = 
  case (termToExpressionFloat expr, termToExpressionInt expr) of 
    (Just expr, _) -> Just . ConstTerm . Float . eval' $ expr
    (_, Just expr) -> Just . ConstTerm . Int . eval' $ expr
    (_, _) -> Nothing

