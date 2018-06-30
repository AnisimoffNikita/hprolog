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


termToExpression :: Term -> Maybe (Either (Expression Int) (Expression Float))
termToExpression (ConstTerm (Int a)) = Just (Left (Value a))
termToExpression (ConstTerm (Float a)) = Just (Right (Value a))
termToExpression (CompoundTerm func [x, y]) = do 
  f <- M.lookup func binaryIntDB
  g <- M.lookup func binaryFloatDB
  x' <- termToExpression x 
  y' <- termToExpression y 
  case (x', y') of 
    (Left x, Left y) -> return . Left $ Binary f x y
    (Right x, Right y) -> return . Right $ Binary g x y
    (_, _) -> Nothing



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



eval' :: Expression a -> a
eval' (Binary f a b) = f (eval' a) (eval' b)
eval' (Unary f a) = f (eval' a)
eval' (Value a) = a


eval :: Term -> Maybe Term 
eval expr = do 
  expr' <- termToExpression expr 
  case expr' of 
    Left expr -> Just . ConstTerm . Int $ eval' expr
    Right expr -> Just . ConstTerm . Float $ eval' expr


