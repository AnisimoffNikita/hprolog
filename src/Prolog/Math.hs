{-# LANGUAGE RankNTypes #-}
module Prolog.Math where

import           Prolog.Semantics
import qualified Data.Map                      as M
-- import           Data.Bifunctor
-- import           Data.Biapplicative



eval :: Term -> Maybe Term 
eval expr = do 
  expr' <- termToValue expr 
  case expr' of 
    Left expr -> Just . ConstTerm . Int $ expr
    Right expr -> Just . ConstTerm . Float $ expr

termToValue :: Term -> Maybe (Either Int Float)
termToValue (ConstTerm (Int a)) = Just (Left a)
termToValue (ConstTerm (Float a)) = Just (Right a)
termToValue (CompoundTerm func [x, y]) = do 
  f <- M.lookup func binaryIntDB
  g <- M.lookup func binaryFloatDB
  x' <- termToValue x 
  y' <- termToValue y 
  case (x', y') of 
    (Left x, Left y) -> return . Left $ f x y
    (Right x, Right y) -> return . Right $ g x y
    (_, _) -> Nothing

termToValue (CompoundTerm func [x]) = do 
  f <- M.lookup func unaryIntDB
  g <- M.lookup func unaryFloatDB
  x' <- termToValue x 
  case x' of 
    Left x -> return . Left $ f x
    Right x -> return . Right $ g x



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




