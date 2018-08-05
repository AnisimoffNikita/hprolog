module Language.Prolog.Bool 
  where 

import           Language.Prolog.Semantics
import qualified Data.Map                      as M

evalBool :: Term -> Maybe Bool 
evalBool (CompoundTerm func [x, y]) = do 
  f <- M.lookup func binaryIntDB
  g <- M.lookup func binaryFloatDB
  case (x, y) of 
    (ConstTerm (Int x), ConstTerm (Int y)) -> Just $ f x y
    (ConstTerm (Float x), ConstTerm (Float y)) -> Just $ g x y
    _ -> Nothing
evalBool _ = Nothing 



binaryIntDB :: (Num a, Eq a, Ord a) => M.Map String (a -> a -> Bool)
binaryIntDB = M.fromList 
  [(">", (>))
  ,("<", (<))
  ,(">=", (>=))
  ,("=<", (<=))
  ,("==", (==))
  ,("\\=", (/=))]

binaryFloatDB :: (Floating a, Eq a, Ord a) => M.Map String (a -> a -> Bool)
binaryFloatDB = M.fromList 
  [(">", (>))
  ,("<", (<))
  ,(">=", (>=))
  ,("=<", (<=))
  ,("==", (==))
  ,("\\=", (/=))]