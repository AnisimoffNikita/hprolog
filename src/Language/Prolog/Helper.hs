module Language.Prolog.Helper
  (takeWhile')
  where

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' f []       = []
takeWhile' f (x : xs) = if not . f $ x then [x] else x : takeWhile' f xs
