{-# LANGUAGE RankNTypes #-}
module Prolog.Math where 

import Prolog.Semantics 


evaluate :: Term -> Term 
evaluate (CompoundTerm "-" args) = 
  case args of 
    [x@(CompoundTerm _ _), ConstTerm (Int y)] -> 
      case evaluate x of 
        (ConstTerm (Int x)) -> ConstTerm (Int $ (-) x y)
        _ -> error "incompatible types"
    [x@(CompoundTerm _ _), ConstTerm (Float y)] -> 
      case evaluate x of 
        (ConstTerm (Float x)) -> ConstTerm (Float $ (-) x y)
        _ -> error "incompatible types"
    [ConstTerm (Int x), y@(CompoundTerm _ _)] -> 
      case evaluate y of 
        (ConstTerm (Int y)) -> ConstTerm (Int $ (-) x y)
        _ -> error "incompatible types"
    [ConstTerm (Float x), y@(CompoundTerm _ _)] -> 
      case evaluate y of 
        (ConstTerm (Float y)) -> ConstTerm (Float $ (-) x y)
        _ -> error "incompatible types"
    [ConstTerm (Int x), ConstTerm (Int y)] -> ConstTerm (Int $ (-) x y)
    [ConstTerm (Float x), ConstTerm (Float y)] -> ConstTerm (Float $ (-) x y)

    [x@(CompoundTerm _ _), y@(CompoundTerm _ _)] -> 
      case (evaluate x, evaluate y) of 
        (ConstTerm (Int x), ConstTerm (Int y)) -> ConstTerm (Int $ (-) x y)
        (ConstTerm (Float x), ConstTerm (Float y)) -> ConstTerm (Float $ (-) x y)
        _ -> error "incompatible types"
    _ -> error "incompatible types"
evaluate (CompoundTerm "*" args) = 
  case args of 
    [x@(CompoundTerm _ _), ConstTerm (Int y)] -> 
      case evaluate x of 
        (ConstTerm (Int x)) -> ConstTerm (Int $ (*) x y)
        _ -> error "incompatible types"
    [x@(CompoundTerm _ _), ConstTerm (Float y)] -> 
      case evaluate x of 
        (ConstTerm (Float x)) -> ConstTerm (Float $ (*) x y)
        _ -> error "incompatible types"
    [ConstTerm (Int x), y@(CompoundTerm _ _)] -> 
      case evaluate y of 
        (ConstTerm (Int y)) -> ConstTerm (Int $ (*) x y)
        _ -> error "incompatible types"
    [ConstTerm (Float x), y@(CompoundTerm _ _)] -> 
      case evaluate y of 
        (ConstTerm (Float y)) -> ConstTerm (Float $ (*) x y)
        _ -> error "incompatible types"
    [ConstTerm (Int x), ConstTerm (Int y)] -> ConstTerm (Int $ (*) x y)
    [ConstTerm (Float x), ConstTerm (Float y)] -> ConstTerm (Float $ (*) x y)

    [x@(CompoundTerm _ _), y@(CompoundTerm _ _)] -> 
      case (evaluate x, evaluate y) of 
        (ConstTerm (Int x), ConstTerm (Int y)) -> ConstTerm (Int $ (*) x y)
        (ConstTerm (Float x), ConstTerm (Float y)) -> ConstTerm (Float $ (*) x y)
        _ -> error "incompatible types"
    _ -> error "incompatible types"
evaluate _ = error "!"

