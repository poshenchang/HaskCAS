{-# LANGUAGE FlexibleInstances #-}
module HaskCAS.Simplify where

import Data.Monoid hiding (Sum)
import Control.Applicative hiding (Const)
import Data.List (sort)

import HaskCAS.Core

-- TODO:
-- constant folding
-- identity rules
-- zero rules
-- term collection
-- expansion

newtype SumList a = Sum [a]
    deriving (Show, Eq)

instance Semigroup (SumList Expr) where
    (Sum x) <> (Sum y) = Sum (x ++ y)

instance Monoid (SumList Expr) where
    mempty = Sum []

instance Functor SumList where
    fmap f (Sum x) = Sum (map f x)

instance Applicative SumList where
    pure x = Sum [x]
    (Sum fs) <*> (Sum xs) = Sum (fs <*> xs)

toSumList :: Expr -> SumList Expr
toSumList (Const x)  = Sum [Const x]
toSumList (Var x)    = Sum [Var x]
toSumList (Negate x) = Mul (Const (-1)) <$> toSumList x
toSumList (Add x y)  = toSumList x <> toSumList y
toSumList (Sub x y)  = toSumList x <> toSumList (Negate y)
toSumList (Mul x y)  = Mul <$> toSumList x <*> toSumList y
toSumList (Div x y)  = (`Div` y) <$> toSumList x

instance Ord Expr where
    compare (Const x) (Const y) = compare x y
    compare (Var x) (Var y) = compare x y
    compare (Add x1 x2) (Add y1 y2) = compare (x1, x2) (y1, y2)
    compare (Sub x1 x2) (Sub y1 y2) = compare (x1, x2) (y1, y2)
    compare (Mul x1 x2) (Mul y1 y2) = compare (x1, x2) (y1, y2)
    compare (Div x1 x2) (Div y1 y2) = compare (x1, x2) (y1, y2)
    compare (Var _) _ = LT
    compare _ (Var _) = GT
    compare (Const _) _ = LT
    compare _ (Const _) = GT
    compare (Negate _) _ = LT
    compare _ (Negate _) = GT
    compare (Add _ _) _ = LT
    compare _ (Add _ _) = GT
    compare (Sub _ _) _ = LT
    compare _ (Sub _ _) = GT
    compare (Mul _ _) _ = LT
    compare _ (Mul _ _) = GT

sortSumList :: Ord a => SumList a -> SumList a
sortSumList (Sum exps) = Sum (sort exps)