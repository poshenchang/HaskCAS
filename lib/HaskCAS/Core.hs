{-# LANGUAGE InstanceSigs #-}
module HaskCAS.Core where

-- TODO: add power and functions
data Expr
    = Var String
    | Const Integer
    | Add Expr Expr
    | Sub Expr Expr
    | Mul Expr Expr
    | Div Expr Expr
    | Negate Expr
    deriving (Eq)

-- TODO: implement elementary functions
-- data Func
--     = Sin
--     | Cos
--     | Log
--     | Exp
--     deriving (Eq, Show)

-- precedence of each operator
-- Negate: 0
-- Add, Sub: 10
-- Mul, Div: 20
-- Func: 30
-- Pow: 40
instance Show Expr where
    showsPrec :: Int -> Expr -> ShowS
    showsPrec _ (Var s) = showString s
    showsPrec _ (Const a) = shows a
    showsPrec d (Negate a) = showParen (d > 0) $
        showString "-" . showsPrec 1 a
    showsPrec d (Add a b) = showParen (d > 10) $
        showsPrec 11 a . showString "+" . showsPrec 11 b
    showsPrec d (Sub a b) = showParen (d > 10) $
        showsPrec 11 a . showString "-" . showsPrec 11 b
    showsPrec d (Mul a b) = showParen (d > 20) $
        showsPrec 21 a . showString "*" . showsPrec 21 b
    showsPrec d (Div a b) = showParen (d > 20) $
        showsPrec 21 a . showString "/" . showsPrec 21 b