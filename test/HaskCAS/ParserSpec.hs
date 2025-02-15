module HaskCAS.ParserSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import HaskCAS.Core
import HaskCAS.Parser

-- String containing all alphabetic characters
gen_Alpha :: Gen Char
gen_Alpha = elements $ ['a'..'z'] ++ ['A'..'Z']

-- String containing all alphanumeric characters
gen_AlphaNum :: Gen Char
gen_AlphaNum = elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']

-- generator for nonnegative integer constants
gen_Const :: Gen Integer
gen_Const = arbitrarySizedNatural

-- can parse nonnegative integer constants
prop_Const :: Integer -> Bool
prop_Const n = parseExpr (show n) == Right (Const n)

-- generator for strings of legal variable formats
gen_Var :: Gen String
gen_Var = do
  x <- gen_Alpha
  xs <- listOf gen_AlphaNum
  return (x:xs)

-- can parse variables
prop_Var :: String -> Bool
prop_Var s = parseExpr s == Right (Var s)

-- interleave two given lists (truncate according to the shorter list)
interleave :: [a] -> [a] -> [a]
interleave (x:xs) ys = x : (interleave ys xs)
interleave _ _       = []

-- generator for strings of legal expression formats
gen_Expr :: Gen String
gen_Expr = do
  n <- chooseInt (1, 100)
  lits <- vectorOf (n+1) (oneof [show <$> gen_Const, gen_Var])
  ops <- vectorOf n (elements $ ["+", "-", "*", "/"])
  return (concat $ interleave lits ops)

gen_ExprPair :: Gen (String, String)
gen_ExprPair = do
  exp1 <- gen_Expr
  exp2 <- gen_Expr
  return (exp1, exp2)

prop_Add :: (String, String) -> Bool
prop_Add (exp1, exp2) = parseExpr ("(" ++ exp1 ++ ")+(" ++ exp2 ++ ")") 
                        == (Add <$> parseExpr exp1 <*> parseExpr exp2)

prop_Sub :: (String, String) -> Bool
prop_Sub (exp1, exp2) = parseExpr ("(" ++ exp1 ++ ")-(" ++ exp2 ++ ")") 
                        == (Sub <$> parseExpr exp1 <*> parseExpr exp2)

prop_Mul :: (String, String) -> Bool
prop_Mul (exp1, exp2) = parseExpr ("(" ++ exp1 ++ ")*(" ++ exp2 ++ ")") 
                        == (Mul <$> parseExpr exp1 <*> parseExpr exp2)

prop_Div :: (String, String) -> Bool
prop_Div (exp1, exp2) = parseExpr ("(" ++ exp1 ++ ")/(" ++ exp2 ++ ")") 
                        == (Div <$> parseExpr exp1 <*> parseExpr exp2)

prop_Negate :: String -> Bool
prop_Negate exp1 = parseExpr ("-(" ++ exp1 ++ ")") 
                        == (Negate <$> parseExpr exp1)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "parseExpr" $ do
    it "can parse nonnegative integer constants" $ do
      property $ forAll gen_Const prop_Const
    it "can parse variables" $ do
      property $ forAll gen_Var prop_Var
    it "can parse addition" $ do
      property $ forAll gen_ExprPair prop_Add
    it "can parse subtraction" $ do
      property $ forAll gen_ExprPair prop_Sub
    it "can parse multiplication" $ do
      property $ forAll gen_ExprPair prop_Mul
    it "can parse division" $ do
      property $ forAll gen_ExprPair prop_Div
    it "can parse negation" $ do
      property $ forAll gen_Expr prop_Negate