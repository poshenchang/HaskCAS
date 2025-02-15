module HaskCAS.ParserSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import HaskCAS.Core
import HaskCAS.Parser

-- String containing all alphabetic characters
genAlpha :: Gen Char
genAlpha = elements $ ['a'..'z'] ++ ['A'..'Z']

-- String containing all alphanumeric characters
genAlphaNum :: Gen Char
genAlphaNum = elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']

-- can parse nonnegative integer constants
prop_Const :: Integer -> Property
prop_Const n = n >= 0 ==> parseExpr (show n) == Right (Const n)

-- can parse variables
prop_Var :: String -> Bool
prop_Var s = parseExpr s == Right (Var s)

-- generator for strings of legal formats for variables
gen_Var :: Gen String
gen_Var = do
  x <- genAlpha
  xs <- listOf genAlphaNum
  return (x:xs)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "parseExpr" $ do
    it "can parse nonnegative integer constants" $ do
      property $ prop_Const
    it "can parse variables" $ do
      property $ forAll gen_Var prop_Var