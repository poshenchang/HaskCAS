module HaskCAS.Parser.Spec (spec) where

import Test.Hspec
import HaskCAS.Parser

import Data.Char (isAlpha, isAlphaNum)

-- can parse nonnegative integer constants
propConst :: Integer -> Bool
propConst n | n >= 0    = parseExpr (show x) == Right (Const x)
            | otherwise = True

-- can parse variables
propVar :: String -> Bool
propVar s@(x:_) | not (all isAlphaNum s) = True
                | not isAlpha x          = True
                | otherwise              = parseExpr s == Right (Var s)

spec :: Spec
spec = do
  describe "parseExpr" $ do
    it "can parse nonnegative integer constants" $ do
      property $ propConst
    it "can parse variables" $ do
      property $ propVar