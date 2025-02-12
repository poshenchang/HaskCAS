{-# LANGUAGE OverloadedStrings #-}
module HaskCAS.Parser (pExpr, parseExpr) where

import HaskCAS.Core

import Data.Text
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.Combinators.Expr

type Parser = Parsec Void Text

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

symbol :: Text -> Parser Text
symbol = L.symbol space

pVariable :: Parser Expr
pVariable = Var <$> lexeme 
            ((:) <$> letterChar <*> many alphaNumChar <?> "variable")

pInteger :: Parser Expr
pInteger = Const <$> lexeme L.decimal

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

pTerm :: Parser Expr
pTerm = choice
        [ parens pExpr
        , pVariable
        , pInteger
        ]

pExpr :: Parser Expr
pExpr = makeExprParser pTerm operatorTable

parseExpr :: String -> Either (ParseErrorBundle Text Void) Expr
parseExpr = runParser (space *> pExpr <* eof) "" . pack

-- data Operator m a -- N.B.
--   = InfixN  (m (a -> a -> a)) -- ^ Non-associative infix
--   | InfixL  (m (a -> a -> a)) -- ^ Left-associative infix
--   | InfixR  (m (a -> a -> a)) -- ^ Right-associative infix
--   | Prefix  (m (a -> a))      -- ^ Prefix
--   | Postfix (m (a -> a))      -- ^ Postfix

operatorTable :: [[Operator Parser Expr]]
operatorTable =  [ [ prefix "-" Negate
                   ], 
                   [ binary "*" Mul, 
                     binary "/" Div
                   ], 
                   [ binary "+" Add, 
                     binary "-" Sub
                   ]
                 ]

binary :: Text -> (Expr -> Expr -> Expr) -> Operator Parser Expr
binary name f = InfixL (f <$ symbol name)

prefix :: Text -> (Expr -> Expr) -> Operator Parser Expr
prefix name f = Prefix (f <$ symbol name)