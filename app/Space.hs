{-# LANGUAGE OverloadedStrings #-}

module Space where

import Data.Text (Text)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.Combinators.Expr 

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

charLiteral :: Parser Char
charLiteral = between (char '\'') (char '\'') L.charLiteral

stringLiteral :: Parser String
stringLiteral = char '\"' *> manyTill L.charLiteral (char '\"')

integer :: Parser Integer
integer = lexeme L.decimal

float :: Parser Double
float = lexeme L.float

signedInteger :: Parser Integer
signedInteger = L.signed sc integer

signedFloat :: Parser Double
signedFloat = L.signed sc float

pKeyword :: Text -> Parser Text
pKeyword keyword = lexeme (string keyword <* notFollowedBy alphaNumChar)

withPredicate1 :: (a -> Bool) -> String -> Parser a -> Parser a
withPredicate1 f msg p = do
    r <- lookAhead p
    if f r
        then p
        else fail msg

withPredicate2 :: (a -> Bool) -> String -> Parser a -> Parser a
withPredicate2 f msg p = do
    o <- getOffset
    r <- p
    if f r
        then return r
        else do
            setOffset o
            fail msg

data Expr = Var String | Int Int | Negation Expr | Sum Expr Expr | Subtr Expr Expr | Product Expr Expr | Division Expr Expr
    deriving (Eq, Ord, Show)

pVariable :: Parser Expr
pVariable = Var <$> lexeme ((:) <$> letterChar <*> many alphaNumChar <?> "variable")

pInteger :: Parser Expr
pInteger = Int <$> lexeme L.decimal

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

pTerm :: Parser Expr
pTerm = choice [ parens pExpr, pVariable, pInteger ]

pExpr :: Parser Expr
pExpr = makeExprParser pTerm operatorTable

operatorTable :: [[Operator Parser Expr]]
operatorTable = [[ prefix "-" Negation, prefix "+" id ], [ binary "*" Product, binary "/" Division ], [binary "+" Sum, binary "-" Subtr ]]

binary :: Text -> (Expr -> Expr -> Expr) -> Operator Parser Expr
binary name f = InfixL (f <$ symbol name)

prefix, postfix :: Text -> (Expr -> Expr) -> Operator Parser Expr
prefix name f = Prefix (f <$ symbol name)
postfix name f = Postfix (f <$ symbol name)

