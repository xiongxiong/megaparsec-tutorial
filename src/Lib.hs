{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Text (Text)
import Data.Void

type Parser = Parsec Void Text

token :: MonadParsec e s m => (Token s -> Maybe a) -> Set (ErrorItem (Token s)) -> m a

satisfy :: MonadParsec e s m => (Token s -> Bool) -> m (Token s)

single :: MonadParsec e s m => Token s -> m (Token s)
single t = token testToken expected
    where
        testToken x = if x == t then Just x else Nothing
        expected = Set.singleton (Tokens (t:|[]))

newline :: (MonadParsec e s m, Token s ~ Char) => m (Token s)
newline = single '\n'

tokens :: MonadParsec e s m => (Tokens s -> Tokens s -> Bool) -> Tokens s -> m (Tokens s)

chunk :: MonadParsec e s m => Tokens s -> m (Token s)
chunk = tokens (==)

string' :: (MonadParsec e s m, Data.CaseInsensitive.FoldCase (Tokens s)) => Tokens s -> m (Tokens s)
string' = tokens ((==) `on` Data.CaseInsensitive.mk)

mySequence :: Parser (Char, Char, Char)
mySequence =
    (,,) <$> char 'a' <*> char 'b' <*> char 'c'

someFunc :: IO ()
someFunc = putStrLn "someFunc"

