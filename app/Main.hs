{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Main where

import Control.Applicative
import Control.Monad
import Data.Text (Text)
import Data.Void
import Text.Megaparsec hiding (State, some)
import Text.Megaparsec.Char
import Text.Megaparsec.Debug
import qualified Data.Text as T
import qualified Text.Megaparsec.Char.Lexer as L
import Data.List

-- scheme:[//[user:password@]host[:port]][/]path[?query][#fragment]

type Parser = Parsec Void Text

data Scheme = SchemeData | SchemeFile | SchemeFtp | SchemeHttp | SchemeHttps | SchemeIrc | SchemeMailto deriving (Eq, Show)

pScheme :: Parser Scheme
pScheme = choice . ((\(a, b) -> a <$ string b) <$>) . reverse . sortOn snd $ [(SchemeData, "data"), (SchemeFile, "file"), (SchemeFtp, "ftp"), (SchemeHttp, "http"), (SchemeHttps, "https"), (SchemeIrc, "irc"), (SchemeMailto, "mailto")]

data Authority = Authority { authUser :: Maybe (Text, Text), authHost :: Text, authPort :: Maybe Int } deriving (Eq, Show)

data Uri = Uri { uriScheme :: Scheme, uriAuthority :: Maybe Authority, uriPath :: Text, uriQuery :: Maybe Text, uriSegment :: Maybe Text } deriving (Eq, Show)

pUri :: Parser Uri
pUri = do
    uriScheme <- dbg "scheme" pScheme <?> "valid scheme"
    char ':'
    uriAuthority <- dbg "auth" . optional $ do
        string "//"
        authUser <- dbg "user" . optional . try $ do
            user <- T.pack <$> some alphaNumChar <?> "username"
            char ':'
            password <- T.pack <$> some alphaNumChar <?> "password"
            char '@'
            return (user, password)
        authHost <- T.pack <$> dbg "host" (some (alphaNumChar <|> char '.')) <?> "hostname"
        authPort <- dbg "port" $ optional (char ':' *> label "port number" L.decimal)
        return Authority {..}
    return Uri {..}

main :: IO ()
main = undefined

alternatives :: Parser (Char, Char)
alternatives = foo <|> bar
    where
        foo = (,) <$> char 'a' <*> char 'b'
        bar = (,) <$> char 'a' <*> char 'c'
