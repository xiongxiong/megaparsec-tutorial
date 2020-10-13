{-# LANGUAGE OverloadedStrings, TupleSections #-}

module Indent where

import Control.Applicative hiding (some)
import Control.Monad (void)
import Data.Text (Text, unpack)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set

type Parser = Parsec Custom Text

lineComment :: Parser ()
lineComment = L.skipLineComment "#"

scn :: Parser ()
scn = L.space space1 lineComment empty

sc :: Parser ()
sc = L.space (void $ some (char ' ' <|> char '\t')) lineComment empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

pComplexItem :: Parser (String, [String])
pComplexItem = L.indentBlock scn p
    where
        p = do
            header <- pItem
            return (L.IndentMany Nothing (return . (header, )) pLineFold)

pLineFold :: Parser String
pLineFold = L.lineFold scn $ \sc' ->
    let ps = some (alphaNumChar <|> char '-') `sepBy1` try sc'
    in unwords <$> ps <* scn

pItemList :: Parser (String, [(String, [String])])
pItemList = L.nonIndented scn (L.indentBlock scn p)
    where
        p = do
            header <- pItem
            -- return (L.IndentMany Nothing (return . (header, )) pItem)
            -- return (L.IndentSome (Just (mkPos 5)) (return . (header, )) pItem)
            return (L.IndentSome Nothing (return . (header, )) pComplexItem)

pItem :: Parser String
pItem = lexeme (some (alphaNumChar <|> char '-')) <?> "list item"

unfortunateParser :: Parser ()
unfortunateParser = failure (Just EndOfInput) (Set.fromList es)
    where
        es = [Tokens (NE.fromList "a"), Tokens (NE.fromList "b")]

incorrectIndent :: MonadParsec e s m => Ordering -> Pos -> Pos -> m a
incorrectIndent ord ref actual = fancyFailure . Set.singleton $ ErrorIndentation ord ref actual

data Custom = NotKeyword Text
    deriving (Eq, Show, Ord)

instance ShowErrorComponent Custom where
    showErrorComponent (NotKeyword txt) = unpack txt ++ " is not a keyword"

notKeyword :: Text -> Parser a
notKeyword = customFailure . NotKeyword

