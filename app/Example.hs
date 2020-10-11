{-# LANGUAGE OverloadedStrings #-}

module Example (main) where

import Control.Applicative ( Alternative(empty, (<|>)) )
import Control.Monad.State.Strict
    ( MonadState(get, put), runState, State )
import Data.Text (Text)
import Data.Void ( Void )
import Text.Megaparsec ( runParserT, ParsecT )

type Parser = ParsecT Void Text (State String)

parser0 :: Parser String
parser0 = a <|> b
    where
        a = "foo" <$ put "branch A"
        b = get <* put "branch B"

parser1 :: Parser String
parser1 = a <|> b
    where
        a = "foo" <$ put "branch A" <* empty
        b = get <* put "branch B"

main :: IO ()
main = do
    let run p = runState (runParserT p "" "") "initial"
        (Right a0, s0) = run parser0
        (Right a1, s1) = run parser1

    putStrLn "Parser 0"
    putStrLn ("Result: " ++ show a0)
    putStrLn ("Final state: " ++ show s0)

    putStrLn "Parser 1"
    putStrLn ("Result: " ++ show a1)
    putStrLn ("Final state: " ++ show s1)
