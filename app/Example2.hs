{-# LANGUAGE OverloadedStrings #-}

module Example2 (main) where

import Control.Applicative
import Control.Monad.Identity
import Control.Monad.State.Strict
import Data.Text (Text)
import Data.Void
import Text.Megaparsec hiding (State)

type Parser = StateT String (ParsecT Void Text Identity)

parser :: Parser String
parser = a <|> b
    where
        a = "foo" <$ put "branch A" <* empty
        b = get <* put "branch B"

main :: IO ()
main = do
    let p = runStateT parser "initial"
        Right (a, s) = runParser p "" ""

    putStrLn ("Result: " ++ show a)
    putStrLn ("Final state: " ++ show s)