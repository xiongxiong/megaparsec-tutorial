{-# LANGUAGE OverloadedStrings, TypeApplications #-}

module Observing where

import Control.Application hiding (some)
import Data.List (intercalate)
import Data.Set (Set)
import Data.Text (Text)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Data.Set as Set

data Custom
    = TrivialWithLocation
      [String] -- position stack
      (Maybe (ErrorItem Char))
      (Set (ErrorItem Char))
    | FancyWithLocation
      [String] -- position stack
      (ErrorFancy Void) -- Void, because we do not want to allow to nest Customs
    deriving (Eq, Ord, Show)

instance ShowErrorComponent Custom where
    showErrorComponent (TrivialWithLocation stack us es) =
        parseErrorTextPretty (TrivialError @Text @Void undefined us es)
