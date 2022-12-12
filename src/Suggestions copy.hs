module Suggestions where

import Data.Map
import GHC.Base (undefined)
import Parsing
import ShellSyntax
import System.IO qualified as IO
import System.IO.Error qualified as IO

-- | Parses a File using the parser and outputs the warnings
parseFromFile :: Parser a -> String -> IO ()
parseFromFile parser filename = undefined

-- | Need to both evaluate each line and hold a history at the same time
-- | State is a Map of Var to Expression
type History = Map Var Expression



-- | Reads line of strings and outputs the warnings
parseByLine :: Parser a -> [String] -> IO ()
parseByLine parser = undefined