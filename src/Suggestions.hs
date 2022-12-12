module Suggestions where

import Parsing
import ShellSyntax
import System.IO qualified as IO
import System.IO.Error qualified as IO
import GHC.Base (undefined)

-- | Parses a File using the parser and outputs the warnings
parseFromFile :: Parser a -> String -> IO ()
parseFromFile parser filename = undefined

