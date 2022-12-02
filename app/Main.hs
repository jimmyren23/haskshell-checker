module Main where

import Parsing
import Suggestions
import System.Environment (getArgs)

main = do
  args <- getArgs
  let filename = head args
   in parseFromFile bashFileP filename
