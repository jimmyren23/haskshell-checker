module Main where

import Parsing
import Suggestions
import System.Environment (getArgs)

main = do
  fileName <- head getArgs
  parseFromFile bashFileP fileName
