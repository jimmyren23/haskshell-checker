{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main where

import Control.Monad (unless)
import Parsing
import ShellParsing
import Suggestions
import System.Environment (getArgs)

userInput :: IO ()
userInput = do
  putStr "\n"
  putStrLn "++ Please enter in the path to a shell script (or text file) that you would like to check."
  putStrLn "** Path must be relative from the root directory of this project."
  x <- getLine
  unless (x == "q") $ do
    evalScript x
    userInput

main :: IO ()
main = do
  putStrLn "\n<< Welcome to HaskShell Checker >>"
  userInput
