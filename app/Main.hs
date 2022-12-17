{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main where

import Parsing
import ShellParsing
import Suggestions
import System.Environment (getArgs)

main :: IO ()
main = do
  putStrLn "\n << Welcome to HaskShell >>"
  putStrLn "Please enter in the relative path to a shell script (or text file) that you would like to check."
  x <- getLine 
  putStrLn "\t"
  evalScript x
  return ()

-- Current Flow
-- Suggestions:
-- Takes in file name
-- reads file and gets it as a string
-- Runs the parser, parseShellScript
-- Looks at the result of the parse, and then checks if there are any errors
-- If there are errors, then it will return the error message
-- If there are no errors, then it will return [BashCommand], [TODO + list of warnings?]
-- blockP
-- Parser that parses a block of bash commands
-- bashCommandP
-- Parser that parses a single bash command
-- First tries to do an assign
-- then it does an assignP
--- possibleAssignP
-- conditionalP - will try to do if [ expresion ] or if [[ expression ]]
-- conditionalP - then, else, fi
-- possibleAssignP - will try to do an assign
--
-- execCommandP - will try to do an exec command