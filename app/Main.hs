module Main where

import Parsing
import ShellParsing
import Suggestions
import System.Environment (getArgs)

main = undefined

-- main = do
--   args <- getArgs
--   let filename = head args
--    in parseFromFile bashFileP filename

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