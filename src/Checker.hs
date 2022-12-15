module Checker where

import Data.Map
import Data.Map qualified as Map
import Parsing
import PrettyPrint (pretty)
import ShellParsing qualified as S
import ShellSyntax

{- Quoting -}

-- | Checks if a variable is quoted
checkUnquotedVar :: Var -> Either String Var
checkUnquotedVar = undefined

-- | Checks if tilde is used in quotes
checkQuotedTildeExpansion :: BashCommand -> Either String BashCommand
checkQuotedTildeExpansion (ExecCommand cmd args) = checkQuotedTildeExpansionArgs (ExecCommand cmd args) args
checkQuotedTildeExpansion cmd = Right cmd

checkQuotedTildeExpansionArgs :: BashCommand -> [Arg] -> Either String BashCommand
checkQuotedTildeExpansionArgs cmd args =
  case args of
    [] -> Right cmd
    (a : as) -> 
      case a of
        Arg x -> if x == "<tilde>" then Left "Tilde expansion can't be used in strings" else checkQuotedTildeExpansionArgs cmd as
        SingleQuote args2 -> do
          checkQuotedTildeExpansionArgs cmd args2
          args <- checkQuotedTildeExpansionArgs cmd as
          return cmd
        DoubleQuote args2 -> do
          checkQuotedTildeExpansionArgs cmd args2
          args <- checkQuotedTildeExpansionArgs cmd as
          return cmd

-- | Checks if single quotes are closed by apostrophe
checkSingleQuoteApostrophe :: Value -> Either String Value
checkSingleQuoteApostrophe = undefined

-- | Checks if apostrophe is escaped inside sinlge quotes
checkEscapeQuote :: Value -> Either String Value
checkEscapeQuote = undefined

-- | Checks if variables are used in single quotes
-- checkVarInSingleQuotes :: [Arg] -> Map Var (String, Bool) -> BashCommand -> Either String [Arg]
-- checkVarInSingleQuotes (x1 : xs) history cmd =
--   case x1 of
--     Arg x ->
--       case parse S.variableRef x of
--         Left error -> Left ("Error: " ++ error)
--         Right possVar ->
--           let var = V possVar
--             in case Map.lookup var history of
--                 Nothing -> Left ("Error: " ++ possVar ++ " is not assigned")
--                 Just (s, False) -> Left ("Did you mean to assign variable " ++ pretty var ++ "  when you wrote: " ++ s ++ "? It was used later in: " ++ pretty cmd)
--                 Just _ -> do
--                   args <- checkVarInSingleQuotes xs history cmd
--                   return (Arg x : args)
--     SingleQuote (a : as) ->
--       case parse S.variableRef a of
--         Left error -> checkVarInSingleQuotes xs history cmd
--         Right possVar -> Left "Variables aren't allowed in single quotes"
--     _ -> checkVarInSingleQuotes xs history cmd

-- checkVarInSingleQuotes [] _  _ = Right []


{- Conditionals -}

-- | Checks if equal-to is missing spaces around itself
checkMissingSpaces :: Expression -> Maybe String
checkMissingSpaces = undefined

-- | Checks if Expression to see if conditional is implicitly always true through mislabeled expression
checkLiteralVacuousTrue :: Expression -> Either String Expression
checkLiteralVacuousTrue = undefined

-- | Checks if Quoted regex in =~
checkQuotedRegex :: Expression -> Either String Value
checkQuotedRegex = undefined

-- | Checks if unsupported operators are used
-- TODO: Define a list of supported operators in ShellSyntax.hs
checkUnsupportedOperators :: Expression -> Either String Expression
checkUnsupportedOperators = undefined

-- | Check if strings and numbers are being compared
checkNumericalStrComparison :: Expression -> Either String Expression
checkNumericalStrComparison = undefined

-- | Checks that && is not used inside [] statement
checkAnd :: Expression -> Either String Expression
checkAnd = undefined

-- | Checks if test operators are used in ((..))
checkTestOperators :: Expression -> Either String Expression
checkTestOperators = undefined

-- | Checks if piping and backgrounding are part of condition
checkBackgroundingAndPiping :: Expression -> Either String Expression
checkBackgroundingAndPiping = undefined

{- Freq Misused Commands -}

-- | Checks if sudo is being redirected
checkRedirectInSudo :: BashCommand -> Either String BashCommand
checkRedirectInSudo = undefined

-- | Checks if aliases are defined with arguments
checkArgumentsInAliases :: BashCommand -> Either String BashCommand
checkArgumentsInAliases = undefined

-- | Checks if redirections are in find
checkRedirectionInFind :: BashCommand -> Either String BashCommand
checkRedirectionInFind = undefined

{- Beginner Mistakes -}

-- | Checks if extra spaces are used in assignments
-- | This checker is run later if the var gets used
-- | ec : execCommand that var is used in
-- checkSpacesInAssignments :: BashCommand -> Either String BashCommand
-- checkSpacesInAssignments ec =
--   case comm of
--     PossibleAssign var exp -> Left "Did you mean to assign variable " ++ var ++ "  when you wrote: " ++ pretty possibleAssign ++ "? It was used later in: " ++ pretty exp
--     _ -> Right ec

-- | Checks if dollar sign is present in assignments
checkDollarSignInAssignments :: BashCommand -> Either String BashCommand
checkDollarSignInAssignments = undefined

-- | Cheecks if elements in array are separated by commas
checkCommaSeparatedArrays :: BashCommand -> Either String BashCommand
checkCommaSeparatedArrays = undefined

-- | Checks if 'else if' is used
checkElseIf :: BashCommand -> Either String BashCommand
checkElseIf = undefined

-- | Checks if function has not been defined previously
checkUndefinedFunction :: BashCommand -> Either String BashCommand
checkUndefinedFunction = undefined

-- | Checks if only false is present conditional expression
checkSingleFalse :: BashCommand -> Either String BashCommand
checkSingleFalse = undefined

-- | Checks if (...) is used instead of test operator
checkParenthesisInsteadOfTest :: BashCommand -> Either String BashCommand
checkParenthesisInsteadOfTest = undefined

{- Style -}

-- | Checks if `` is used to interpolate instead of $()
checkCommandSubstitution :: BashCommand -> Either String BashCommand
checkCommandSubstitution = undefined

-- | Checks if outdated $[] is used instead of standard $((..))
checkArithmeticParentheses :: BashCommand -> Either String BashCommand
checkArithmeticParentheses = undefined

-- | Checks if $ is used for variables in $((..))
checkNoVarInArithemetic :: BashCommand -> Either String BashCommand
checkNoVarInArithemetic = undefined

-- | Checks if echo is unnecessarily used
checkEchoUsage :: BashCommand -> Either String BashCommand
checkEchoUsage = undefined

-- | Checks if cat is unnecessarily used
checkCatUsage :: BashCommand -> Either String BashCommand
checkCatUsage = undefined

{- Data and typing errors -}

-- | Checks if arrays are assigned to strings
checkArrayAssignAsString :: BashCommand -> Either String BashCommand
checkArrayAssignAsString = undefined -- \$@ -> Used to access bash command line args array

-- | Checks if arrays are referencd as strings
checkArrayReferenceInString :: BashCommand -> Either String BashCommand
checkArrayReferenceInString = undefined

-- | Checks if arrays and strings are being concatenated
checkStringArrayConcatenation :: BashCommand -> Either String BashCommand
checkStringArrayConcatenation = undefined

-- | Checks if numbers are being compared as strings
checkStringNumericalComparison :: BashCommand -> Either String BashCommand
checkStringNumericalComparison = undefined -- \$# retrives # of params passed in, [str] > [str]

-- | Checks if variables are being attempted to be used incorrectly - user intends to use it but does not do so correctly using $
checkUnusedVar :: BashCommand -> Either String Command
checkUnusedVar = undefined

-- | How to print original command for possible assigns?
-- | 1. Store in history map as a string in its original form
-- | 2. Different types of possible assign
-- | 3. Store = as part of var string
checkArg :: [Arg] -> Map Var (String, Bool) -> BashCommand -> Either String [Arg]
checkArg (x : xs) history cmd = 
  case x of
    Arg a -> 
      case parse S.variableRef a of
      Left error -> Left ("Error: " ++ error)
      Right possVar ->
        let var = V possVar
        in case Map.lookup var history of
              Nothing -> Left ("Error: " ++ possVar ++ " is not assigned")
              Just (s, False) -> Left ("Did you mean to assign variable " ++ pretty var ++ "  when you wrote: " ++ s ++ "? It was used later in: " ++ pretty cmd)
              Just _ -> do
                args <- checkArg xs history cmd
                return (x : args)
    SingleQuote (arg : as) ->
      let Arg a = arg in
         case parse S.variableRef a of
          Left error -> 
            do 
              checkArg as history cmd
              args <- checkArg xs history cmd
              return (x : args)
          _ -> Left "Variables cannot be used inside single quotes."
    DoubleQuote (arg : as) -> 
      let Arg a = arg in
        case parse S.variableRef a of
          Left error -> Left ("Error: " ++ error)
          Right possVar ->
            let var = V possVar
            in case Map.lookup var history of
                  Nothing -> Left ("Error: " ++ possVar ++ " is not assigned")
                  Just (s, False) -> Left ("Did you mean to assign variable " ++ possVar ++ "  when you wrote: " ++ s ++ "? It was used later in: " ++ pretty cmd)
                  Just _ -> 
                     do 
                      checkArg as history cmd
                      args <- checkArg xs history cmd
                      return (x : args)
    _ -> checkArg xs history cmd
checkArg [] _  _ = Right []

-- | Checks if undefined variables are being used
checkUnassignedVar :: BashCommand -> Map Var (String, Bool) -> Either String BashCommand
checkUnassignedVar command@(ExecCommand cmd (x : xs)) history = do
  args <- checkArg (x : xs) history command
  return (ExecCommand cmd args)
checkUnassignedVar cmd _ = Right cmd -- for other types like assignments, skip.

-- >>> checkUnassignedVar (ExecCommand (ExecName "echo") ["$x"]) Map.empty
-- Left "Error: x is not assigned"

-- -- # Assignments in subshells
-- checkAssignmentInSubshell :: BashCommand -> Either String Command
-- checkAssignmentInSubshell = undefined

-- | Checks if commands that don't read are being piped
checkPipingRead :: BashCommand -> Either String Command
checkPipingRead = undefined

-- | Checks if argument count doesn't match in printf
checkPrintArgCount :: BashCommand -> Either String Command
checkPrintArgCount = undefined

-- | Checks if word boundaries are lost in array eval
checkArrayEval :: BashCommand -> Either String Command
checkArrayEval = undefined -- [@] -> treats each element as a separate command by default

-- TODO: Need to define for-loop type first

-- | Checks if array value is being used as a key
-- for i in "${x[@]}";
--    do ${x[$i]}
-- done
checkArrayValueUsedAsKey :: BashCommand -> Either String Command
checkArrayValueUsedAsKey = undefined

{- Robustness -}

-- | Checks if vaiables are used in printf
checkNoVariablesInPrintf :: BashCommand -> Either String Command
checkNoVariablesInPrintf = undefined
