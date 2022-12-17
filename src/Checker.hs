module Checker where

import Data.Map
import Data.Map qualified as Map
import Parsing
import PrettyPrint (pretty)
import ShellParsing qualified as S
import ShellSyntax

-- | Finds the first result with error
eitherOp :: Either String a -> Either String a -> Either String a
eitherOp (Left err1) _ = Left err1
eitherOp _  (Left err2) = Left err2
eitherOp (Right cmd) _  = Right cmd

{- Quoting -}

-- | Checks if a variable is quoted (warning)
checkUnquotedVar :: Var -> Either String Var
checkUnquotedVar = undefined

-- | DONE - Checks if tilde is used in quotes

checkQuotedTildeExpansionTokens :: Token -> Either String [Token]
checkQuotedTildeExpansionTokens token =
    if token == "<tilde>" then Left "Tilde expansion can't be used in strings" else Right [token]

-- | Checks if single quotes are closed by apostrophe
checkSingleQuoteApostrophe :: Value -> Either String Value
checkSingleQuoteApostrophe = undefined

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

-- DONE
-- -- | Checks if dollar sign is present in assignments
-- checkDollarSignInAssignments :: BashCommand -> Either String BashCommand
-- checkDollarSignInAssignments = undefined

-- DONE
-- -- | Cheecks if elements in array are separated by commas
-- checkCommaSeparatedArrays :: BashCommand -> Either String BashCommand
-- checkCommaSeparatedArrays = undefined

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

checkVarInSingleQuotes :: Token -> Either String [Token]
checkVarInSingleQuotes t =
  case parse S.variableRef t of
        Left error -> Right [t]
        Right _ -> Left "Variables cannot be used inside single quotes."

checkEscapeInSingleQuotes :: Token -> Either String [Token]
checkEscapeInSingleQuotes t =
  if t == "<escape>" then Left "Escape cannot be used in single quotes" else Right [t]

checkArgSingleQuotes :: [Token] -> Map Var BashCommand -> Either String [Token]
checkArgSingleQuotes tokens@(t : ts) history =
  let res = checkVarInSingleQuotes t `eitherOp` checkQuotedTildeExpansionTokens t `eitherOp` checkEscapeInSingleQuotes t in
    case res of
      Left err -> Left err
      Right tt -> do
        tokenss <- checkArgSingleQuotes ts history
        return (tt ++ tokenss)
checkArgSingleQuotes [] _  = Right []

checkArgDoubleQuotes :: [Token] -> Map Var BashCommand -> BashCommand -> Either String [Token]
checkArgDoubleQuotes tokens@(t : ts) history cmd =
   case parse S.variableRef t of
          Left error -> Right tokens
          Right var ->
            let V possVar = var
            in case Map.lookup var history of
                  Nothing -> Left ("Error: " ++ possVar ++ " is not assigned")
                  Just (PossibleAssign pa) -> Left ("Did you mean to assign variable " ++ possVar ++ "  when you wrote: " ++ pretty pa ++ "? It was used later in: " ++ pretty cmd)
                  Just _ ->
                     do
                      tokenss <- checkArgDoubleQuotes ts history cmd
                      return (t : tokenss)
checkArgDoubleQuotes [] _ _  = Right []


-- | How to print original command for possible assigns?
-- | 1. Store in history map as a string in its original form
-- | 2. Different types of possible assign
-- | 3. Store = as part of var string
checkArg :: [Arg] -> Map Var BashCommand -> BashCommand -> Either String [Arg]
checkArg args@(x : xs) history cmd =
  case x of
    Arg a ->
      case parse S.variableRef a of
        Left error -> Right args
        Right var ->
          let V possVar = var
          in case Map.lookup var history of
                Nothing -> Left (possVar ++ " is not assigned")
                Just (PossibleAssign pa) -> Left ("Did you mean to assign variable " ++ pretty var ++ "  when you wrote: " ++ pretty pa ++ "? It was used later in: " ++ pretty cmd)
                Just _ -> do
                  args <- checkArg xs history cmd
                  return (x : args)
    SingleQuote tokens -> do
      checkArgSingleQuotes tokens history
      rArgs <- checkArg xs history cmd
      return (x : rArgs)
    DoubleQuote tokens -> do
      checkArgDoubleQuotes tokens history cmd
      rArgs <- checkArg xs history cmd
      return (x : rArgs)
checkArg [] _  _ = Right []

checkExecCommandArgs :: BashCommand -> Map Var BashCommand -> Either String BashCommand
checkExecCommandArgs command@(ExecCommand cmd (x : xs)) history = do
  args <- checkArg (x : xs) history command
  return (ExecCommand cmd args)
checkExecCommandArgs cmd _ = Right cmd -- for other types like assignments, skip.

checkVarInExp :: IfExpression -> Map Var BashCommand -> IfExpression -> Either String IfExpression
checkVarInExp exp history fullExp =
  case exp of
    IfVar var ->
      let V possVar = var in
      case Map.lookup var history of
        Nothing -> Left ("Error: " ++ possVar ++ " is not assigned")
        Just (PossibleAssign pa) -> Left ("Did you mean to assign variable " ++ pretty var ++ "  when you wrote: " ++ pretty pa ++ "? It was used later in: " ++ pretty fullExp)
        Just _ -> Right fullExp
    IfOp1 _ exp -> checkVarInExp exp history fullExp
    IfOp2 exp _ _ -> checkVarInExp exp history fullExp
    _ -> Right fullExp

checkNumericalCompStrInExp :: IfExpression -> Either String IfExpression
checkNumericalCompStrInExp exp =
  case exp of
    IfOp2 (IfVal (StringVal _)) op _ -> if op `elem` numOps then Left (pretty op ++ pretty " is for numerical comparisons.") else Right exp
    IfOp2 _ op (IfVal (StringVal _)) -> if op `elem` numOps then Left (pretty op ++ pretty " is for numerical comparisons.") else Right exp
    _ -> Right exp

checkAndInExp :: IfExpression -> Either String IfExpression
checkAndInExp exp =
  case exp of
    IfOp2 _ AndIf _ -> Left (pretty And ++ " cannot be used inside [...] or [[...]].")
    _ -> Right exp

checkConditionalSt :: BashCommand -> Map Var BashCommand -> Either String BashCommand
checkConditionalSt cmd@(Conditional exp (Block b1) (Block b2)) history =
  do
    checkVarInExp exp history exp `eitherOp` checkNumericalCompStrInExp exp `eitherOp` checkAndInExp exp
    return cmd
checkConditionalSt cmd _ = Right cmd





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
