module Checker where

import Control.Applicative (Alternative (..))
import Data.Foldable ( Foldable(foldr) )
import Data.Map ( Map )
import Data.Map qualified as Map
import Parsing ( parse, stringP )
import PrettyPrint (pretty)
import ShellParsing qualified as S
import ShellSyntax
    ( numOps,
      arithmeticOps,
      Block(..),
      Bop(And),
      PossibleAssign(PossibleAssignWS),
      Command(ExecName),
      Expression(Var, Arr, Val),
      IfUop(LengthNonZero, LengthZero),
      IfBop(Err, Reg, AndIf),
      Value(StringVal),
      IfExpression(..),
      BashCommand(..),
      Var(..),
      Arg(..),
      Misc(Tilde),
      ArgToken(..), TokenS, Message (None, WarningMessage, ErrorMessage) )
import Test.HUnit.Lang (Result (Error))

-- | Finds the first result with error
eitherOp :: Either Message a -> Either Message a -> Either Message a
eitherOp (Left err1) _ = Left err1
eitherOp _ (Left err2) = Left err2
eitherOp (Right cmd) _ = Right cmd

{- Quoting [4] -}

-- Warnings

-- | Checks if tilde is used in quotes
checkQuotedTildeExpansionTokens :: ArgToken -> Either Message ArgToken
checkQuotedTildeExpansionTokens token@(ArgM t) =
  case t of
    Tilde -> Left (WarningMessage "Tilde expansion can't be used in strings")
    _ -> Right token
checkQuotedTildeExpansionTokens token = Right token

-- | Checks if token could be a variable, considering if its in its history
possVariableRefToken :: String -> Map Var BashCommand -> Bool
possVariableRefToken t history =
  let res = parse S.varP t
   in case res of
        Left _ -> False
        Right var -> Map.member var history

-- | Checks if variable was quoted or not when used
checkUnquotedVar :: Arg -> Map Var BashCommand -> Either Message Arg
checkUnquotedVar token history =
  case token of
    Arg t ->
      if possVariableRefToken t history
        then Left (WarningMessage $ "Variable " ++ t ++ " that was previously used is not quoted")
        else Right token
    _ -> Right token

-- | Checks if single quotes are closed by apostrophe
-- checkSingleQuoteApostrophe :: Arg -> Map Var BashCommand -> Either Message Arg
-- checkSingleQuoteApostrophe (SingleQuote tokens) history = case tokenListSearch tokens history of
--   Left err -> Left err
--   Right _ -> Right (SingleQuote tokens)
-- checkSingleQuoteApostrophe val _ = Right val

-- | Checks if variables are used in single quotes
checkVarInSingleQuotes :: ArgToken -> Either Message ArgToken
checkVarInSingleQuotes t@(ArgS ts) =
  case parse S.varP ts of
    Left error -> Right t
    Right _ -> Left (WarningMessage "Variables cannot be used inside single quotes")
checkVarInSingleQuotes t = Right t

checkArgSingleQuotes :: [ArgToken] -> Map Var BashCommand -> Either Message [ArgToken]
checkArgSingleQuotes (t : tokens) history =
  let res = checkVarInSingleQuotes t `eitherOp` checkQuotedTildeExpansionTokens t
   in case res of
        Left err -> Left err
        Right tt -> do
          tokenss <- checkArgSingleQuotes tokens history
          return (tt : tokenss)
checkArgSingleQuotes [] _ = Right []

checkVarInDoubleQuotes :: ArgToken -> Map Var BashCommand -> BashCommand -> Either Message ArgToken
checkVarInDoubleQuotes t@(ArgS ts) history cmd =
  case parse S.varP ts of
    Left error -> Right t
    Right var ->
      let V possVar = var
       in case Map.lookup var history of
            Nothing -> Left (WarningMessage ("Variable '" ++ possVar ++ "'" ++ " is not assigned"))
            Just (PossibleAssign pa) -> Left (WarningMessage ("Did you mean to assign variable " ++ possVar ++ " when you wrote: `" ++ pretty pa ++ "`? It was used later in: `" ++ pretty cmd ++ "`"))
            Just assign@(Assign _ (Arr _)) -> Left (WarningMessage $ "Referencing arrays as strings in `" ++ pretty assign ++ "`")
            Just _ -> Right t
checkVarInDoubleQuotes t _ _ = Right t

checkArgDoubleQuotes :: [ArgToken] -> Map Var BashCommand -> BashCommand -> Either Message [ArgToken]
checkArgDoubleQuotes tokens@(t : ts) history cmd =
  let res = checkVarInDoubleQuotes t history cmd `eitherOp` checkQuotedTildeExpansionTokens t
   in case res of
        Left err -> Left err
        Right tt -> do
          tokenss <- checkArgDoubleQuotes ts history cmd
          return (tt : tokenss)
checkArgDoubleQuotes [] _ _ = Right []

{- Conditionals [8] -}

-- Warnings

-- | Checks if constant values are being compared in the test expression
checkConstantTestExpressions :: IfExpression -> Either Message IfExpression
checkConstantTestExpressions exp =
  case exp of
    IfOp2 (IfVal _) op (IfVal _) -> if op /= Reg then Left (WarningMessage $ "The expression `" ++ pretty exp ++ "` is constant") else Right exp
    IfOp3 (IfVal _) op (IfVal _) -> if op /= Reg then Left (WarningMessage $ "The expression `" ++ pretty exp ++ "` is constant") else Right exp
    _ -> Right exp

verifyRegStrings :: String -> IfExpression -> Either Message IfExpression
verifyRegStrings s exp =
  case parse S.regex s of
    Left _ -> Right exp
    Right _ -> Left (WarningMessage $ "Remove quotes in `" ++ pretty exp ++ "` to match as a regex instead of literally")

-- | Checks if regex is quoted in expression with =~
checkQuotedRegex :: IfExpression -> Either Message IfExpression
checkQuotedRegex exp =
  case exp of
    IfOp2 expL op (IfVal (StringVal s)) -> if op == Reg then verifyRegStrings s exp else Right exp
    _ -> Right exp

-- | Checks if test operators are used in ((..))
checkTestOperators :: IfExpression -> Either Message IfExpression
checkTestOperators exp =
  case exp of
    IfOp3 _ op _ -> if op `notElem` arithmeticOps then Left (WarningMessage $ "Test operators like " ++ pretty op ++ " can't be used in arithmetic contexts") else Right exp
    _ -> Right exp

-- Errors

-- | Checks if numerical operators are used against strings
checkNumericalCompStrInExp :: IfExpression -> Either Message IfExpression
checkNumericalCompStrInExp exp =
  case exp of
    IfOp2 (IfVal (StringVal _)) op _ -> if op `elem` numOps then Left (ErrorMessage $ pretty op ++ pretty " is for numerical comparisons") else Right exp
    IfOp2 _ op (IfVal (StringVal _)) -> if op `elem` numOps then Left (ErrorMessage $ pretty op ++ pretty " is for numerical comparisons") else Right exp
    IfOp3 (IfVal (StringVal _)) op _ -> if op `elem` numOps then Left (ErrorMessage $ pretty op ++ pretty " is for numerical comparisons") else Right exp
    IfOp3 _ op (IfVal (StringVal _)) -> if op `elem` numOps then Left (ErrorMessage $ pretty op ++ pretty " is for numerical comparisons") else Right exp
    _ -> Right exp

-- | Checks if && is used in test expressions
checkAndInExp :: IfExpression -> Either Message IfExpression
checkAndInExp exp =
  case exp of
    IfOp2 _ AndIf _ -> Left (ErrorMessage $ pretty And ++ " cannot be used inside [...] or [[...]]")
    _ -> Right exp

-- | Checks if conditional expression is implicitly always true through mislabeled expression
checkLiteralVacuousTrue :: IfExpression -> Either Message IfExpression
checkLiteralVacuousTrue exp =
  case exp of
    IfOp1 op (IfVal (StringVal _)) -> if op `elem` [LengthZero, LengthNonZero] then Left (ErrorMessage $ "Argument to " ++ pretty op ++ " is always true") else Right exp
    _ -> Right exp

-- | Checks if unsupported operators are used
checkUnsupportedOperators :: IfExpression -> Either Message IfExpression
checkUnsupportedOperators exp =
  case exp of
    IfOp2 _ Err _ -> Left (ErrorMessage $ "Operator in `" ++ pretty exp ++ "` is not supported")
    IfOp3 _ Err _ -> Left (ErrorMessage $ "Operator in `" ++ pretty exp ++ "` is not supported")
    _ -> Right exp

-- | Checks if unassigned variables are used
checkVarInExp :: IfExpression -> Map Var BashCommand -> IfExpression -> Either Message IfExpression
checkVarInExp exp history fullExp =
  case exp of
    IfVar var ->
      let V possVar = var
       in case Map.lookup var history of
            Nothing -> Left (WarningMessage ("Variable '" ++ possVar ++ "'" ++ " is not assigned"))
            Just (PossibleAssign pa) -> Left (WarningMessage ("Did you mean to assign variable " ++ pretty var ++ " when you wrote:`" ++ pretty pa ++ "`? It was used later in: `" ++ pretty fullExp ++ "`"))
            Just _ -> Right fullExp
    IfOp1 _ exp -> checkVarInExp exp history fullExp
    IfOp2 exp1 _ exp2 -> do
      checkVarInExp exp1 history fullExp
      checkVarInExp exp2 history fullExp
    IfOp3 exp1 _ exp2 -> do
      checkVarInExp exp1 history fullExp
      checkVarInExp exp2 history fullExp
    _ -> Right fullExp

allWarningCondCheckers :: [IfExpression -> Either Message IfExpression]
allWarningCondCheckers = [checkConstantTestExpressions, checkQuotedRegex, checkTestOperators]

func :: IfExpression -> (IfExpression -> Either Message IfExpression) -> [Message] -> [Message]
func exp f msgs =
  case f exp of
    Right _ -> None : msgs
    Left m -> m : msgs

runWarningCondCheckers :: IfExpression -> [Message]
runWarningCondCheckers exp = Data.Foldable.foldr (func exp) [] allWarningCondCheckers

checkConditionalSt :: BashCommand -> Map Var BashCommand -> Either Message BashCommand
checkConditionalSt cmd@(Conditional exp (Block b1) (Block b2)) history =
  do
    checkTestOperators exp `eitherOp` checkVarInExp exp history exp `eitherOp` checkNumericalCompStrInExp exp `eitherOp` checkAndInExp exp `eitherOp` checkConstantTestExpressions exp `eitherOp` checkLiteralVacuousTrue exp `eitherOp` checkUnsupportedOperators exp `eitherOp` checkQuotedRegex exp
    return cmd
checkConditionalSt cmd _ = Right cmd

{- Freq Misused Commands [2] -}

-- Warnings

-- | Checks if sudo is being redirected
checkRedirectInSudo :: BashCommand -> Map Var BashCommand -> Either Message BashCommand
checkRedirectInSudo (ExecCommand cmd@(ExecName cmdName) args) _ =
  if cmdName == "sudo" && hasRedirect args
    then Left (WarningMessage "sudo is being redirected")
    else Right (ExecCommand cmd args)
checkRedirectInSudo cmd _ = Right cmd

redirectArg :: Arg -> Bool
redirectArg (Arg s) = s == "<" || s == ">" || s == ">>"
redirectArg _ = False

hasRedirect :: [Arg] -> Bool
hasRedirect = Prelude.foldr ((||) . redirectArg) False

-- -- | Checks if aliases are defined with arguments
-- checkArgumentsInAliases :: BashCommand -> Either String BashCommand
-- checkArgumentsInAliases = undefined

-- | Checks if redirections are in find
checkRedirectionInFind :: BashCommand -> Map Var BashCommand -> Either Message BashCommand
checkRedirectionInFind (ExecCommand cmd@(ExecName cmdName) args) _ =
  if cmdName == "find" && hasRedirect args
    then Left (WarningMessage "Redirections is being used on find command")
    else Right (ExecCommand cmd args)
checkRedirectionInFind cmd _ = Right cmd

{- Beginner Mistakes -}

-- Warnings

-- [NOTE] Extra spaces and $ in assignment checkers are included towards the bottom of this file

checkCommaSeparatedArrays :: Expression -> Either Message Expression
checkCommaSeparatedArrays exp@(Arr s) =
  case parse S.entireCommaInArr s of
    Left _ -> Right exp
    Right _ -> Left (WarningMessage "Use spaces to separate array elements")
checkCommaSeparatedArrays exp = Right exp

{- Style [4] -}

-- Warnings

-- | Checks if token uses backticks, should be $() instead
unstylisticInterpolation :: Arg -> Either Message Arg
unstylisticInterpolation (SingleQuote tokens) = if Prelude.foldr ((||) . backTickToken) False tokens then Left (WarningMessage "Backticks are being used, which has been deprecated - use $() instead") else Right (SingleQuote tokens)
unstylisticInterpolation arg = Right arg

backTickToken :: ArgToken -> Bool
backTickToken (ArgS t) =
  case parse S.backticksP t of
    Left _ -> False
    Right _ -> True
backTickToken _ = False

-- | Checks if outdated $[] is used instead of standard $((..)) in an Arg
oldArithExpansionArg :: Arg -> Either Message Arg
oldArithExpansionArg (Arg s) = case parse S.oldArithmeticExpansion s of
  Left _ -> Right (Arg s)
  Right _ -> Left (WarningMessage $ "Old arithmetic expansion is being used in " ++ s ++ " - use $((..)) instead")
oldArithExpansionArg arg = Right arg

checkArithmeticParentheses :: BashCommand -> Map Var BashCommand -> Either Message BashCommand
checkArithmeticParentheses (ExecCommand cmd@(ExecName cmdName) args) _ = case mapM argArithmeticExpansion args of
  Left err -> Left err
  Right args' -> Right (ExecCommand cmd args')
checkArithmeticParentheses cmd _ = Right cmd

argArithmeticExpansion :: Arg -> Either Message Arg
argArithmeticExpansion (Arg s) = case parse S.arithmeticExpansion s of
  Left _ -> Right (Arg s)
  Right inner -> case parse S.arithmeticInner inner of
    Left _ -> Left (WarningMessage $ "$ is being used in " ++ s ++ " - don't use $ on variables in $((..))")
    Right _ -> Right (Arg s)
argArithmeticExpansion arg = Right arg

-- | Checks if $ is used for variables in $((..))
checkNoVarInArithemetic :: BashCommand -> Map Var BashCommand -> Either Message BashCommand
checkNoVarInArithemetic (ExecCommand cmd@(ExecName cmdName) args) _ = case mapM argArithmeticExpansion args of
  Left err -> Left err
  Right args' -> Right (ExecCommand cmd args')
checkNoVarInArithemetic cmd _ = Right cmd

-- | Checks if echo is unnecessarily used
-- checkEchoUsage :: BashCommand -> Either String BashCommand
-- checkEchoUsage = undefined

-- | Checks if cat is unnecessarily used
-- checkCatUsage :: BashCommand -> Either String BashCommand
-- checkCatUsage = undefined

{- Data and typing errors [6] -}

-- Warnings

-- | Checks if arrays are assigned to strings
checkArrayAssignAsString :: Expression -> Either Message Expression
checkArrayAssignAsString exp@(Val (StringVal str)) =
  case parse (stringP "$@") str of
    Left err -> Right exp
    Right _ -> Left (WarningMessage "Assigning an array to a string using `$@`")
checkArrayAssignAsString exp = Right exp

-- | Checks if variables are defined but not used
argUnusedVar :: Map Var BashCommand -> Map Var Int -> Arg -> Either Message Arg
argUnusedVar history varFreq (Arg s) = case parse S.word s of
  Left _ -> Right (Arg s)
  Right potentialVar ->
    if Map.member (V potentialVar) history
      then case Map.lookup (V potentialVar) varFreq of
        Just freq ->
          if freq == 0
            then Left (WarningMessage $ "Potentially trying to use variable " ++ potentialVar ++ "- add $ to use it")
            else Left (WarningMessage "Previously used variable")
        Nothing -> Right (Arg s)
      else Right (Arg s)
argUnusedVar _ _ arg = Right arg

-- | Checks if variables are being attempted to be used incorrectly - user intends to use it but does not do so correctly using $
checkUnusedVar :: BashCommand -> Map Var BashCommand -> Map Var Int -> Either Message BashCommand
checkUnusedVar (ExecCommand cmd@(ExecName cmdName) args) history varFreq = case mapM (argUnusedVar history varFreq) args of
  Left err -> Left err
  Right args' -> Right (ExecCommand cmd args')
checkUnusedVar cmd _ _ = Right cmd

-- >>> checkUnassignedVar (ExecCommand (ExecName "echo") ["$x"]) Map.empty
-- Left "Error: x is not assigned"

-- -- # Assignments in subshells
-- checkAssignmentInSubshell :: BashCommand -> Either String Command
-- checkAssignmentInSubshell = undefined

-- | Checks if commands that don't read are being piped
checkPipingRead :: BashCommand -> Either String BashCommand
checkPipingRead = undefined

-- | Checks Double Quote Arguments to tell you if contain type conversion
-- | if you find the arg with double quotes, then look after for arguments
-- | for each token
hasCorrectNumberPrintfArgs :: [Arg] -> Bool
hasCorrectNumberPrintfArgs [] = True
hasCorrectNumberPrintfArgs (x : xs) = case x of
  DoubleQuote tokens -> length xs == S.numFormatSpecInTokens tokens
  _ -> hasCorrectNumberPrintfArgs xs

-- | Checks if argument count doesn't match in printf
checkPrintArgCount :: BashCommand -> Map Var BashCommand -> Either Message BashCommand
checkPrintArgCount fullCmd@(ExecCommand cmd@(ExecName cmdName) args) _ = if hasCorrectNumberPrintfArgs args then Right (ExecCommand cmd args) else Left (WarningMessage $ pretty cmdName ++ " in `" ++ pretty fullCmd ++ "` has incorrect number of arguments")
checkPrintArgCount cmd _ = Right cmd

--- >>> checkPrintArgCount (ExecCommand (ExecName "printf") [SingleQuote [ArgS "%s:",ArgS "%s\\n"]]) Map.empty
-- Right (ExecCommand (ExecName "printf") [SingleQuote [ArgS "%s:",ArgS "%s\\n"]])


-- | Checks if word boundaries are lost in array eval
checkArrayEval :: BashCommand -> Either String BashCommand
checkArrayEval = undefined -- [@] -> treats each element as a separate command by default


checkArrayValueUsedAsKey :: BashCommand -> Either String BashCommand
checkArrayValueUsedAsKey = undefined

{- Robustness -}

isTokenVar :: Map Var BashCommand -> ArgToken -> Bool
isTokenVar history t = 
  case t of
    ArgS ts ->
      case parse S.varP ts of
      Left _ -> False
      Right var -> Map.member var history
    _ -> False

checkVarInPrintfArgs :: [Arg] -> Map Var BashCommand -> Either Message [Arg]
checkVarInPrintfArgs args history =
  case args of
    [] -> Right []
    [arg] -> case arg of
        DoubleQuote tokens -> if any (isTokenVar history) tokens then Left (WarningMessage "Variables are not allowed in printf arguments") else Right [arg]
        _ -> Right [arg]
    (arg : as) ->
      case arg of
        DoubleQuote tokens -> if any (isTokenVar history) tokens then Left (WarningMessage "Variables are not allowed in printf arguments") else checkVarInPrintfArgs as history
        _ -> checkVarInPrintfArgs as history

-- | Checks if variables are used in printf argument
checkVarInPrintf :: BashCommand -> Map Var BashCommand -> Either Message BashCommand
checkVarInPrintf cmd@(ExecCommand (ExecName cmdName) args) history = 
  if cmdName == "printf" then 
    case checkVarInPrintfArgs args history of
      Left err -> Left err
      Right _ -> Right cmd
    else Right cmd
checkVarInPrintf cmd _ = Right cmd

-- | Checks if variables are used in printf
checkNoVariablesInPrintf :: BashCommand -> Map Var BashCommand -> Either Message BashCommand
checkNoVariablesInPrintf (ExecCommand cmd@(ExecName cmdName) args) history =
  if cmdName == "printf"
    then
      let res = checkArg args history (ExecCommand cmd args)
       in case res of
            Left err -> Left err
            Right args -> Right (ExecCommand cmd args)
    else Right (ExecCommand cmd args)
checkNoVariablesInPrintf cmd history = Right cmd

{- Miscellaneous -}

-- Warnings 

-- | Checks if a variable is assigned to itself
checkVariableAssignedToItself :: String -> Expression -> Either Message Expression
checkVariableAssignedToItself varName exp@(Var (V s)) =
    if s == varName then Left (WarningMessage $ "variable " ++ varName ++ " is assigned to itself - this does not do anything") else Right exp
checkVariableAssignedToItself _ exp = Right exp


mapRight :: Either Message ArgToken -> Either Message [ArgToken]
mapRight (Right x) = Right [x]
mapRight (Left x) = Left x

checkArg :: [Arg] -> Map Var BashCommand -> BashCommand -> Either Message [Arg]
checkArg args@(x : xs) history cmd =
  case x of
    Arg a ->
      case parse S.varP a of
        Left error -> Right args
        Right var ->
          let V possVar = var
           in case Map.lookup var history of
                Nothing -> Left (WarningMessage ("Variable '" ++ possVar ++ "'" ++ " is not assigned"))
                Just (PossibleAssign pa) -> Left (WarningMessage ("Did you mean to assign variable " ++ pretty var ++ " when you wrote: `" ++ pretty pa ++ "`? It was used later in: `" ++ pretty cmd ++ "`"))
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
checkArg [] _ _ = Right []

checkExecCommandArgs :: BashCommand -> Map Var BashCommand -> Either Message BashCommand
checkExecCommandArgs command@(ExecCommand cmd (x : xs)) history = do
  args <- checkArg (x : xs) history command
  return (ExecCommand cmd args)
checkExecCommandArgs cmd _ = Right cmd -- for other types like assignments, skip.

checkAssignmentExp :: BashCommand -> Map Var BashCommand -> Either Message BashCommand
checkAssignmentExp cmd@(Assign (V var) exp) history  = 
  let res = checkArrayAssignAsString exp `eitherOp` checkVariableAssignedToItself var exp  `eitherOp` checkCommaSeparatedArrays exp in
    case res of
      Left err -> Left err
      Right _ -> Right cmd
checkAssignmentExp cmd _ = Right cmd

{- 
  **
  Functions below run each checkers and stops immediately whenever an error encountered by a checker.
    Otherwise, rest of the checkers will continued to be run and accumulate warnings to show after 
    all checkers are run without any errors.
  ** 
-}

argCheckers :: [Arg -> Either Message Arg]
argCheckers = [unstylisticInterpolation, oldArithExpansionArg, argArithmeticExpansion]

argHistCheckers :: [Arg -> Map Var BashCommand -> Either Message Arg]
argHistCheckers = [checkUnquotedVar]

ifCheckers :: [IfExpression -> Either Message IfExpression]
ifCheckers = [checkConstantTestExpressions, checkLiteralVacuousTrue, checkQuotedRegex, checkUnsupportedOperators, checkTestOperators, checkNumericalCompStrInExp, checkAndInExp]

bashCheckers :: [BashCommand -> Map Var BashCommand -> Either Message BashCommand]
bashCheckers =
  [ 
    checkExecCommandArgs,
    checkAssignmentExp,
    checkRedirectInSudo,
    checkRedirectionInFind,
    checkArithmeticParentheses,
    checkNoVarInArithemetic,
    checkPrintArgCount,
    checkNoVariablesInPrintf,
    checkVarInPrintf
  ]

ifAllChecker :: IfExpression -> Map Var BashCommand -> [IfExpression -> Either Message IfExpression] -> [Message]
ifAllChecker ifexpr history = Prelude.foldr (\x acc -> acc ++ ifChecker ifexpr x) []
  where
    ifChecker ifexpr checker =
      case checker ifexpr of
        Left z -> [z]
        Right _ -> []

getMessages :: Either Message a -> [Message] -> [Message]
getMessages x acc = 
  case x of
    Left z -> z : acc
    Right _ -> acc

argHistAllCheckers :: [Arg] -> Map Var BashCommand -> [Arg -> Map Var BashCommand -> Either Message Arg] -> [Message]
argHistAllCheckers args history = Prelude.foldr (\x acc -> acc ++ argsChecker args x history) []
  where
    argsChecker args checker history =
      Prelude.foldr
        getMessages
        []
        (fmap checkresult args)
      where
        checkresult = flip checker history

argAllCheckers :: [Arg] -> [Arg -> Either Message Arg] -> [Message]
argAllCheckers args = Prelude.foldr (\x acc -> acc ++ argsChecker args x) []
  where
    argsChecker args checker =
      Prelude.foldr
        getMessages
        []
        (fmap checker args)


bashAllChecker :: BashCommand -> Map Var BashCommand -> [BashCommand -> Map Var BashCommand -> Either Message BashCommand] -> [Message]
bashAllChecker cmd history = Prelude.foldr (\x acc -> acc ++ bashChecker cmd x) []
  where
    bashChecker cmd checker =
      case checker cmd history of
        Left z -> [z]
        Right _ -> []

blockChecker :: Block -> Map Var BashCommand -> [BashCommand -> Map Var BashCommand -> Either Message BashCommand] -> [Message]
blockChecker (Block cmds) history checkers = Prelude.foldr (\x acc -> acc ++ bashChecker x) [] cmds
  where
    bashChecker cmd = bashAllChecker cmd history checkers

-- | Looks for the first error message
findErrorMessage :: [Message] -> Message
findErrorMessage [] = None
findErrorMessage (x : xs) = case x of
  ErrorMessage _ -> x
  _ -> findErrorMessage xs

-- | allWarning Messages
allWarningMessages :: [Message] -> [Message]
allWarningMessages [] = []
allWarningMessages (x : xs) = case x of
  WarningMessage _ -> x : allWarningMessages xs
  _ -> allWarningMessages xs

mainFold f cmd acc = 
  case f cmd of
    Left m -> m : acc
    Right (_, m) -> m ++ acc

-- Looks at different structures of bash command -> attempts to run checkers on it
mainChecker :: Map Var BashCommand -> Map Var Int -> BashCommand -> Either Message (BashCommand, [Message])
mainChecker history varFreq (Assign var expr) =
  let bashMessages = bashAllChecker (Assign var expr) history bashCheckers
      allMessages = bashMessages
      errorMessage = findErrorMessage allMessages
   in case errorMessage of
        None -> Right (Assign var expr, allWarningMessages allMessages)
        _ -> Left errorMessage
mainChecker history varFreq (Conditional ifExpr block1@(Block cmds1) block2@(Block cmds2)) =
  let ifMessages = ifAllChecker ifExpr history ifCheckers ++ getMessages (checkVarInExp ifExpr history ifExpr) []
      block1Messages = foldr (mainFold (mainChecker history varFreq)) [] cmds1
      block2Messages = foldr (mainFold (mainChecker history varFreq)) [] cmds2
      allMessages =
        ifMessages
          ++ block1Messages
          ++ block2Messages
      errorMessage = findErrorMessage allMessages
   in case errorMessage of
        None -> Right (Conditional ifExpr block1 block2, allWarningMessages allMessages)
        _ -> Left errorMessage
mainChecker history varFreq pa@(PossibleAssign (PossibleAssignWS var _ _ _ expr))=
  let bashMessages = bashAllChecker pa history bashCheckers
      allMessages = bashMessages
      errorMessage = findErrorMessage allMessages
   in case errorMessage of
        None -> Right (pa, allWarningMessages allMessages)
        _ -> Left errorMessage
mainChecker history varFreq command@(ExecCommand cmd args)  = do
  let argMessages = argAllCheckers args argCheckers
  let argHistMessages = argHistAllCheckers args history argHistCheckers
  let bashMessages = bashAllChecker command history bashCheckers ++ getMessages (checkUnusedVar command history varFreq) []
  let allMessages = argMessages ++ bashMessages ++ argHistMessages
  let errorMessage = findErrorMessage allMessages
  case errorMessage of
    None -> Right (ExecCommand cmd args, allWarningMessages allMessages)
    _ -> Left errorMessage
mainChecker _ _ _ = Left (ErrorMessage "Error: This should not happen.")