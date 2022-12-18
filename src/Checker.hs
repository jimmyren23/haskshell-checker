module Checker where

import Control.Applicative (Alternative (..))
import Data.Foldable
import Data.Map
import Data.Map qualified as Map
import Parsing
import PrettyPrint (pretty)
import ShellParsing qualified as S
import ShellSyntax
import Test.HUnit.Lang (Result (Error))

-- Datatype to differentiate between messages
data Message = WarningMessage String | ErrorMessage String | None
  deriving (Show, Eq)

-- History is a map of variables to the command assigned to it
type History = Map Var BashCommand

-- VarFrequency is a map of variables to the number of times they have been used
type VarFrequency = Map Var Int

-- newtype CommandCheck a = CommandCheck {checkCommand :: a -> Map Var BashCommand -> Map Var Int -> Either Message a}

-- instance Applicative CommandCheck where
--   pure :: a -> CommandCheck a
--   pure a = CommandCheck (\_ _ _ -> Right a)

--   (<*>) :: CommandCheck (a -> b) -> CommandCheck a -> CommandCheck b
--   c1@(CommandCheck f) <*> c2@(CommandCheck g) = CommandCheck $ \m env vars -> do
--     a <- g m env vars
--     aa <- f a env vars
--     return (aa a)

-- instance Alternative CommandCheck where
--   empty :: CommandCheck a
--   empty = CommandCheck (\_ _ _ -> Left (WarningMessage "error"))

--   (<|>) :: CommandCheck a -> CommandCheck a -> CommandCheck a
--   p1 <|> p2 = CommandCheck (\a history freq -> checkCommand p1 a history freq `eitherOp` checkCommand p2 a history freq)

-- instance Alternative Parser where
--   empty :: Parser a
--   empty = P $ const $ Left "error"

--   (<|>) :: Parser a -> Parser a -> Parser a
--   p1 <|> p2 = P $ \s -> doParse p1 s `firstRight` doParse p2 s

-- | Finds the first result with error
eitherOp :: Either Message a -> Either Message a -> Either Message a
eitherOp (Left err1) _ = Left err1
eitherOp _ (Left err2) = Left err2
eitherOp (Right cmd) _ = Right cmd

{- Quoting [5] -}

-- | Checks if a variable is quoted
checkUnquotedVar :: Arg -> Map Var BashCommand -> Either Message Arg
checkUnquotedVar (Arg s) history = case parse S.argUnquotedVar s of
  Left str -> Right (Arg s)
  Right var ->
    if Map.member (V s) history
      then Left (WarningMessage ("Warning: Variable " ++ s ++ " that was previously used is not quoted."))
      else Right (Arg s)
checkUnquotedVar arg _ = Right arg

-- | Checks if tilde is used in quotes
checkQuotedTildeExpansionTokens :: Token -> Either Message Token
checkQuotedTildeExpansionTokens token =
  if token == "<tilde>" then Left (WarningMessage "Tilde expansion can't be used in strings") else Right token

-- | Checks if token could be a variable, considering if its in its history
possVariableRefToken :: Token -> Map Var BashCommand -> Bool
possVariableRefToken t history =
  let res = parse S.variableRef t
   in case res of
        Left _ -> False
        Right var -> Map.member var history

-- | Checks if variable was quoted or not when used
tokenListSearch :: [Token] -> Map Var BashCommand -> Either Message [Token]
tokenListSearch [] _ = Right []
tokenListSearch (t : ts) history =
  if possVariableRefToken t history
    then Left (WarningMessage $ "Warning: Variable " ++ t ++ " that was previously used is not quoted.")
    else tokenListSearch ts history

-- | Checks if single quotes are closed by apostrophe
checkSingleQuoteApostrophe :: Arg -> Map Var BashCommand -> Either Message Arg
checkSingleQuoteApostrophe (SingleQuote tokens) history = case tokenListSearch tokens history of
  Left err -> Left err
  Right _ -> Right (SingleQuote tokens)
checkSingleQuoteApostrophe val _ = Right val

{- Conditionals [8] -}

-- | Checks if constant values are being compared in the test expression
checkConstantTestExpressions :: IfExpression -> Either Message IfExpression
checkConstantTestExpressions exp =
  case exp of
    IfOp2 (IfVal _) op (IfVal _) -> Left (WarningMessage $ "The expression `" ++ pretty exp ++ "` is constant")
    IfOp3 (IfVal _) op (IfVal _) -> Left (WarningMessage $ "The expression `" ++ pretty exp ++ "` is constant")
    _ -> Right exp

-- | Checks if Expression to see if conditional is implicitly always true through mislabeled expression
checkLiteralVacuousTrue :: IfExpression -> Either Message IfExpression
checkLiteralVacuousTrue exp =
  case exp of
    IfOp1 op (IfVal (StringVal _)) -> if op `elem` [LengthZero, LengthNonZero] then Left (ErrorMessage $ "Argument to " ++ pretty op ++ " is always true.") else Right exp
    _ -> Right exp

-- | Checks if regex is quoted in expression with =~
checkQuotedRegex :: IfExpression -> Either Message IfExpression
checkQuotedRegex exp =
  case exp of
    IfOp2 expL op (IfVal (StringVal s)) -> if op == Reg then verifyRegStrings s exp else Right exp
    _ -> Right exp

verifyRegStrings :: String -> IfExpression -> Either Message IfExpression
verifyRegStrings s exp =
  case parse S.regex s of
    Left _ -> Right exp
    Right _ -> Left (WarningMessage $ "Remove quotes in `" ++ pretty exp ++ "` to match as a regex instead of literally.")

-- | Checks if unsupported operators are used
checkUnsupportedOperators :: IfExpression -> Either Message IfExpression
checkUnsupportedOperators exp =
  case exp of
    IfOp2 _ Err _ -> Left (ErrorMessage $ "Operator in `" ++ pretty exp ++ "` is not supported.")
    IfOp3 _ Err _ -> Left (ErrorMessage $ "Operator in `" ++ pretty exp ++ "` is not supported.")
    _ -> Right exp

-- | Checks if test operators are used in ((..))
checkTestOperators :: IfExpression -> Either Message IfExpression
checkTestOperators exp =
  case exp of
    IfOp3 _ op _ -> if op `notElem` arithmeticOps then Left (WarningMessage $ "Test operators like " ++ pretty op ++ " can't be used in arithmetic contexts.") else Right exp
    _ -> Right exp

-- | Checks if unassigned variables are used
checkVarInExp :: IfExpression -> Map Var BashCommand -> IfExpression -> Either Message IfExpression
checkVarInExp exp history fullExp =
  case exp of
    IfVar var ->
      let V possVar = var
       in case Map.lookup var history of
            Nothing -> Left (WarningMessage ("Variable '" ++ possVar ++ "'" ++ " is not assigned"))
            Just (PossibleAssign pa) -> Left (WarningMessage ("Did you mean to assign variable " ++ pretty var ++ " when you wrote: " ++ pretty pa ++ "? It was used later in: " ++ pretty fullExp))
            Just _ -> Right fullExp
    IfOp1 _ exp -> checkVarInExp exp history fullExp
    IfOp2 exp1 _ exp2 -> do
      checkVarInExp exp1 history fullExp
      checkVarInExp exp2 history fullExp
    IfOp3 exp1 _ exp2 -> do
      checkVarInExp exp1 history fullExp
      checkVarInExp exp2 history fullExp
    _ -> Right fullExp

-- | Checks if numerical operators are used against strings
checkNumericalCompStrInExp :: IfExpression -> Either Message IfExpression
checkNumericalCompStrInExp exp =
  case exp of
    IfOp2 (IfVal (StringVal _)) op _ -> if op `elem` numOps then Left (ErrorMessage $ pretty op ++ pretty " is for numerical comparisons.") else Right exp
    IfOp2 _ op (IfVal (StringVal _)) -> if op `elem` numOps then Left (ErrorMessage $ pretty op ++ pretty " is for numerical comparisons.") else Right exp
    IfOp3 (IfVal (StringVal _)) op _ -> if op `elem` numOps then Left (ErrorMessage $ pretty op ++ pretty " is for numerical comparisons.") else Right exp
    IfOp3 _ op (IfVal (StringVal _)) -> if op `elem` numOps then Left (ErrorMessage $ pretty op ++ pretty " is for numerical comparisons.") else Right exp
    _ -> Right exp

-- | Checks if && is used in test expressions
checkAndInExp :: IfExpression -> Either Message IfExpression
checkAndInExp exp =
  case exp of
    IfOp2 _ AndIf _ -> Left (ErrorMessage $ pretty And ++ " cannot be used inside [...] or [[...]].")
    _ -> Right exp

allWarningCondCheckers :: [IfExpression -> Either Message IfExpression]
allWarningCondCheckers = [checkConstantTestExpressions, checkQuotedRegex, checkTestOperators]

func :: IfExpression -> (IfExpression -> Either Message IfExpression) -> [Message] -> [Message]
func exp f msgs =
  case f exp of
    Right _ -> None : msgs
    Left m -> m : msgs

runWarningCondCheckers :: IfExpression -> [Message]
runWarningCondCheckers exp = Data.Foldable.foldr (func exp) [] allWarningCondCheckers

-- >>> runWarningCondCheckers (IfOp2 (IfVar (V "y")) Reg (IfVal (StringVal "x+")))
-- [None,WarningMessage "Remove quotes in `y =~ 'x+'` to match as a regex instead of literally.",None]

checkConditionalSt :: BashCommand -> Map Var BashCommand -> Either Message BashCommand
checkConditionalSt cmd@(Conditional exp (Block b1) (Block b2)) history =
  do
    checkTestOperators exp `eitherOp` checkVarInExp exp history exp `eitherOp` checkNumericalCompStrInExp exp `eitherOp` checkAndInExp exp `eitherOp` checkConstantTestExpressions exp `eitherOp` checkLiteralVacuousTrue exp `eitherOp` checkUnsupportedOperators exp `eitherOp` checkQuotedRegex exp
    return cmd
checkConditionalSt cmd _ = Right cmd

{- Freq Misused Commands [2] -}

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
    then Left (WarningMessage "Redirections is being used on find command. Rewrite it.")
    else Right (ExecCommand cmd args)
checkRedirectionInFind cmd _ = Right cmd

{- Beginner Mistakes -}

{- Style [4] -}

-- | Checks if token uses backticks, should be $() instead
unstylisticInterpolation :: Arg -> Either Message Arg
unstylisticInterpolation (SingleQuote tokens) = if Prelude.foldr ((||) . backTickToken) False tokens then Left (WarningMessage "Backticks are being used, which has been deprecated. Use $() instead.") else Right (SingleQuote tokens)
unstylisticInterpolation arg = Right arg

backTickToken :: Token -> Bool
backTickToken t =
  case parse S.backticksP t of
    Left _ -> False
    Right _ -> True

-- | Checks if redirections are used in find command
checkCommandSubstitution :: BashCommand -> Map Var BashCommand -> Either Message BashCommand
checkCommandSubstitution (ExecCommand cmd@(ExecName cmdName) args) _ =
  if cmdName == "find" && hasRedirect args
    then Left (WarningMessage "Style Warning: Redirections is being used on find command. Rewrite it.")
    else Right (ExecCommand cmd args)
checkCommandSubstitution cmd _ = Right cmd

-- | Checks if outdated $[] is used instead of standard $((..)) in an Arg
oldArithExpansionArg :: Arg -> Either Message Arg
oldArithExpansionArg (Arg s) = case parse S.oldArithmeticExpansion s of
  Left _ -> Right (Arg s)
  Right _ -> Left (WarningMessage $ "Old arithmetic expansion is being used in" ++ s ++ ". Use $((..)) instead.")
oldArithExpansionArg arg = Right arg

-- | Checks if outdated $[] is used instead of standard $((..))
checkArithmeticParentheses :: BashCommand -> Map Var BashCommand -> Either Message BashCommand
checkArithmeticParentheses (ExecCommand cmd@(ExecName cmdName) args) _ = case mapM argArithmeticExpansion args of
  Left err -> Left err
  Right args' -> Right (ExecCommand cmd args')
checkArithmeticParentheses cmd _ = Right cmd

argArithmeticExpansion :: Arg -> Either Message Arg
argArithmeticExpansion (Arg s) = case parse S.arithmeticExpansion s of
  Left _ -> Right (Arg s)
  Right inner -> case parse S.arithmeticInner inner of
    Left _ -> Left (WarningMessage $ "Style Warning: $ is being used in $()). " ++ s ++ ". Don't use $ on variables in $((..))")
    Right _ -> Right (Arg s)
argArithmeticExpansion arg = Right arg

-- >>> argArithmeticExpansion (Arg "$(($Random % 6))")
-- Left "Style Warning: $ is being used in $()). $(($Random % 6)). Don't use $ on variables in $((..))"

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

-- | Checks if arrays are assigned to strings
checkArrayAssignAsString :: Expression -> Either Message Expression
checkArrayAssignAsString exp@(Val (StringVal str)) =
  case parse (stringP "$@") str of
    Left err -> Right exp
    Right _ -> Left (WarningMessage "Assigning an array to a string using `$@`.")
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
            then Left (WarningMessage $ "Style Warning: Potentialy trying to use variable " ++ potentialVar ++ ". It is unused. Use $ to use it.")
            else Left (WarningMessage "Warning: Previously used variable")
        Nothing -> Right (Arg s)
      else Right (Arg s)
argUnusedVar _ _ arg = Right arg

-- | Checks if variables are being attempted to be used incorrectly - user intends to use it but does not do so correctly using $
checkUnusedVar :: BashCommand -> Map Var BashCommand -> Map Var Int -> Either Message BashCommand
checkUnusedVar (ExecCommand cmd@(ExecName cmdName) args) history varFreq = case mapM (argUnusedVar history varFreq) args of
  Left err -> Left err
  Right args' -> Right (ExecCommand cmd args')
checkUnusedVar cmd _ _ = Right cmd

-- | Checks if variables are used in single quotes
checkVarInSingleQuotes :: Token -> Either Message [Token]
checkVarInSingleQuotes t =
  case parse S.variableRef t of
    Left error -> Right [t]
    Right _ -> Left (WarningMessage "Variables cannot be used inside single quotes.")

-- -- | Checks if
-- checkEscapeInSingleQuotes :: Token -> Either Message [Token]
-- checkEscapeInSingleQuotes t =
--   if t == "<escape>" then Left (ErrorMessage "Escape cannot be used in single quotes") else Right [t]

mapRight :: Either Message Token -> Either Message [Token]
mapRight (Right x) = Right [x]
mapRight (Left x) = Left x

checkArgSingleQuotes :: [Token] -> Map Var BashCommand -> Either Message [Token]
checkArgSingleQuotes (t : tokens) history =
  let res = checkVarInSingleQuotes t `eitherOp` mapRight (checkQuotedTildeExpansionTokens t)
   in case res of
        Left err -> Left err
        Right tt -> do
          tokenss <- checkArgSingleQuotes tokens history
          return (tt ++ tokenss)
checkArgSingleQuotes [] _ = Right []

checkVarInDoubleQuotes :: Token -> Map Var BashCommand -> BashCommand -> Either Message Token
checkVarInDoubleQuotes t history cmd =
  case parse S.variableRef t of
    Left error -> Right t
    Right var ->
      let V possVar = var
       in case Map.lookup var history of
            Nothing -> Left (WarningMessage ("Variable '" ++ possVar ++ "'" ++ " is not assigned"))
            Just (PossibleAssign pa) -> Left (WarningMessage ("Did you mean to assign variable " ++ possVar ++ " when you wrote: " ++ pretty pa ++ "? It was used later in: " ++ pretty cmd))
            Just (Assign _ (Arr _)) -> Left (WarningMessage "Referencing arrays as strings.")
            Just _ -> Right t

checkArgDoubleQuotes :: [Token] -> Map Var BashCommand -> BashCommand -> Either Message [Token]
checkArgDoubleQuotes tokens@(t : ts) history cmd =
  let res = checkVarInDoubleQuotes t history cmd `eitherOp` checkQuotedTildeExpansionTokens t
   in case res of
        Left err -> Left err
        Right tt -> do
          tokenss <- checkArgDoubleQuotes ts history cmd
          return (tt : tokenss)
checkArgDoubleQuotes [] _ _ = Right []

checkArg :: [Arg] -> Map Var BashCommand -> BashCommand -> Either Message [Arg]
checkArg args@(x : xs) history cmd =
  case x of
    Arg a ->
      case parse S.variableRef a of
        Left error -> Right args
        Right var ->
          let V possVar = var
           in case Map.lookup var history of
                Nothing -> Left (WarningMessage ("Variable '" ++ possVar ++ "'" ++ " is not assigned"))
                Just (PossibleAssign pa) -> Left (WarningMessage ("Did you mean to assign variable " ++ pretty var ++ " when you wrote: " ++ pretty pa ++ "? It was used later in: " ++ pretty cmd))
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
checkAssignmentExp cmd@(Assign var exp) history =
  let res = checkArrayAssignAsString exp
   in case res of
        Left err -> Left err
        Right _ -> Right cmd
checkAssignmentExp cmd _ = Right cmd

-- >>> checkUnassignedVar (ExecCommand (ExecName "echo") ["$x"]) Map.empty
-- Left "Error: x is not assigned"

-- -- # Assignments in subshells
-- checkAssignmentInSubshell :: BashCommand -> Either String Command
-- checkAssignmentInSubshell = undefined

-- | Checks if commands that don't read are being piped
checkPipingRead :: BashCommand -> Either String BashCommand
checkPipingRead = undefined

-- | checks Double Quote Arguments to tell you if contain type conversion
-- | if you find the arg with double quotes, then look after for arguments
-- | for each token
hasCorrectNumberPrintfArgs :: [Arg] -> Bool
hasCorrectNumberPrintfArgs [] = True
hasCorrectNumberPrintfArgs (x : xs) = case x of
  DoubleQuote tokens -> length xs == S.tokenPars tokens
  _ -> hasCorrectNumberPrintfArgs xs

-- | Checks if argument count doesn't match in printf
checkPrintArgCount :: BashCommand -> Map Var BashCommand -> Either Message BashCommand
checkPrintArgCount (ExecCommand cmd@(ExecName cmdName) args) _ = if hasCorrectNumberPrintfArgs args then Right (ExecCommand cmd args) else Left (WarningMessage $ "Printf" ++ pretty cmdName ++ " has incorrect number of arguments.")
checkPrintArgCount cmd _ = Right cmd

-- | Checks if word boundaries are lost in array eval
checkArrayEval :: BashCommand -> Either String BashCommand
checkArrayEval = undefined -- [@] -> treats each element as a separate command by default

-- TODO: Need to define for-loop type first

-- | Checks if array value is being used as a key
-- for i in "${x[@]}";
--    do ${x[$i]}
-- done
checkArrayValueUsedAsKey :: BashCommand -> Either String BashCommand
checkArrayValueUsedAsKey = undefined

{- Robustness -}

isTokenVar :: Map Var BashCommand -> Token -> Bool
isTokenVar history t = case parse S.variableRef t of
  Left _ -> False
  Right var -> Map.member var history

-- | Checks if variables are used in printf argument
checkVarInPrintfArgs :: Arg -> Map Var BashCommand -> Either Message Arg
checkVarInPrintfArgs arg history = case arg of
  DoubleQuote tokens -> if any (isTokenVar history) tokens then Left (WarningMessage "Variables are not allowed in printf arguments.") else Right arg
  _ -> Right arg

-- >>> checkVarInPrintfArgs (DoubleQuote ["$x"]) (Map.fromList [(V "x", Assign (V "x") (Val (StringVal "hello")))])
-- Left "Error: Variables are not allowed in printf arguments."

-- | Checks if vaiables are used in printf
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

argCheckers :: [Arg -> Map Var BashCommand -> Either Message Arg]
argCheckers = [checkUnquotedVar, checkSingleQuoteApostrophe]

tokenCheckers :: [Token -> Either Message Token]
tokenCheckers = [checkQuotedTildeExpansionTokens]

ifCheckers :: [IfExpression -> Either Message IfExpression]
ifCheckers = [checkConstantTestExpressions, checkLiteralVacuousTrue, checkQuotedRegex, checkUnsupportedOperators, checkTestOperators, checkNumericalCompStrInExp, checkAndInExp]

bashCheckers :: [BashCommand -> Map Var BashCommand -> Either Message BashCommand]
bashCheckers =
  [ checkConditionalSt,
    checkRedirectInSudo,
    checkRedirectionInFind,
    checkCommandSubstitution,
    checkArithmeticParentheses,
    checkNoVarInArithemetic,
    checkExecCommandArgs,
    checkPrintArgCount,
    checkNoVariablesInPrintf
  ]

ifAllChecker :: IfExpression -> Map Var BashCommand -> [IfExpression -> Either Message IfExpression] -> [Message]
ifAllChecker ifexpr history = Prelude.foldr (\x acc -> acc ++ ifChecker ifexpr x) []
  where
    ifChecker ifexpr checker =
      case checker ifexpr of
        Left z -> [z]
        Right _ -> []

tokenAllCheckers :: [Token] -> Map Var BashCommand -> [Token -> Either Message Token] -> [Message]
tokenAllCheckers tokens history = Prelude.foldr (\x acc -> acc ++ tokenChecker tokens x) []
  where
    tokenChecker tokens checker =
      Prelude.foldr
        ( \x acc ->
            case x of
              Left z -> z : acc
              Right _ -> acc
        )
        []
        (fmap checker tokens)

argAllCheckers :: [Arg] -> Map Var BashCommand -> [Arg -> Map Var BashCommand -> Either Message Arg] -> [Message]
argAllCheckers args history = Prelude.foldr (\x acc -> acc ++ argsChecker args x history) []
  where
    argsChecker args checker history =
      Prelude.foldr
        ( \x acc ->
            case x of
              Left z -> z : acc
              Right _ -> acc
        )
        []
        (fmap checkresult args)
      where
        checkresult = flip checker history

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

-- Looks at different structures of bash command -> attempts to run checkers on it
mainChecker :: BashCommand -> Map Var BashCommand -> Map Var Int -> Either Message (BashCommand, [Message])
mainChecker (ExecCommand cmd args) history varFreq = do
  let argMessages = argAllCheckers args history argCheckers
  let bashMessages = bashAllChecker (ExecCommand cmd args) history bashCheckers
  let allMessages = argMessages ++ bashMessages
  let errorMessage = findErrorMessage allMessages
  case errorMessage of
    None -> Right (ExecCommand cmd args, allWarningMessages allMessages)
    _ -> Left errorMessage
mainChecker (Conditional ifExpr block1 block2) history varFreq =
  let ifMessages = ifAllChecker ifExpr history ifCheckers
      block1Messages = blockChecker block1 history bashCheckers
      block2Messages = blockChecker block2 history bashCheckers
      allMessages =
        ifMessages
          ++ block1Messages
          ++ block2Messages
      errorMessage = findErrorMessage allMessages
   in case errorMessage of
        None -> Right (Conditional ifExpr block1 block2, allWarningMessages allMessages)
        _ -> Left errorMessage
mainChecker (Assign var expr) history varFreq =
  let bashMessages = bashAllChecker (Assign var expr) history bashCheckers
      allMessages = bashMessages
      errorMessage = findErrorMessage allMessages
   in case errorMessage of
        None -> Right (Assign var expr, allWarningMessages allMessages)
        _ -> Left errorMessage
mainChecker _ _ _ = Left (ErrorMessage "Error: This should not happen.")
