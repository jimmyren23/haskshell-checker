module CheckerHUnitTests where

import Checker
import Test.HUnit (Assertion, Counts, Test (..), assert, runTestTT, (~:), (~?=))
import Parsing ( parse )
import ShellParsing ( bashCommandP, execCommandP )
import ShellSyntax
import qualified Data.Map as Map

test_quoting =
  "quoting_checkers"
  ~: TestList
    [
      checkQuotedTildeExpansionTokens (ArgM Tilde)
        ~?= Left (WarningMessage "Tilde expansion can't be used in strings"),
      checkQuotedTildeExpansionTokens (ArgS "no")
        ~?= Right (ArgS "no"),
      checkUnquotedVar (Arg "$var") (Map.fromList [(V "var", Assign (V "var") (Val (IntVal 1)))])
        ~?= Left (WarningMessage "Variable $var that was previously used is not quoted"),
      checkUnquotedVar (Arg "var") (Map.fromList [(V "var", Assign (V "var") (Val (IntVal 1)))])
        ~?= Right (Arg "var"),
      checkVarInSingleQuotes (ArgS "$var")
        ~?= Left (WarningMessage "Variables cannot be used inside single quotes"),
      checkVarInSingleQuotes (ArgS "var")
        ~?= Right (ArgS "var"),
      checkVarInDoubleQuotes (ArgS "$var") (Map.fromList [(V "var", PossibleAssign (PossibleAssignWS (V "var") " " "=" " " (Val (IntVal 1))))]) (Assign (V "var") (Val (IntVal 1)))
        ~?= Left (WarningMessage "Did you mean to assign variable var when you wrote: `var = 1`? It was used later in: `var=1`"),
      checkVarInDoubleQuotes (ArgS "$var") Map.empty (Assign (V "var") (Val (IntVal 1)))
        ~?= Left (WarningMessage "Variable 'var' is not assigned"),
      checkVarInDoubleQuotes (ArgS "$var")  (Map.fromList [(V "var", Assign (V "var") (Arr "hi"))]) (ExecCommand (ExecName "echo") [Arg "$var"])
        ~?= Left (WarningMessage "Referencing arrays as strings in `echo $var`")
    ]

test_conditionals =
  "conditional_checkers"
  ~: TestList
  [
    checkConstantTestExpressions (IfOp2 (IfVal (StringVal "hi")) GtIf (IfVal (IntVal 1)))
      ~?= Left (WarningMessage "The expression ('hi' > 1) is constant"),
    checkConstantTestExpressions (IfOp2 (IfVar (V "var")) GtIf (IfVal (IntVal 1)))
      ~?= Right (IfOp2 (IfVar (V "var")) GtIf (IfVal (IntVal 1))),
    checkQuotedRegex (IfOp2 (IfVal (StringVal "hi")) Reg (IfVal (StringVal "a+")))
      ~?= Left (WarningMessage "Remove quotes in ('hi' =~ 'a+') to match as a regex instead of literally"),
    checkQuotedRegex (IfOp2 (IfVal (StringVal "hi")) Reg (IfVal (StringVal "a*c")))
      ~?= Left (WarningMessage "Remove quotes in ('hi' =~ 'a*c') to match as a regex instead of literally"),
    checkQuotedRegex (IfOp2 (IfVal (StringVal "hi")) Reg (IfVal (StringVal "aa")))
      ~?= Right (IfOp2 (IfVal (StringVal "hi")) Reg (IfVal (StringVal "aa"))),
    checkTestOperators (IfOp3 (IfVal (StringVal "hi")) GtIf (IfVal (StringVal "aa")))
      ~?= Left (WarningMessage "Test operators like > can't be used in arithmetic contexts"),
    checkTestOperators (IfOp3 (IfVal (StringVal "hi")) Reg (IfVal (StringVal "aa")))
      ~?= Left (WarningMessage "Test operators like =~ can't be used in arithmetic contexts"),
    checkTestOperators (IfOp3 (IfVal (IntVal 7)) PlusIf (IfVal (IntVal 7)))
      ~?= Right (IfOp3 (IfVal (IntVal 7)) PlusIf (IfVal (IntVal 7))),
    checkTestOperators (IfOp3 (IfVal (IntVal 7)) ModuloIf (IfVal (IntVal 7)))
      ~?= Right (IfOp3 (IfVal (IntVal 7)) ModuloIf (IfVal (IntVal 7))),
    checkNumericalCompStrInExp (IfOp2 (IfVal (StringVal "hi")) GtNIf (IfVal (IntVal 7)))
      ~?= Left (ErrorMessage "-gt is for numerical comparisons"),
    checkTestOperators (IfOp2 (IfVal (IntVal 7)) GtNIf (IfVal (IntVal 7)))
      ~?= Right (IfOp2 (IfVal (IntVal 7)) GtNIf (IfVal (IntVal 7))),
    checkAndInExp (IfOp2 (IfVal (IntVal 7)) AndIf (IfVal (IntVal 7)))
      ~?= Left (ErrorMessage "&& cannot be used inside [...] or [[...]]"),
    checkAndInExp (IfOp2 (IfVal (IntVal 7)) PlusIf (IfVal (IntVal 7)))
      ~?= Right (IfOp2 (IfVal (IntVal 7)) PlusIf (IfVal (IntVal 7))),
    checkLiteralVacuousTrue (IfOp1 LengthNonZero (IfVal (StringVal "")))
      ~?= Left (ErrorMessage "Argument to -n is always true"),
    checkLiteralVacuousTrue (IfOp1 LengthNonZero (IfVar (V "var")))
      ~?= Right (IfOp1 LengthNonZero (IfVar (V "var"))),
    checkUnsupportedOperators (IfOp2 (IfVal (IntVal 7)) Err (IfVal (IntVal 7)))
      ~?= Left (ErrorMessage "Operator in (7 <op> 7) is not supported"),
    checkVarInExp (IfOp1 LengthNonZero (IfVar (V "var"))) (Map.fromList [(V "var", PossibleAssign (PossibleAssignWS (V "var") " " "=" " " (Val (IntVal 1))))]) (IfOp1 LengthNonZero (IfVar (V "var")))
      ~?= Left (WarningMessage "Did you mean to assign variable var when you wrote:`var = 1`? It was used later in: `-n $var`"),
    checkVarInExp (IfOp1 LengthNonZero (IfVar (V "var"))) (Map.fromList [(V "var", Assign (V "var") (Val (IntVal 1)))]) (IfOp1 LengthNonZero (IfVar (V "var")))
    ~?= Right (IfOp1 LengthNonZero (IfVar (V "var"))),
    checkVarInExp (IfOp1 LengthNonZero (IfVar (V "var"))) Map.empty (IfOp1 LengthNonZero (IfVar (V "var")))
    ~?= Left (WarningMessage "Variable 'var' is not assigned")
  ]

test_freq_misused =
  "frequently misused commands tests"
  ~: TestList
  [
    checkRedirectInSudo (ExecCommand (ExecName "sudo") [Arg ">"]) Map.empty
      ~?= Left (WarningMessage "sudo is being redirected"),
    checkRedirectInSudo (ExecCommand (ExecName "sudo") [Arg "hi"]) Map.empty
      ~?= Right (ExecCommand (ExecName "sudo") [Arg "hi"]),
    checkRedirectionInFind (ExecCommand (ExecName "find") [Arg ">>"]) Map.empty
      ~?= Left (WarningMessage "Redirections is being used on find command"),
    checkRedirectionInFind (ExecCommand (ExecName "find") [Arg "file.txt"]) Map.empty
    ~?= Right (ExecCommand (ExecName "find") [Arg "file.txt"])
  ]

test_beginner_mistakes =
  "beginner mistakes tests"
  ~: TestList
  [
    checkCommaSeparatedArrays (Arr "1,2,3")
      ~?= Left (WarningMessage "Use spaces to separate array elements"),
    checkCommaSeparatedArrays (Arr "1,2  ,3  ")
      ~?= Left (WarningMessage "Use spaces to separate array elements")
  ]

test_style =
  "style tests"
  ~: TestList
  [
    unstylisticInterpolation (SingleQuote [ArgS "`hi`"])
      ~?= Left (WarningMessage "Backticks are being used, which has been deprecated - use $() instead"),
    unstylisticInterpolation (SingleQuote [ArgS "$(hi)"])
      ~?= Right (SingleQuote [ArgS "$(hi)"]),
    oldArithExpansionArg (Arg "$[sdf]")
      ~?= Left (WarningMessage "Old arithmetic expansion is being used in $[sdf] - use $((..)) instead"),
    oldArithExpansionArg (Arg "$((2+3)))")
      ~?= Right (Arg "$((2+3)))"),
    argArithmeticExpansion (Arg "$(( $v ))")
      ~?= Left (WarningMessage "$ is being used in $(( $v )) - don't use $ on variables in $((..))"),
    argArithmeticExpansion (Arg "$(( 1 ))")
  ~?= Right (Arg "$(( 1 ))")
  ]

test_data_typing =
  "data and typing error tests"
  ~: TestList
  [
    checkPrintArgCount (ExecCommand (ExecName "printf") [DoubleQuote [ArgS "%s"]]) Map.empty
      ~?= Left (WarningMessage "printf in `printf \"%s\"` has incorrect number of arguments"),
    checkPrintArgCount (ExecCommand (ExecName "printf") [DoubleQuote [ArgS "%s"], Arg "hi"]) Map.empty
      ~?= Right (ExecCommand (ExecName "printf") [DoubleQuote [ArgS "%s"], Arg "hi"])
  ]

{- Miscellaneous -}
test_miscellaneous =
  "miscellaneous tests"
   ~: TestList
  [
    checkVariableAssignedToItself "var" (Var (V "var"))
      ~?= Left (WarningMessage "variable var is assigned to itself - this does not do anything"),
    checkVariableAssignedToItself "var" (Var (V "v2"))
      ~?= Right (Var (V "v2"))
  ]

shellTests :: Test
shellTests =
  TestList [
    test_quoting,
    test_beginner_mistakes,
    test_conditionals,
    test_data_typing,
    test_freq_misused,
    test_style,
    test_miscellaneous
  ]

runShellTests :: IO Counts
runShellTests = runTestTT shellTests
