module CheckerHUnitTests where

import Checker
import Test.HUnit (Assertion, Counts, Test (..), assert, runTestTT, (~:), (~?=))

-- TODO: Decide on error messages (use the same as Shell Check or no?)

test_quoting =
  "quoting"
    ~: TestList
      [ checkUnquotedVar
          Var
          "$1"
          ~= Left
            "Info: Double quoting to interpolate variable"
            checkUnquotedVar
            Var
            "\"$1\""
          ~= Right
            Var
            "$1"
            checkQuotedTildeExpansion
            "\"~/5520 file.txt\""
          ~= Left
            "Warning: Tilde does not expand inside quotes."
            checkQuotedTildeExpansion
            "~/5520 file.txt"
          ~= Right
            "rm ~/5520 file.txt"
            checkSingleQuoteApostrophe
            "\'it's not right\'"
          ~= Left
            "Error: Cannot parse the string, apostrophe is terminating the single quoted string."
            checkSingleQuoteApostrophe
            "\'it'\''s a string\'"
          ~= Right
            "\'it'\''s a string\'"
            checkEscapeQuote
            "\'it\'s a string\'"
          ~= Left
            "Error:  Cannot parse the string, escaping apostrophe is not done properly."
            checkVarInSingleQuotes
            "\'$1\'"
          ~= Left "Info: Expressions do not expand inside single quotes."
      ]

test_conditional =
  "conditional"
    ~: TestList
      [ checkMissingSpaces Right Expression "if [\"$foo\"==0]\n"
          ~= Left
            "Error: Missing spaces around comparison operator."
            -- TODO: Decide on how to store conditionals (when to split up conditions & blocks)
            checkMissingSpaces
            Right
            Expression
            "if [\"$foo\" == 0]\n"
          ~= Right
            Expression 
            "if [\"$foo\" == 0]\n"
            checkLiteralVacuousTrue
            Right
            Expression
            "if [-n \"$foo \"]\n"
          ~= Left
            "Warning: Condition always evaluates to true."
            checkLiteralVacuousTrue
            Right
            Expression
            "if [-n \"$foo\"]\n"
          ~= Right
            Expression
            "if [-n \"$foo\"]\n"
            checkQuotedRegex
            Right
            Expression
            "if [[ $foo =~ "
            fo
          + " ]]"
          ~= Left
            "Warning: Regex is quoted"
            checkQuotedRegex
            Right
            Expression 
            "if [$1 -eq \"hi\"]\n"
          ~= Left
            "Error: Invalid use of -eq, use = to compare strings."
            checkAnd
            Right
            Expression 
            "if [$a && $b]"
          ~= Left
            "Error: && inside ((..)), use [..] && [..] instead."
            checkTestOperators
            Right
            Expression 
            "if [ ((2 -gt 1)) ]"
          ~= Left
            "Error: Test oprators are not valid inside ((..))."
            checkBackgroundingAndPiping
            Right
            Expression 
            "if [ [ x ] & [ y ] | [ z ] ]"
          ~= Left
            "Error: Unintended use of backgrounding and piping."
      ]

test_freq_misused =
  "freq_misused"
    ~: TestList
      [ checkRedirectInSudo Right ExecCommand "sudo echo \"hello\" > file.txt"
          ~= Left
            "Error: Redirecting sudo.",
        checkArgumentsInAliases Right ExecCommand "alias ls='ls -l'"
          ~= Left
            "Error: Arguments in aliases.",
        checkRedirectionInFind Right ExecCommand "find . -name \"*.txt\" > file.txt"
          ~= Left
            "Error: Redirecting find."
      ]

test_beginner_mistakes =
  "beginner_mistakes"
    ~: TestList
      [ checkSpacesInAssignments Right ExecCommand "foo = bar"
          ~= Left
            "Error: Spaces around equal-to operator, remove spaces.",
        checkDollarSignInAssignments Right ExecCommand "$foo = bar"
          ~= Left
            "Error: Variable in assignment, remove $.",
        checkCommaSeparatedArrays Right ExecCommand "foo=(bar, baz)"
          ~= Left
            "Error: Comma-separated arrays, use space-separated arrays.",
        checkElseIf Right ExecCommand "if [\"$foo\" == 0]\nthen echo \"true\"\nelse if [\"$foo\" == 1]\nthen echo \"false\"\nfi"
          ~= Left
            "Error: Use elif instead of else if",
        checkSingleFalse Right ExecCommand "if [ false ]\nthen echo \"true\"\n"
          ~= Left
            "Warning: The [ false ] is actually interpreted as true.",
        checkParenthesisInsteadOfTest Right ExecCommand "if ( -f file ) \nthen echo \"true\"\n"
          ~= Left
            "Error: Use test instead of ( )."
      ]

test_style =
  "style"
    ~: TestList
      [ checkCommandSubstitution Right ExecCommand "echo `date`"
          ~= Left
            "Warning: Instead of backticks, use $().",
        checkArithmeticParentheses Right ExecCommand "echo $[ 1 + 1 ]"
          ~= Left
            "Warning: Instead of $[...], use the new standard $((...)).",
        checkNoVarInArithemetic Right ExecCommand "echo $((NUM + 1))"
          ~= Left
            "Error: Use $[] instead of $().",
        checkEchoUsage Right ExecCommand "echo \"$(date)\""
          ~= Left
            "Warning: usage of echo here is unnecessary.",
        checkCatUsage Right ExecCommand "cat fileName | grep bar"
          ~= Left
            "Warning: usage of cat here is unnecessary."
      ]

test_data_type =
  "data and type errors"
    ~: TestList
      [ checkArrayReferenceInString Right ExecCommand "foo=(bar baz); echo $foo"
          ~= Left
            "Error: Try to use an array as a string.",
        checkStringArrayConcatenation Right ExecCommand "foo=(bar baz); z=\"hi\"; y=z+foo"
          ~= Left
            "Warning: Trying to concatenate an array with a string.",
        checkStringNumericalComparison Right ExecCommand "if [\"$foo\" -eq 0]\nthen echo \"true\"\n"
          ~= Left
            "Error: Trying to compare a string with a number.",
        checkUnusedVar Right ExecCommand "foo=bar; echo \"hi\" foo"
          ~= Left
            "Warning: Unused variable referred to in command.",
        checkUnassignedVar Right ExecCommand "echo $foo"
          ~= Left
            "Error: Unassigned variable referred to in command.",
        checkPipingRead Right ExecCommand "cat file.txt | echo \"hi\""
          ~= Left
            "Error: Piping into a command that does not intake input.",
        checkPrintArgCount Right ExecCommand "printf \'%s: %s\' foo"
          ~= Left
            "Error: An inccorect number of arguments were given to the command."
        checkArrayEval Right ExecCommand "eval \"${array[@]}\""
          ~= Left
            "Error: Word boundaries lost when attempting to read array."
      ]

test_robustness =
  "robustness"
    ~: TestList
      [ checkNoVariablesInPrintf Right ExecCommand "printf $myvar"
          ~= Left
            "Error: No variables should be used in printf."
      ]