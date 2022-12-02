module CheckerHUnitTests where

import Checker
import Test.HUnit  (runTestTT,Test(..),Assertion, (~?=), (~:), assert, Counts)

-- TODO: Decide on error messages (use the same as Shell Check or no?)

test_quoting =
  "quoting"
    ~: TestList 
    [
      checkUnquotedVar Var "$1" ~= Left "Error: No quotes around variable"
      checkUnquotedVar "echo \"$1\"" ~= Right Var "$1"
      checkQuotedTildeExpansion "\"~/5520 file.txt\"" ~= Left "Error: Misplaced quotes around tilde expression"
      checkQuotedTildeExpansion "~/5520 file.txt" ~= Right "rm ~/5520 file.txt"
      checkSingleQuoteApostrophe "\'it's not right\'" ~= Left "Error: Single quote closed by apostrophe"
      checkSingleQuoteApostrophe "\'it'\''s a string\'" ~= Right "\'it'\''s a string\'"
      checkEscapeQuote "\'it\'s a string\'" ~= Left "Error: Can't escape single quote inside single quotes"
      checkVarInSingleQuotes "\'$1\'" ~= Left "Error: Variable can't be used in single quotes"
    ]

test_conditional =
  "conditional"
  ~: TestList 
  [
    checkMissingSpaces Right Conditional "if [\"$foo\"==0]\n" ["then echo \"true\"\n", "else echo \"false\"\n", "fi"] ~= Left "Error: Missing spaces around equal-to operator"
    -- TODO: Decide on how to store conditionals (when to split up conditions & blocks)
    checkMissingSpaces Right Conditional "if [\"$foo\" == 0]\n" ["then echo \"true\"\n", "else echo \"false\"\n", "fi"] ~= Right Conditional "if [\"$foo\" == 0]\n" ["then echo \"true\"\n", "else echo \"false\"\n", "fi"]
    checkLiteralVacuousTrue Right Conditional "if [-n \"$foo \"]\n" ["then echo \"true\"\n", "else echo \"false\"\n", "fi"] ~= Left "Warning: Condition always evaluates to true"
    checkLiteralVacuousTrue Right Conditional "if [-n \"$foo\"]\n" ["then echo \"true\"\n", "else echo \"false\"\n", "fi"] ~= Right Conditional "if [-n \"$foo\"]\n" ["then echo \"true\"\n", "else echo \"false\"\n", "fi"]
  ]
