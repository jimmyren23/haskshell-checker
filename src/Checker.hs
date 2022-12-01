module Checker where

import Parsing
import ShellSyntax

-- Quoting

-- | Check if a variable is quoted
checkUnquotedVar :: Var -> Either String Var
checkUnquotedVar = undefined

checkQuotedTildeExpansion :: Value -> Either String Value
checkQuotedTildeExpansion = undefined

checkSingleQuoteApostrophe :: Value -> Either String Value
checkSingleQuoteApostrophe = undefined

checkEscapeQuote :: Value -> Either String Value
checkEscapeQuote = undefined

checkVarInSingleQuotes :: Value -> Either String Value
checkVarInSingleQuotes = undefined

-- single quote vs. double quote when used with $ for interpolation (double quote -> interpolation)

-- | Checks that the variable being interpolated has been defined previously
checkVarInterpolation :: Value -> Either String Value
checkVarInterpolation = undefined

-- Conditionals

-- | if the variable assingment is missing spaces, then it will return an error message.
checkMissingSpaces :: Parser BashCommand -> Maybe String
checkMissingSpaces = undefined

-- | Checks if Expression to see if conditional is implicitly always true through mislabeled expression
checkLiteralVacuousTrue :: Expression -> Either String Expression
checkLiteralVacuousTrue = undefined

-- | | Checks if Quoted regex in =~
checkQuotedRegex :: Value -> Either String Value
checkQuotedRegex = undefined

-- | Checks if unsupported operators are used
-- TODO: Define a list of supported operators in ShellSyntax.hs
checkUnsupportedOperators :: Expression -> Either String Expression
checkUnsupportedOperators = undefined

-- | Check if strings and numbers are being compared
checkNumericalStrComparison :: BashCommand -> Either String BashCommand
checkNumericalStrComparison = undefined

-- | Checks that && is not used inside [] statement
checkAnd :: Expression -> Either String Expression
checkAnd = undefined

-- Using test operators in ((..))
checkTestOperators :: Expression -> Either String Expression
checkTestOperators = undefined

-- Accidental backgrounding and piping
checkBackgroundingAndPiping :: Expression -> Either String Expression
checkBackgroundingAndPiping = undefined

-- Freq Misused Commands
--  # Redirecting sudo
checkRedirectInSudo :: BashCommand -> Either String BashCommand
checkRedirectInSudo = undefined

-- # Defining aliases with arguments
checkArgumentsInAliases :: BashCommand -> Either String BashCommand
checkArgumentsInAliases = undefined

-- # Redirections in find
checkRedirectionInFind :: BashCommand -> Either String BashCommand
checkRedirectionInFind = undefined

-- Beginner Mistakes
--  # Spaces around = in assignments
checkSpacesInAssignments :: BashCommand -> Either String BashCommand
checkSpacesInAssignments = undefined

--  # $ in assignments
checkDollarSignInAssignments :: BashCommand -> Either String BashCommand
checkDollarSignInAssignments = undefined

--  # Comma separated arrays
checkCommaSeparatedArrays :: BashCommand -> Either String BashCommand
checkCommaSeparatedArrays = undefined

-- # Using 'else if'
checkElseIf :: BashCommand -> Either String BashCommand
checkElseIf = undefined

-- # Using function before definition
checkUndefinedFunction :: BashCommand -> Either String BashCommand
checkUndefinedFunction = undefined

-- # 'false' being true
checkSingleFalse :: BashCommand -> Either String BashCommand
checkSingleFalse = undefined

-- # Using (..) instead of test
checkParenthesisInsteadOfTest :: BashCommand -> Either String BashCommand
checkParenthesisInsteadOfTest = undefined

-- Style
-- # Use $() instead
checkCommandSubstitution :: BashCommand -> Either String BashCommand
checkCommandSubstitution = undefined

-- # Use standard $((..)) instead of old $[]

--  # Don't use $ on variables in $((..))
-- # Useless use of echo
-- # Useless use of cat

-- Data and typing errors
-- # Comparing numbers as strings

-- Robustness
