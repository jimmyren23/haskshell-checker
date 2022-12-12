{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module ShellParsing where

import Control.Applicative
import Control.Monad (guard)
import Data.Char
  ( Char,
    isAlpha,
    isDigit,
    isLower,
    isSpace,
    isUpper,
  )
import Data.Foldable
import Data.Map ()
import Parsing
import ShellSyntax
import Test.HUnit (Assertion, Counts, Test (..), assert, runTestTT, (~:), (~?=))
import Prelude hiding (filter)

-- Untyped shell

-- | A parser is a function that takes a string and returns untyped shell

-- | Parses a line of input for an assignment
assignUntypedP :: Parser BashCommand
assignUntypedP = undefined

-- | Parses a line of input for a conditional statement into untyped conditional
conditionalUntypedP :: Parser BashCommand
conditionalUntypedP = undefined

-- | Parses a line of input for for exec commands and arguments
-- example: "ls -l" would need to be broken down into
execCommandUntypedP :: Parser BashCommand
execCommandUntypedP = undefined

bashCommandP :: Parser BashCommand
bashCommandP = assignUntypedP <|> conditionalUntypedP <|> execCommandUntypedP

reserved :: [String]
reserved = ["!", "fi", "then", "elif", "else", "if"]

operators :: [String]
operators = ["&&", "||", ";;", "<<", ">>", "<&", ">&", "<>", "<<-", ">|", "+", "-", "*", "/", "%", "=", "==", "!=", "<", ">", "<=", ">=", "!", "(", ")", "{", "}", "[", "]", ";", "&", "|", ">", "<", ">>", "<<", "<<<", ">>>"]

-- | Parses operators
operatorParser :: Parser String
operatorParser =
  constP "&&" "&&"
    <|> constP "||" "||"
    <|> constP ";;" ";;"
    <|> constP "<<" "<<"
    <|> constP ">>" ">>"
    <|> constP "<&" "<&"
    <|> constP ">&" ">&"
    <|> constP "<>" "<>"
    <|> constP "<<-" "<<-"
    <|> constP ">|" ">|"
    <|> constP "+" "+"
    <|> constP "-" "-"
    <|> constP "*" "*"
    <|> constP "/" "/"
    <|> constP "%" "%"
    <|> constP "=" "="
    <|> constP "==" "=="
    <|> constP "!=" "!="
    <|> constP "<" "<"
    <|> constP ">" ">"
    <|> constP "<=" "<="
    <|> constP ">=" ">="
    <|> constP "!" "!"
    <|> constP "(" "("
    <|> constP ")" ")"
    <|> constP "{" "{"
    <|> constP "}" "}"
    <|> constP "[" "["
    <|> constP "]" "]"
    <|> constP ";" ";"
    <|> constP "&" "&"
    <|> constP "|" "|"
    <|> constP ">" ">"
    <|> constP "<" "<"
    <|> constP ">>" ">>"
    <|> constP "<<" "<<"
    <|> constP "<<<" "<<<"
    <|> constP ">>>" ">>>"

-- | Parses single quotes or double quotes and everything in between them
quoteParser :: Parser String
quoteParser =
  between (stringP "\"") (many (satisfy (/= '"'))) (stringP "\"") <|> between (stringP "'") (many (satisfy (/= '\''))) (stringP "'")

-- | parses anything thats not an operator, quote, or space
notQuoteOrSpaceP :: Parser Char
notQuoteOrSpaceP = satisfy (\c -> c /= '"' && c /= '\'' && not (isSpace c))

-- parses a name from a string
word :: Parser String
word = (:) <$> notQuoteOrSpaceP <*> many notQuoteOrSpaceP
  where
    x = (:) <$> notQuoteOrSpaceP

wordP :: Parser Token
wordP = wsP word

-- >>> parse (many wordP) "x sfds _ nil "
-- Right ["x","sfds","_","nil"]

wordParser :: Parser Token
wordParser = wsP (many notQuoteOrSpaceP)

-- -- | parses a line of input into a list of words
-- tokenParser :: Parser String
-- tokenParser = wordParser

-- tokenizer :: Parser [String]
-- tokenizer = many notQuoteOrSpaceP

test_tokenizer :: Test
test_tokenizer =
  TestList
    [ parse notQuoteOrSpaceP "e" ~?= Right 'e',
      parse notQuoteOrSpaceP "\"" ~?= Left "No parses",
      parse notQuoteOrSpaceP " " ~?= Left "No parses",
      parse (many notQuoteOrSpaceP) "echo" ~?= Right "echo",
      parse (many notQuoteOrSpaceP) "echo " ~?= Left " ",
      parse wordParser "echo" ~?= Right "echo",
      parse wordParser "echo      " ~?= Right "echo",
      parse wordParser "echo      l" ~?= Left "l",
      parse wordParser "l" ~?= Right "l"
    ]

-- >>> parse wordParser "echo l"
-- Right "echo l"

-- >>> runTestTT test_tokenizer
-- Counts {cases = 9, tried = 9, errors = 0, failures = 0}
