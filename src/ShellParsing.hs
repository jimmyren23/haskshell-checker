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

operators :: [String]
operators = ["&&", "||", ";;", "<<", ">>", "<&", ">&", "<>", "<<-", ">|", "+", "-", "*", "/", "%", "=", "==", "!=", "<", ">", "<=", ">=", "!", "(", ")", "{", "}", "[", "]", ";", "&", "|", ">", "<", ">>", "<<", "<<<", ">>>"]

-- | Parses operators
operatorParser :: Parser Token
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
quoteParser :: Parser Token
quoteParser =
  between (stringP "\"") (many (satisfy (/= '"'))) (stringP "\"") <|> between (stringP "'") (many (satisfy (/= '\''))) (stringP "'")

-- | parses anything thats not an operator, quote, or space
notQuoteOrSpaceP :: Parser Char
notQuoteOrSpaceP = satisfy (\c -> c /= '"' && c /= '\'' && not (isSpace c))

-- parses a name from a string
word :: Parser String
word = (:) <$> notQuoteOrSpaceP <*> many notQuoteOrSpaceP

wordParser :: Parser Token
wordParser = wsP word

-- >>> parse (many wordP) "x sfds _ nil "
-- Right ["x","sfds","_","nil"]

-- -- | parses a line of input into a list of words
tokenParser :: Parser String
tokenParser = quoteParser <|> operatorParser <|> wordParser

-- tokenizer :: Parser [String]
tokenizer = many tokenParser

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
      parse wordParser "l" ~?= Right "l",
      parse quoteParser "\"echo\"" ~?= Right "echo",
      parse quoteParser "'echo'" ~?= Right "echo",
      parse quoteParser "'echo" ~?= Left "No parses",
      parse quoteParser "\"echo" ~?= Left "No parses",
      parse tokenizer "l fejwklf && fjej" ~?= Right ["l", "fejwklf", "&&", "fjej"],
      parse tokenizer "l fejwklf && fjej \"ewjkfjwelkfj\"" ~?= Right ["l", "fejwklf", "&&", "fjej", "ewjkfjwelkfj"],
      parse tokenizer "l fejwklf && fjej \"ewjkfjwelkfj" ~?= Left "\"ewjkfjwelkfj",
      parse tokenizer "l \"ewfjklew && ejwkflej \" weff" ~?= Right ["l", "ewfjklew && ejwkflej ", "weff"],
      parse tokenizer "x=5" ~?= Right ["x=5"]
    ]

-- >>> parse tokenizer "\"a\"=3"
-- Left "\"ewjkfjwelkfj"

-- >>> runTestTT test_tokenizer
-- Counts {cases = 18, tried = 18, errors = 0, failures = 0}

reserved :: [String]
reserved = ["!", "fi", "then", "elif", "else", "if"]

-- Parses the first character of a name
startOfName :: Parser Char
startOfName = char '_' <|> alpha <|> lower <|> upper

-- Parses valid character from the rest of a name
restOfName :: Parser Char
restOfName = startOfName <|> digit

-- parses a name from a string
name :: Parser String
name = (:) <$> startOfName <*> many restOfName

nameP :: Parser NameToken
nameP = wsP name
  where
    isReserved = not . (`elem` reserved)
