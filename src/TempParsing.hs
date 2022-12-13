{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module TempParsing where

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
import ShellParsing qualified as P
import ShellSyntax
import State qualified as S
import Test.HUnit (Assertion, Counts, Test (..), assert, runTestTT, (~:), (~?=))
import Prelude hiding (filter)

-- Parses the first character of a name
startOfName :: Parser Char
startOfName = char '_' <|> alpha <|> lower <|> upper

-- Parses valid character from the rest of a name
restOfName :: Parser Char
restOfName = startOfName <|> digit

-- parses a name from a string
name :: Parser Var
name = V <$> ((:) <$> startOfName <*> many restOfName)

-- parses binary operators
bopP :: Parser Bop
bopP =
  constP "+" Plus
    <|> constP "-" Minus
    <|> constP "*" Times
    <|> constP "//" Divide
    <|> constP "%" Modulo
    <|> constP ".." Concat
    <|> constP "<=" Le
    <|> constP "<" Lt
    <|> constP ">=" Ge
    <|> constP ">" Gt
    <|> constP "==" Eq

-- | Parse an operator at a specified precedence level
opAtLevel :: Int -> Parser (Expression -> Expression -> Expression)
opAtLevel l = flip Op2 <$> filter (\x -> level x == l) bopP

level :: Bop -> Int
level Times = 7
level Divide = 7
level Plus = 5
level Minus = 5
level Concat = 4
level _ = 3 -- comparison operators

-- | Parses unary operators
uopP :: Parser Uop
uopP = constP "-" Neg <|> constP "not" Not

intValP :: Parser Value
intValP = IntVal <$> wsP int

-- >>> P.parse (many boolValP) "true false\n true"
-- Right [BoolVal True,BoolVal False,BoolVal True]
boolValP :: Parser Value
boolValP = BoolVal <$> wsP (constP "true" True <|> constP "false" False)

-- >>> P.parse (many nilValP) "nil nil\n nil"
-- Right [NilVal,NilVal,NilVal]
nilValP :: Parser Value
nilValP = wsP (constP "nil" NilVal)

inner :: Parser String
inner = many (satisfy (/= '\"'))

stringValP :: Parser Value
stringValP = StringVal <$> between (char '\"') inner (wsP (char '\"'))

-- | parses different values
valueP :: Parser Value
valueP = intValP <|> boolValP <|> nilValP <|> stringValP

expP :: Parser Expression
expP = compP
  where
    compP = catP `chainl1` opAtLevel (level Gt)
    catP = sumP `chainl1` opAtLevel (level Concat)
    sumP = prodP `chainl1` opAtLevel (level Plus)
    prodP = uopexpP `chainl1` opAtLevel (level Times)
    uopexpP =
      baseP
        <|> Op1 <$> uopP <*> uopexpP
    baseP = Val <$> valueP

-- | Parses a line of input for an assignment
assignP :: Parser BashCommand
assignP = Assign <$> name <*> (char '=' *> expP)

test_assign :: Test
test_assign =
  TestList
    [ parse assignP "hi=31" ~?= Right (Assign (V "hi") (Val (IntVal 31)))
    ]

-- >>> runTestTT test_assign
-- Counts {cases = 1, tried = 1, errors = 0, failures = 0}

-- >>> parse expP "31"
-- Right (Val (IntVal 31))

possibleAssignP :: Parser BashCommand
possibleAssignP = PossibleAssign <$> wsP name <* wsP (char '=') <*> wsP expP

-- >>> parse possibleAssignP "\"a\"= 10"
-- Left "No parses"

-- >>> parse possibleAssignP "a= 3"
-- Right (PossibleAssign (V "a") (Val (IntVal 3)))

-- >>> parse possibleAssignP "a =3"
-- Right (PossibleAssign (V "a") (Val (IntVal 3)))

-- | parses anything thats not an operator, quote, or space
notQuoteOrSpaceP :: Parser Char
notQuoteOrSpaceP = satisfy (\c -> c /= '"' && c /= '\'' && not (isSpace c))

-- parses a name from a string
word :: Parser String
word = (:) <$> notQuoteOrSpaceP <*> many notQuoteOrSpaceP

reserved :: [String]
reserved = ["!", "fi", "then", "elif", "else", "if"]

operators :: [String]
operators = ["&&", "||", ";;", "<<", ">>", "<&", ">&", "<>", "<<-", ">|", "+", "-", "*", "/", "%", "=", "==", "!=", "<", ">", "<=", ">=", "!", "(", ")", "{", "}", "[", "]", ";", "&", "|", ">", "<", ">>", "<<", "<<<", ">>>"]

-- parses command name
commandP :: Parser Command
commandP = ExecName <$> wsP (filter isSpecial P.name)
  where
    isSpecial = not . (`elem` reserved ++ operators)

argsP :: Parser [Arg]
argsP = many (Arg <$> wsP word)

execCommandP :: Parser BashCommand
execCommandP = ExecCommand <$> commandP <*> argsP

-- >>> parse execCommandP "ls -l"
-- Right (ExecCommand (ExecName "ls") [Arg "-l"])

-- >>> parse execCommandP "$ls -l"
-- Left "No parses"

-- >>> parse execCommandP "ls -l -a w$few wefjkl"
-- Right (ExecCommand (ExecName "ls") [Arg "-l",Arg "-a",Arg "w$few",Arg "wefjkl"])

-- >>> parse execCommandP "ls && -a"
-- Right (ExecCommand (ExecName "ls") [Arg "&&",Arg "-a"])

-- >>> parse execCommandP "&& -l -a"
-- Left "No parses"

bashCommandP :: Parser BashCommand
bashCommandP = assignP <|> possibleAssignP <|> execCommandP

variableRef :: Parser String
variableRef = (:) <$> char '$' *> word

-- >>> parse variableRef "$x"
-- Right "x"

-- >>> parse variableRef "$xfwejklfj"
-- Right "xfwejklfj"

-- >>> parse variableRef "$xewf\""
-- Left "\""

innerArithmetic :: Parser String
innerArithmetic = many (satisfy (/= ')'))

arithmeticExpansion :: Parser String
arithmeticExpansion = stringP "$" *> between (stringP "(") (between (stringP "(") innerArithmetic (stringP ")")) (stringP ")")

-- >>> parse arithmeticExpansion "$((3 + 4))"
-- Right "3 + 4"

-- >>> parse bashCommandP "x=3"
-- Right (Assign (V "x") (Val (IntVal 3)))

-- >>> parse bashCommandP "x = 3"
-- Right (PossibleAssign (V "x") (Val (IntVal 3)))

-- >>> parse bashCommandP "ls -l"
-- Right (ExecCommand (ExecName "ls") [Arg "-l"])

-- >>> parse bashCommandP "ls -l -a awefew wefjkl"
-- Right (ExecCommand (ExecName "ls") [Arg "-l",Arg "-a",Arg "awefew",Arg "wefjkl"])
