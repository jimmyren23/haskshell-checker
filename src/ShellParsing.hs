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

-- Parses the first character of a name
startOfName :: Parser Char
startOfName = char '_' <|> alpha <|> lower <|> upper

-- Parses valid character from the rest of a name
restOfName :: Parser Char
restOfName = startOfName <|> digit

-- parses a name from a string
name :: Parser String
name = (:) <$> startOfName <*> many restOfName

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

-- >>> parse (many boolValP) "true false\n true"
-- Right [BoolVal True,BoolVal False,BoolVal True]
boolValP :: Parser Value
boolValP = BoolVal <$> wsP (constP "true" True <|> constP "false" False)

-- >>> parse (many nilValP) "nil nil\n nil"
-- Right [NilVal,NilVal,NilVal]
nilValP :: Parser Value
nilValP = wsP (constP "nil" NilVal)

-- | Since ' can be used in double quoted string and vice versa, inner has to be defined separately

innerDq :: Parser String
innerDq = many (satisfy (/= '\"'))

innerSq :: Parser String
innerSq = many (satisfy (/= '\''))

-- | Double quoted string
dqStringValP :: Parser String
dqStringValP = between (char '\"') innerDq (wsP (char '\"'))

-- | Single quoted string
sqStringValP :: Parser String
sqStringValP = between (char '\'') innerSq (wsP (char '\''))


-- | Double quoted string
dqStringValArgP :: Parser [Arg]
dqStringValArgP = between (char '\"') (many (Arg <$> errorStrParser <|> argP)) (wsP (char '\"'))

-- | Single quoted string
sqStringValArgP :: Parser [Arg]
sqStringValArgP = between (char '\'') (many (Arg <$> errorStrParser <|> argP)) (wsP (char '\''))


-- >>> parse sqStringValArgP "\'\\'klj\'"
-- Right [Arg "<escape>",Arg "lj"]

stringValP :: Parser Value
stringValP = StringVal <$> (dqStringValP <|> sqStringValP)

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
assignP = (Assign . V <$> name) <*> (char '=' *> expP)

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
possibleAssignP = wsAssignP <|> dsAssignP

wsAssignP :: Parser BashCommand
wsAssignP = PossibleAssign <$> wsP (V <$> name) <* wsP (char '=') <*> wsP expP

-- | Parses assignments with $ in front of var name
dsAssignP :: Parser BashCommand
dsAssignP =  stringP "$" *> wsAssignP



-- >>> parse possibleAssignP "a = 10"
-- Right (PossibleAssign (V "a") (Val (IntVal 10)))

-- >>> parse possibleAssignP "$a=7"

-- Right (PossibleAssign (V "a") (Val (IntVal 7)))
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
commandP = ExecName <$> wsP (filter isSpecial name)
  where
    isSpecial = not . (`elem` reserved ++ operators)

-- | parses single word as an arg
argP :: Parser Arg
argP = Arg <$> wsP word


-- | parses quoted string as an arg
argsP :: Parser Arg
argsP = (SingleQuote <$> sqStringValArgP) <|> (DoubleQuote <$> dqStringValArgP)

-- | parses unallowed strs in quotes
errorStrParser :: Parser String
errorStrParser =
  constP ">>" "<singleQuote>" -- single quote
  <|> constP "\\'" "<escape>" <* satisfy isAlpha
  <|> constP "~/" "<tilde>"

execCommandP :: Parser BashCommand
execCommandP = ExecCommand <$> commandP <*> many (argP <|> argsP)

-- >>> parse execCommandP "echo \"hi hello\""
-- Right (ExecCommand (ExecName "echo") [DoubleQuote [Arg "hi",Arg "hello"]])

-- >>> parse sqStringValP "'hi'"
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

-- >>> parse bashCommandP "echo '$hi'"
-- Left "'$hi'"

-- >>> parse bashCommandP "ls -l -a awefew wefjkl"
-- Right (ExecCommand (ExecName "ls") [Arg "-l",Arg "-a",Arg "awefew",Arg "wefjkl"])
