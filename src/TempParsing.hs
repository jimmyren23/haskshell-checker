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
import ShellSyntax
import Test.HUnit (Assertion, Counts, Test (..), assert, runTestTT, (~:), (~?=))
import Prelude hiding (filter)

-- Untyped shell

-- | A parser is a function that takes a string and returns untyped shell

-- Parses the first character of a name
startOfName :: Parser Char
startOfName = char '_' <|> alpha <|> lower <|> upper

-- Parses valid character from the rest of a name
restOfName :: Parser Char
restOfName = startOfName <|> digit

-- parses a name from a string
name :: Parser Var
name = V <$> ((:) <$> startOfName <*> many restOfName)

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

opAtLevel :: Int -> Parser (Expression -> Expression -> Expression)
opAtLevel l = flip Op2 <$> filter (\x -> level x == l) bopP

level :: Bop -> Int
level Times = 7
level Divide = 7
level Plus = 5
level Minus = 5
level Concat = 4
level _ = 3 -- comparison operators

uopP :: Parser Uop
uopP = constP "-" Neg <|> constP "not" Not

valueP :: Parser Value
valueP = intValP <|> boolValP <|> nilValP <|> stringValP

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
