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

    -- "-nt", "-ot", "-ef", "==", "!=", "<=", ">=", "-eq", "-ne", "-lt", "-le",
    -- "-gt", "-ge", "=~", ">", "<", "=", "\\<", "\\>", "\\<=", "\\>="
-- parses binary operators

bopP :: Parser Bop
bopP =
  choice
    [ constP "-gt" GtN,
      constP "-lt" LtN,
      constP "-eq" EqN,
      constP "+" Plus,
      constP "-" Minus,
      constP "*" Times,
      constP "//" Divide,
      constP "%" Modulo,
      constP "==" Eq,
      constP ">=" Ge,
      constP ">" Gt,
      constP "<=" Le,
      constP "<" Lt,
      constP ".." Concat,
      constP "&&" And
    ]

ifBopP :: Parser IfBop
ifBopP =
  choice
    [
      constP "-nt" Nt,
      constP "-ot" Ot,
      constP "-ef" Ef,
      constP "-ge" GeNIf,
      constP "-le" LeNIf,
      constP "-ne" NeN,
      constP "-gt" GtNIf,
      constP "-lt" LtNIf,
      constP "-eq" EqNIf,
      constP "!=" Ne,
      constP "==" EqIf,
      constP ">=" GeIf,
      constP ">" GtIf,
      constP "<=" LeIf,
      constP "<" LtIf,
      constP "&&" AndIf,
      constP "=~" Reg,
      constP "=" EqS,
      errP Err
    ]

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

{- Parsers for quoted strings -}

-- | parses unallowed tokens in quotes
errorStrParser :: Parser String
errorStrParser =
  -- constP "\'" "<singleQuote>" -- single quote
  constP "\\'" "<escape>" <|> constP "~" "<tilde>"

--
-- Since ' can be used in double quoted string and vice versa, inner has to be defined separately
--
innerDq :: Parser String
innerDq = many (satisfy (/= '\"'))

innerSq :: Parser String
innerSq = many (satisfy (/= '\''))

-- | Double quoted string - extracts out pure string only
dqStringValP :: Parser String
dqStringValP = between (char '\"') innerDq (wsP (char '\"'))

-- | Double quoted string - parses tokens that aren't allowed as well
dqStringValErrP :: Parser [Token]
dqStringValErrP = between (char '\"') (many (errorStrParser <|> (word <* many space))) (char '\"')

-- | Single quoted string - extracts out pure string only
sqStringValP :: Parser String
sqStringValP = between (char '\'') innerSq (wsP (char '\''))

-- | Single quoted string - parses tokens that aren't allowed as well
sqStringValErrP :: Parser [Token]
sqStringValErrP = between (char '\'') (many (errorStrParser <|> wsP word)) (char '\'')

-- >>> parse dqStringValErrP "\"~\""
-- Right ["<tilde>"]

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
    baseP = Var <$> variableRef <|> Val <$> valueP

ifExpP :: Parser IfExpression
ifExpP = bopexpP
  where
    bopexpP = baseP `chainl1` (flip IfOp2 <$> ifBopP)
    -- uopexpP =
    --   baseP
    --     <|> Op1 <$> uopP <*> uopexpP
    baseP = IfVar <$> variableRef <|> IfVal <$> valueP



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
possibleAssignP = PossibleAssign <$> (wsAssignP <|> dsAssignP)

-- | Parses var with whitespaces (retains them, too)
wsAssignP :: Parser PossibleAssign
wsAssignP = PossibleAssignWS <$> (V <$> name) <*> many (char ' ') <*> string "=" <*> many (char ' ') <*> wsP expP

-- | Parses assignments with $ in front of var name
dsAssignP :: Parser PossibleAssign
dsAssignP = PossibleAssignDS <$> (stringP "$" *> (V <$> name)) <*> many (char ' ' <|> char '=') <*> wsP expP

-- >>> parse possibleAssignP "a =1"
-- Right (PossibleAssign (PossibleAssignWS (V "a") " " "=" "" (Val (IntVal 1))))

-- >>> parse possibleAssignP "$a=7"
-- Left "No parses"

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
argP = Arg <$> word

-- | parses quoted string as an arg
argsP :: Parser Arg
argsP = (SingleQuote <$> sqStringValErrP) <|> (DoubleQuote <$> dqStringValErrP)

execCommandP :: Parser BashCommand
execCommandP = ExecCommand <$> commandP <*> many (argP <|> argsP) <* many (char '\n')

-- >>> parse execCommandP "echo \"hi\"\n"
-- Right (ExecCommand (ExecName "echo") [DoubleQuote ["hi"]])

-- >>> parse execCommandP "ls -l -a w$few wefjkl"
-- Right (ExecCommand (ExecName "ls") [Arg "-l",Arg "-a",Arg "w$few",Arg "wefjkl"])

-- >>> parse execCommandP "ls && -a"
-- Right (ExecCommand (ExecName "ls") [Arg "&&",Arg "-a"])

-- >>> parse execCommandP "&& -l -a"
-- Left "No parses"

conditionalStrP :: Parser String
conditionalStrP = choice [wsP $ string "", wsP (string "if ["), wsP $ many get, wsP (string "fi")]

-- >>> parse conditionalStrP "if [$y < 0] \nthen\n  x=2\nelse\n  x=3\nfi\n"
-- Left "if [$y < 0] \nthen\n  x=2\nelse\n  x=3\nfi\n"

conditionalP :: Parser BashCommand
conditionalP =
-- "if [y=1] \nthen\n  x=2\nelse\n  x=3\nfi\n"
  Conditional <$> ((wsP (string "if [") *> wsP ifExpP <* wsP (string "]")) <|> (wsP (string "if [[") *> wsP ifExpP <* wsP (string "]]")))
    <*> (wsP (string "then") *> wsP blockP)
    <*> (wsP (string "else") *> wsP blockP <* wsP (string "fi"))

-- >>> parse ((wsP (string "then") *> wsP blockP)) "\nthen\n  x=2"
-- Left "No parses"

-- >>> parse (wsP (string "else") *> wsP blockP <* wsP (string "fi")) "else\n  x=3\nfi\n"
-- Right (Block [Assign (V "x") (Val (IntVal 3))])

-- >>> parse (wsP (string "if [") *> wsP blockP <* string "]") "if [y=1]"

bashCommandP :: Parser BashCommand
bashCommandP = assignP <|> conditionalP <|> possibleAssignP <|> execCommandP

variableRef :: Parser Var
variableRef = V <$> (char '$' *> wsP word)

argUnquotedVar :: Parser Arg
argUnquotedVar = Arg <$> (char '$' *> wsP word)

-- >>> parse bashCommandP "echo \"hi\""
-- Right (ExecCommand (ExecName "echo") [DoubleQuote ["hi"]])

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

blockP :: Parser Block
blockP = Block <$> many (wsP bashCommandP)

{- Script parser -}
parseShellScript :: String -> IO (Either ParseResult Block)
parseShellScript = parseFromFile (const <$> blockP <*> eof)

word2 :: Parser String
word2 = (:) <$> satisfy (/= '\n') <*> many (satisfy (/= '\n'))

-- newlineP :: Parser String
-- newlineP =

-- >>> parse newlineP "y=1\nif [$y -lt 1] \nthen\n  x=2\nelse\n  x=3\nfi\n"
-- Left "y=1\nif [$y -lt 1] \nthen\n  x=2\nelse\n  x=3\nfi\n"

-- How it looks : "if [y < 0] \nthen\n  x=2\nelse\n  x=3\nfi\n"

-- >>> parse expP "1"
-- Right (Val (IntVal 1))

-- >>> parse expP "$y < 1"
-- Right (Op2 (Var (V "y")) Lt (Val (IntVal 1)))

-- >>> parseShellScript "test/conditional.sh"
-- Right (Block [Assign (V "y") (Val (IntVal 1)),PossibleAssign (PossibleAssignWS (V "x") " " "=" " " (Val (IntVal 1))),Conditional (Op2 (Var (V "x")) And (Val (StringVal "hi"))) (Block [Assign (V "x") (Val (IntVal 4))]) (Block [Assign (V "x") (Val (IntVal 3))])])

p :: String -> Block -> IO ()
p fn ast = do
  result <- parseShellScript fn
  case result of
    (Left _) -> assert False
    (Right ast') -> assert (ast == ast')
