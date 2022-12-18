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

-- | Parses a variable name
nonPrintFArg :: Parser String
nonPrintFArg = many (satisfy (/= '%'))

-- | Parser for format specificators in printf
formatSpecP :: Parser PrintfToken
formatSpecP =
  constP "%s" FormatSpec
    <|> constP "%d" FormatSpec
    <|> constP "%f" FormatSpec
    <|> constP "%c" FormatSpec
    <|> constP "%b" FormatSpec
    <|> constP "%x" FormatSpec
    <|> constP "%o" FormatSpec
    <|> constP "%e" FormatSpec
    <|> constP "%g" FormatSpec
    <|> constP "%a" FormatSpec

-- | Parser for all tokens in printf except format specificators
printfToken :: Parser PrintfToken
printfToken = Token <$> ((:) <$> satisfy (/= '%') <*> many (satisfy (/= '%')))

-- | Parser for printf
printfParser :: Parser [PrintfToken]
printfParser = many (formatSpecP <|> printfToken)

-- | Counts the number of format specificators in a printf
typeCounter :: [PrintfToken] -> Int
typeCounter = foldr (\x acc -> case x of FormatSpec -> acc + 1; _ -> acc) 0

-- | Counts the number of format specificators of the tokens inside a format of a printf
tokenPars :: [Token] -> Int
tokenPars (x : xs) = case parse printfParser x of
  Left _ -> tokenPars xs
  Right printfTokens -> helper printfTokens + tokenPars xs
    where
      helper = foldr (\x acc -> case x of FormatSpec -> acc + 1; _ -> acc) 0
tokenPars [] = 0

-- >>> parse printfParser "%s waejfklawjfe wefjklawfjl %s"
-- Right [FormatSpec,Token "waejfklawjfe wefjklawfjl ",FormatSpec]

-- >>> tokenPars ["%s awefjkwl", "%s", "wefjkwl %g"]
-- 3

-- >>> parse printfToken "eawjawefjklewkflw"
-- Right (Token "eawjawefjklewkflw")
-- >>> parse printfParser "%sewjfk%s"
-- Right [FormatSpec,Token "ewjfk",FormatSpec]

-- | Parses binary operators for non-conditional expressions
bopP :: Parser Bop
bopP =
  choice
    [ constP "+" Plus,
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

-- | Parses binary operators for conditional expressions
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
      constP "+" PlusIf,
      constP " - " MinusIf,
      constP "*" TimesIf,
      constP "//" DivideIf,
      constP "%" ModuloIf,
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

uopP :: Parser Uop
uopP = constP "-" Neg
  <|> constP "not" Not

-- | Parses unary operators
ifUopP :: Parser IfUop
ifUopP =
  choice [
    constP "!" NotIf,
    constP "-a" AndU,
    constP "-b" BlockSpecial,
    constP "-c" CharSpecial,
    constP "-d" FolderExists,
    constP "-e" FileOrFolderExists,
    constP "-f" FileExists,
    constP "-g" GroupId,
    constP "-h" Symlink,
    constP "-k" StickyBit,
    constP "-p" Pipe,
    constP "-r" ReadPermission,
    constP "-s" FileSize,
    constP "-S" Socket,
    constP "-t" Terminal,
    constP "-u" UserId,
    constP "-w" WritePermission,
    constP "-e" ExecPermission,
    constP "-O" Owner,
    constP "-G" GroupIdUser,
    constP "-N" Modified,
    constP "-z" LengthZero,
    constP "-n" LengthNonZero,
    constP "-o" Or
  ]

-- >>> parse ((wsP (string "if [") *> wsP ifExpP <* wsP (string "]"))) "if [ -as 2 ]"
-- Right (IfOp1 AndU (IfOp1 ErrU (IfVal (IntVal 2))))

intValP = IntVal <$> wsP int

boolValP :: Parser Value
boolValP = BoolVal <$> wsP (constP "true" True <|> constP "false" False)

{- Parsers for quoted strings -}

-- | Parses tokens that can't be used within strings
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

innerBacktick :: Parser String
innerBacktick = many (satisfy (/= '`'))

-- | Double quoted string - extracts out pure string only
dqStringValP :: Parser String
dqStringValP = between (char '\"') innerDq (wsP (char '\"'))

-- | Double quoted string - parses tokens that aren't allowed as well
dqStringValErrP :: Parser [Token]
dqStringValErrP = between (char '\"') (many (errorStrParser <|> (word <* many space))) (char '\"')

-- | Single quoted string - extracts out pure string only
sqStringValP :: Parser String
sqStringValP = between (char '\'') innerSq (wsP (char '\''))

-- | backticks
backticksP :: Parser String
backticksP = between (char '`') innerBacktick (wsP (char '`'))

-- | Single quoted string - parses tokens that aren't allowed as well
sqStringValErrP :: Parser [Token]
sqStringValErrP = between (char '\'') (many (errorStrParser <|> wsP word)) (char '\'')

-- >>> parse backticksP "`waefjk~jkw!@#$`"
-- Right "waefjk~jkw!@#$"

-- >>> parse dqStringValErrP "\"~\""
-- Right ["<tilde>"]

-- >>> parse valueP 
-- Left "  "


stringValP :: Parser Value
stringValP = StringVal <$> (dqStringValP <|> sqStringValP)

-- wordP :: Parser Value
-- wordP = Word <$> wsP (many (satisfy (/= ' ')) <* string " ")

-- >>> parse conditionalP "if [[ hes -ef \"hello\" ]]\nthen\n  echo \"$y\"\nelse\n  echo \"$y\"\nfi\n"
-- Left "[ParseError] Please check line:   $y\"   "


-- | parses different values
valueP :: Parser Value
valueP = intValP <|> boolValP <|> stringValP

-- | Parses a expressions
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

wordP :: Parser Value
wordP = Word <$> wsP word

-- >>> parse wordP "hi "
-- Right (Word "hi")

-- | Parses a conditional expression
ifExpP :: Parser IfExpression
ifExpP = bopexpP
  where
    bopexpP = uopexpP `chainl1` (flip IfOp2 <$> ifBopP)
    uopexpP =
      IfOp1 <$> ifUopP <*> uopexpP <|> baseP
    baseP = IfVar <$> variableRef <|> IfVal <$> valueP

-- >>> parse ifExpP "$y > hi"
-- Right (IfOp2 (IfVar (V "y")) GtIf (IfVal (Word "hi")))

-- | Parses array assignments in the form (...)
arrAssignP :: Parser Expression
arrAssignP = Arr <$> wsP (between (string "(") (many (satisfy (/= ')'))) (string ")"))

-- >>> parse arrAssignP "(hi hello)"
-- Left "[ParseError] No more characters to parse."

-- | Parses a line of input for an assignment
assignP :: Parser BashCommand
assignP = (Assign . V <$> name) <*> (char '=' *> (arrAssignP <|> expP))

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

variableRef :: Parser Var
variableRef = V <$> (char '$' *> wsP word)

argUnquotedVar :: Parser Arg
argUnquotedVar = Arg <$> (char '$' *> wsP word)

arithmeticInner :: Parser String
arithmeticInner = many (satisfy (/= '$'))

innerArithmetic :: Parser String
innerArithmetic = many (satisfy (/= ')'))

arithmeticExpansion :: Parser String
arithmeticExpansion = stringP "$" *> between (stringP "(") (between (stringP "(") innerArithmetic (stringP ")")) (stringP ")")

oldArithmeticExpansion :: Parser String
oldArithmeticExpansion = stringP "$" *> between (stringP "[") innerArithmetic (stringP "]")

regex :: Parser String
regex =
   (some (satisfy (`notElem` regOps)) *> (string "$" <|> string "+" <|> string "?") <* many get)
   <|> (many (satisfy (/= '^')) *> string "^" <* some get)
   <|> (some (satisfy (/= '|')) *> string "|" <* some get)
   <|> (many (satisfy (/= '(')) *> char '(' *> some (satisfy (/= ')')) <* char ')' <* many get)
   <|> (many (satisfy (/= '[')) *> char '[' *> some (satisfy (/= ']')) <* char ']' <* many get)
   <|> (many (satisfy (/= '*')) *> string "*" <* some get)
   <|> (some (satisfy (/= '*')) *> string "*" <* many get)
   <|> (many (satisfy (/= '/')) *> string "/" <* some get)

-- >>> parse regex "asdf(asdfad)asdf"
-- Right "asdfad"


-- >>> parse possibleAssignP "a =1"
-- Right (PossibleAssign (PossibleAssignWS (V "a") " " "=" "" (Val (IntVal 1))))

-- >>> parse possibleAssignP "$a=7"
-- Right (PossibleAssign (PossibleAssignDS (V "a") "=" (Val (IntVal 7))))

-- Right (PossibleAssign (V "a") (Val (IntVal 7)))
-- >>> parse possibleAssignP "a =3"
-- Right (PossibleAssign (PossibleAssignWS (V "a") " " "=" "" (Val (IntVal 3))))

-- | parses anything thats not an operator, quote, or space
notQuoteOrSpaceP :: Parser Char
notQuoteOrSpaceP = satisfy (\c -> c /= '"' && c /= '\'' && not (isSpace c))

notQuoteOrSpaceOrNewLineP = satisfy (\c -> not (isSpace c) && c /= '\n')

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

-- >>> parse conditionalP "if [[ 'hi' -ef \"hello\" ]]\nthen\n  echo \"$y\"\nelse\n  echo \"hi\"\nfi\n"
-- Left "[ParseError] Please check line:   hi\"   "


ifNonExp :: Parser IfExpression
ifNonExp = IfVar <$> variableRef <|> IfVal <$> valueP


-- | Parses the entire conditional block "if [...] then [...] else [...] fi"
conditionalP :: Parser BashCommand
conditionalP =
  -- "if [y=1] \nthen\n  x=2\nelse\n  x=3\nfi\n"
  Conditional
    <$> ((wsP (string "if [") *> ifExpP <* wsP (string "]"))
      <|> (wsP (string "if [[") *> wsP ifExpP <* wsP (string "]]"))
         <|> (wsP (string "if ((") *> (IfOp3 <$> ifNonExp <*> ifBopP <*> ifNonExp) <* wsP (string "))")))
    <*> (wsP (string "then") *> wsP blockP)
    <*> (wsP (string "else") *> wsP blockP <* wsP (string "fi"))

-- >>> parse ((wsP (string "if [") *> wsP ifExpP <* wsP (string "]")) <|> (wsP (string "if [[") *> wsP ifExpP <* wsP (string "]]")) <|> (wsP (string "if ((") *> (IfOp3 <$> ifNonExp <*> ifBopP <*> ifNonExp) <* wsP (string "))"))) "if [ $y > hi ]\n"
-- Right (IfOp2 (IfVar (V "y")) GtIf (IfVal (Word "hi")))


-- parse error (possibly incorrect indentation or mismatched brackets)
-- >>> parse conditionalP "if [ $y > hi ]\nthen\n  echo $x\nelse\n  echo \"hello\"\nfi\n"
-- Left "[ParseError] Please check line:   hello\"   ."


-- >>> parse ((wsP (string "if [") *> wsP ifExpP <* wsP (string "]")) "if [ hi > 1 ]"
-- parse error (possibly incorrect indentation or mismatched brackets)

-- >>> parse (wsP (string "if [") *> wsP blockP <* string "]") "if [y=1]"

bashCommandP :: Parser BashCommand
bashCommandP = assignP <|> conditionalP <|> possibleAssignP <|> execCommandP

-- >>> parse bashCommandP "echo \"~\""
-- Right (ExecCommand (ExecName "echo") [DoubleQuote ["<tilde>"]])

-- >>> parse arithmeticExpansion "$((3 + 4))"
-- Right "3 + 4"

-- >>> parse bashCommandP "x=1\nif [[ $z -eq \"hii\" ]]\nthen\n  echo \"$y\"\nelse\n  echo \"hi\"\nfi\n"
-- Left "if [[ $z -eq \"hii\" ]]\nthen\n  echo \"$y\"\nelse\n  echo \"hi\"\nfi\n"

-- >>> parse bashCommandP "x = 3"
-- Right (PossibleAssign (V "x") (Val (IntVal 3)))

-- >>> parse bashCommandP "echo '$hi'"
-- Right (ExecCommand (ExecName "echo") [SingleQuote ["$hi"]])

-- >>> parse bashCommandP "ls -l -a awefew wefjkl"
-- Left " -a awefew wefjkl"

blockP :: Parser Block
blockP = Block <$> many (wsP bashCommandP)

{- Script parser -}
parseShellScript :: String -> IO (Either String Block)
parseShellScript = parseFromFile (const <$> blockP <*> eof)

word2 :: Parser String
word2 = (:) <$> satisfy (/= '\n') <*> many (satisfy (/= '\n'))

-- newlineP :: Parser String
-- newlineP =

-- >>> parse bashCommandP "echo \"hi\""
-- Right (ExecCommand (ExecName "echo") [DoubleQuote ["hi"]])

-- >>> parse newlineP "y=1\nif [$y -lt 1] \nthen\n  x=2\nelse\n  x=3\nfi\n"
-- Variable not in scope: newlineP :: Parser a

-- How it looks : "if [y < 0] \nthen\n  x=2\nelse\n  x=3\nfi\n"

-- >>> parse expP "1"
-- Right (Val (IntVal 1))

-- >>> parse untilNewline "\nif [[ $x -ew \\\"hello\\\" ]]\n"
-- Right ""

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
