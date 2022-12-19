module ShellParsing where

import Control.Applicative
    ( Applicative((<*), (*>), (<*>)),
      (<$>),
      Alternative(many, (<|>), some) )
import Control.Monad (guard)
import Data.Char
  ( Char,
    isAlpha,
    isDigit,
    isLower,
    isSpace,
    isUpper,
  )
import Data.Foldable ( Foldable(elem, foldr), notElem )
import Data.Map ()
import Parsing
    ( parse,
      Parser,
      filter,
      get,
      satisfy,
      alpha,
      digit,
      upper,
      lower,
      space,
      char,
      string,
      int,
      chainl1,
      choice,
      between,
      wsP,
      stringP,
      constP,
      errP,
      eof,
      parseFromFile )
import ShellSyntax
    (
      Block(..),
      Bop(..),
      PossibleAssign(..),
      Command(..),
      Expression(..),
      IfUop(Or, NotIf, AndU, BlockSpecial, CharSpecial, FolderExists,
            FileOrFolderExists, FileExists, GroupId, Symlink, StickyBit, Pipe,
            ReadPermission, FileSize, Socket, Terminal, UserId,
            WritePermission, ExecPermission, Owner, GroupIdUser, Modified,
            LengthZero, LengthNonZero),
      IfBop(..),
      Value(..),
      IfExpression(..),
      BashCommand(Conditional, Assign, PossibleAssign, ExecCommand),
      Var(..),
      Arg(..),
      Uop(..),
      PrintfToken(..),
      regOps, ArgToken (ArgS, ArgM), Misc (Tilde, Esc), TokenS )
import Test.HUnit (Assertion, Counts, Test (..), assert, runTestTT, (~:), (~?=))
import Prelude hiding (filter)

-- | Parses the first character of a name
startOfName :: Parser Char
startOfName = char '_' <|> alpha <|> lower <|> upper

-- | Parses valid character from the rest of a name
restOfName :: Parser Char
restOfName = startOfName <|> digit

-- | Parses a name from a string
name :: Parser String
name = (:) <$> startOfName <*> many restOfName

-- | Parses a variable name
nonPrintFArg :: Parser String
nonPrintFArg = many (satisfy (/= '%'))

-- | Parser for format specificators in printf
formatSpecP :: Parser PrintfToken
formatSpecP =
  constP "%s" FormatS
    <|> constP "%d" FormatD
    <|> constP "%f" FormatF
    <|> constP "%b" FormatB
    <|> constP "%x" FormatX
    <|> constP "%o" FormatO
    <|> constP "%e" FormatE
    <|> constP "%g" FormatG
    <|> constP "%a" FormatA

<<<<<<< Updated upstream
-- | Parser for all tokens in printf except format specificators
printfTokenP :: Parser PrintfToken
printfTokenP = Token <$> ((:) <$> satisfy (/= '%') <*> many (satisfy (/= '%')))
=======
-- | Parser for all tokens ixn printf except format specificators
printfToken :: Parser PrintfToken
printfToken = Token . ArgS <$> ((:) <$> satisfy (/= '%') <*> many (satisfy (/= '%')))
>>>>>>> Stashed changes

-- | Parser for printf
printfP :: Parser [PrintfToken]
printfP = many (formatSpecP <|> printfTokenP)

isFormat :: PrintfToken -> Bool
isFormat (Token _) = False
isFormat _ = True

-- | Counts the number of format specificators in a printf
typeCounter :: [PrintfToken] -> Int
typeCounter = foldr (\x acc -> if isFormat x then acc + 1 else acc) 0

-- | Counts the number of format specificators of the tokens inside a format of a printf
<<<<<<< Updated upstream
numFormatSpecInTokens :: [Token] -> Int
numFormatSpecInTokens (x : xs) = case parse printfP x of
  Left _ -> numFormatSpecInTokens xs
  Right printfTokens -> helper printfTokens + numFormatSpecInTokens xs
    where
      helper = foldr (\x acc -> if isFormat x then acc + 1 else acc) 0
numFormatSpecInTokens [] = 0
=======
tokenPars :: [ArgToken] -> Int
tokenPars (x : xs) = 
  case x of 
    ArgS s ->
      case parse printfParser s of
        Left _ -> tokenPars xs
        Right printfTokens -> helper printfTokens + tokenPars xs
          where
            helper = foldr (\x acc -> case x of FormatSpec -> acc + 1; _ -> acc) 0
    ArgM _ -> tokenPars xs
tokenPars [] = 0
>>>>>>> Stashed changes

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
    [ constP "-nt" Nt,
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
      constP "-" MinusIf,
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

<<<<<<< Updated upstream
uopP :: Parser Uop
uopP =
  constP "-" Neg
    <|> constP "not" Not

=======
>>>>>>> Stashed changes
-- | Parses unary operators
ifUopP :: Parser IfUop
ifUopP =
  choice
    [ constP "!" NotIf,
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
      constP "-x" ExecPermission,
      constP "-O" Owner,
      constP "-G" GroupIdUser,
      constP "-N" Modified,
      constP "-z" LengthZero,
      constP "-n" LengthNonZero,
      constP "-o" Or
    ]

intValP = IntVal <$> wsP int

boolValP :: Parser Value
boolValP = BoolVal <$> wsP (constP "true" True <|> constP "false" False)

{- Parsers for quoted strings -}

-- | Parses tokens that can't be used within strings
errorStrParser :: Parser Misc
errorStrParser =
  constP "\\'" Esc <|> constP "~" Tilde

-- Since ' can be used in double quoted string and vice versa, inner has to be defined separately
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
dqStringValErrP :: Parser [ArgToken]
dqStringValErrP = between (char '\"') (many ((ArgM <$> errorStrParser) <|> (ArgS <$> (word <* many space)))) (char '\"')

-- | Single quoted string - extracts out pure string only
sqStringValP :: Parser String
sqStringValP = between (char '\'') innerSq (wsP (char '\''))

-- | String surrounded by backticks
backticksP :: Parser String
backticksP = between (char '`') innerBacktick (wsP (char '`'))

-- | Single quoted string - parses tokens that aren't allowed as well
<<<<<<< Updated upstream
sqStringValErrP :: Parser [Token]
sqStringValErrP = between (char '\'') (many (errorStrParser <|> wsP word)) (char '\'')

-- >>> parse backticksP "`waefjk~jkw!@#$`"
-- Right "waefjk~jkw!@#$"

-- >>> parse dqStringValErrP "\"efjkelw~\""
-- Right ["efjkelw~"]

-- >>> parse valueP
-- No instance for (Show (String -> Either String Value))
--   arising from a use of ‘evalPrint’
--   (maybe you haven't applied a function to enough arguments?)
=======
sqStringValErrP :: Parser [ArgToken]
sqStringValErrP = between (char '\'') (many ((ArgM <$> errorStrParser) <|> (ArgS <$> (word <* many space)))) (char '\'')
>>>>>>> Stashed changes

stringValP :: Parser Value
stringValP = StringVal <$> (dqStringValP <|> sqStringValP)

<<<<<<< Updated upstream
-- wordP :: Parser Value
-- wordP = Word <$> wsP (many (satisfy (/= ' ')) <* string " ")

-- >>> parse conditionalP "if [[ hes -ef \"hello\" ]]\nthen\n  echo \"$y\"\nelse\n  echo \"$y\"\nfi\n"
-- Left "[ParseError] Please check line:   $y\"   "

-- | parses different values
=======
-- | Parses different types of values
>>>>>>> Stashed changes
valueP :: Parser Value
valueP = intValP <|> boolValP <|> stringValP

-- | Parses non-cnditional expressions
expP :: Parser Expression
expP = compP
  where
    compP = catP `chainl1` opAtLevel (level Gt)
    catP = sumP `chainl1` opAtLevel (level Concat)
    sumP = prodP `chainl1` opAtLevel (level Plus)
<<<<<<< Updated upstream
    prodP = uopexpP `chainl1` opAtLevel (level Times)
    uopexpP =
      baseP
        <|> Op1 <$> uopP <*> uopexpP
    baseP = Var <$> varP <|> Val <$> valueP
=======
    prodP = baseP `chainl1` opAtLevel (level Times)
    baseP = Var <$> variableRef <|> Val <$> valueP
>>>>>>> Stashed changes

-- wordP :: Parser Value
-- wordP = Word <$> wsP word

-- | Parses onditional expressions
ifExpP :: Parser IfExpression
ifExpP = bopexpP
  where
    bopexpP = uopexpP `chainl1` (flip IfOp2 <$> ifBopP)
    uopexpP =
      IfOp1 <$> ifUopP <*> uopexpP <|> baseP
    baseP = IfVar <$> varP <|> IfVal <$> valueP

-- >>> parse ifExpP "$y > hi"
-- Left "> hi"
<<<<<<< Updated upstream
=======

>>>>>>> Stashed changes

-- | Parses array assignments in the form (...)
arrAssignP :: Parser Expression
arrAssignP = Arr <$> wsP (between (string "(") (many (satisfy (/= ')'))) (string ")"))

-- | Parses a line of input for an assignment
assignP :: Parser BashCommand
assignP = (Assign . V <$> name) <*> (char '=' *> (arrAssignP <|> expP))

-- | Parses potential assignments with syntax issues
possibleAssignP :: Parser BashCommand
possibleAssignP = PossibleAssign <$> (wsAssignP <|> dsAssignP)

-- | Parses var with whitespaces (retains them, too)
wsAssignP :: Parser PossibleAssign
wsAssignP = PossibleAssignWS <$> (V <$> name) <*> many (char ' ') <*> string "=" <*> many (char ' ') <*> wsP expP

-- | Parses assignments with $ in front of var name
dsAssignP :: Parser PossibleAssign
dsAssignP = PossibleAssignDS <$> (stringP "$" *> (V <$> name)) <*> many (char ' ' <|> char '=') <*> wsP expP

<<<<<<< Updated upstream
varP :: Parser Var
varP = V <$> (char '$' *> wsP word)
=======
-- | Parses variable usages
variableRef :: Parser Var
variableRef = V <$> (char '$' *> wsP word)
>>>>>>> Stashed changes

-- | Parses variable usages as arguments
argUnquotedVar :: Parser Arg
argUnquotedVar = Arg <$> (char '$' *> wsP word)

-- | Parses repeated pattern of non-space characters separated by comma
commaInArr :: Parser String
commaInArr = many (satisfy (/= ',')) <* stringP "," <* many (char ' ')

-- | Parses out array elements separated by commas
entireCommaInArr :: Parser [String]
entireCommaInArr = some commaInArr <* many get

{- Arithmetic parsers -}

arithmeticInner :: Parser String
arithmeticInner = many (satisfy (/= '$'))

innerArithmetic :: Parser String
innerArithmetic = many (satisfy (/= ')'))

innerArithmeticOld = many (satisfy (/= ']'))

arithmeticExpansion :: Parser String
arithmeticExpansion = stringP "$" *> between (stringP "(") (between (stringP "(") innerArithmetic (stringP ")")) (stringP ")")

oldArithmeticExpansion :: Parser String
oldArithmeticExpansion = stringP "$" *> between (stringP "[") innerArithmeticOld (stringP "]")

-- >>> parse oldArithmeticExpansion "$[hi]"


-- | Parses possible regex expressions based on common operators
regex :: Parser String
regex =
<<<<<<< Updated upstream
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
=======
   (some (satisfy (`notElem` regOps)) *> (string "$" <|> string "+" <|> string "?") <* many get)
   <|> (many (satisfy (/= '^')) *> string "^" <* some get)
   <|> (some (satisfy (/= '|')) *> string "|" <* some get)
   <|> (many (satisfy (/= '(')) *> char '(' *> some (satisfy (/= ')')) <* char ')' <* many get)
   <|> (many (satisfy (/= '[')) *> char '[' *> some (satisfy (/= ']')) <* char ']' <* many get)
   <|> (many (satisfy (/= '*')) *> string "*" <* some get)
   <|> (some (satisfy (/= '*')) *> string "*" <* many get)
   <|> (many (satisfy (/= '/')) *> string "/" <* some get)
  
-- | Parses anything that's not an operator, quote, or space
>>>>>>> Stashed changes
notQuoteOrSpaceP :: Parser Char
notQuoteOrSpaceP = satisfy (\c -> c /= '"' && c /= '\'' && not (isSpace c))

notQuoteOrSpaceOrNewLineP = satisfy (\c -> not (isSpace c) && c /= '\n')

-- parses a name from a string
word :: Parser String
word = (:) <$> notQuoteOrSpaceP <*> many notQuoteOrSpaceP

<<<<<<< Updated upstream
-- parses command name
=======
reserved :: [String]
reserved = ["!", "fi", "then", "elif", "else", "if", "<tilde>", "escape>"]

operators :: [String]
operators = ["&&", "||", ";;", "<<", ">>", "<&", ">&", "<>", "<<-", ">|", "+", "-", "*", "/", "%", "=", "==", "!=", "<", ">", "<=", ">=", "!", "(", ")", "{", "}", "[", "]", ";", "&", "|", ">", "<", ">>", "<<", "<<<", ">>>"]

-- | Parses command name
>>>>>>> Stashed changes
commandP :: Parser Command
commandP = ExecName <$> wsP (filter isNotSpecial name)
  where
    isNotSpecial = not . (`elem` reserved ++ operators)

<<<<<<< Updated upstream
-- | parses single word as an arg
singleArgP :: Parser Arg
singleArgP = Arg <$> word

-- | parses quoted string as an arg
quotedArgP :: Parser Arg
quotedArgP = (SingleQuote <$> sqStringValErrP) <|> (DoubleQuote <$> dqStringValErrP)

-- | parses args
argP :: Parser Arg
argP = singleArgP <|> quotedArgP
=======
-- | Parses single word as an arg
argP :: Parser Arg
argP = Arg <$> word

-- | Parses quoted string as an arg
argsP :: Parser Arg
argsP = (SingleQuote <$> sqStringValErrP) <|> (DoubleQuote <$> dqStringValErrP)
>>>>>>> Stashed changes

execCommandP :: Parser BashCommand
execCommandP = ExecCommand <$> commandP <*> many argP <* many (char '\n')

<<<<<<< Updated upstream
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
ifNonExp = IfVar <$> varP <|> IfVal <$> valueP
=======
-- conditionalStrP :: Parser String
-- conditionalStrP = choice [wsP $ string "", wsP (string "if ["), wsP $ many get, wsP (string "fi")]

-- | Parses if expression in arithmetic context
ifNonExp :: Parser IfExpression
ifNonExp = IfVar <$> variableRef <|> IfVal <$> valueP
>>>>>>> Stashed changes

-- | Parses the entire conditional block "if [...] then [...] else [...] fi"
conditionalP :: Parser BashCommand
conditionalP =
  Conditional
    <$> ( (wsP (string "if [") *> ifExpP <* wsP (string "]"))
            <|> (wsP (string "if [[") *> wsP ifExpP <* wsP (string "]]"))
            <|> (wsP (string "if ((") *> (IfOp3 <$> ifNonExp <*> ifBopP <*> ifNonExp) <* wsP (string "))"))
        )
    <*> (wsP (string "then") *> wsP blockP)
    <*> (wsP (string "else") *> wsP blockP <* wsP (string "fi"))

<<<<<<< Updated upstream
-- >>> parse ((wsP (string "if [") *> wsP ifExpP <* wsP (string "]")) <|> (wsP (string "if [[") *> wsP ifExpP <* wsP (string "]]")) <|> (wsP (string "if ((") *> (IfOp3 <$> ifNonExp <*> ifBopP <*> ifNonExp) <* wsP (string "))"))) "if [ $y > hi ]\n"
-- Right (IfOp2 (IfVar (V "y")) GtIf (IfVal (Word "hi")))

-- parse error (possibly incorrect indentation or mismatched brackets)
-- >>> parse conditionalP "if [ $y > hi ]\nthen\n  echo $x\nelse\n  echo \"hello\"\nfi\n"
-- Left "[ParseError] Please check line:   hello\"   ."

-- >>> parse bashCommandP "x=$xe"
-- Right (Assign (V "x") (Var (V "xe")))

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

-- >>> parse bashCommandP "x=(hi, hi)"
-- Right (Assign (V "x") (Arr "hi, hi"))

-- >>> parse bashCommandP "ls -l -a awefew wefjkl"
-- Left " -a awefew wefjkl"

=======
bashCommandP :: Parser BashCommand
bashCommandP = assignP <|> conditionalP <|> possibleAssignP <|> execCommandP

>>>>>>> Stashed changes
blockP :: Parser Block
blockP = Block <$> many (wsP bashCommandP)


-- | Parses inputted file entirely 
parseShellScript :: String -> IO (Either String Block)
parseShellScript = parseFromFile (const <$> blockP <*> eof)

<<<<<<< Updated upstream
word2 :: Parser String
word2 = (:) <$> satisfy (/= '\n') <*> many (satisfy (/= '\n'))

commaInArr :: Parser String
commaInArr = many (satisfy (/= ',')) <* stringP "," <* many (char ' ')

entireCommaInArr :: Parser [String]
entireCommaInArr = some commaInArr <* many get

-- >>> parse (some commaInArrParsing <* (many get)) "hello, hi hi"
-- Right ["hello"]

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
=======
>>>>>>> Stashed changes
