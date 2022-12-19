module ShellParsing where

import Control.Applicative
  ( Alternative (many, some, (<|>)),
    Applicative ((*>), (<*), (<*>)),
    (<$>),
  )
import Control.Monad (guard)
import Data.Char
  ( Char,
    isAlpha,
    isDigit,
    isLower,
    isSpace,
    isUpper,
  )
import Data.Foldable (Foldable (elem, foldr), notElem)
import Data.Map ()
import Parsing
  ( Parser,
    alpha,
    between,
    chainl1,
    char,
    choice,
    constP,
    digit,
    eof,
    errP,
    filter,
    get,
    int,
    lower,
    parens,
    parse,
    parseFromFile,
    satisfy,
    space,
    string,
    stringP,
    upper,
    wsP,
  )
import ShellSyntax
  ( Arg (..),
    ArgToken (ArgM, ArgS),
    BashCommand (Assign, Conditional, ExecCommand, PossibleAssign),
    Block (..),
    Bop (..),
    Command (..),
    Expression (..),
    IfBop (..),
    IfExpression (..),
    IfUop
      ( AndU,
        BlockSpecial,
        CharSpecial,
        ExecPermission,
        FileExists,
        FileOrFolderExists,
        FileSize,
        FolderExists,
        GroupId,
        GroupIdUser,
        LengthNonZero,
        LengthZero,
        Modified,
        NotIf,
        Or,
        Owner,
        Pipe,
        ReadPermission,
        Socket,
        StickyBit,
        Symlink,
        Terminal,
        UserId,
        WritePermission
      ),
    Misc (Esc, Tilde),
    PossibleAssign (..),
    PrintfToken (..),
    TokenS,
    Uop (..),
    Value (..),
    Var (..),
    regOps,
  )
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

-- | Parser for all tokens ixn printf except format specificators
printfTokenP :: Parser PrintfToken
printfTokenP = Token . ArgS <$> ((:) <$> satisfy (/= '%') <*> many (satisfy (/= '%')))

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
numFormatSpecInTokens :: [ArgToken] -> Int
numFormatSpecInTokens (x : xs) =
  case x of
    ArgS s ->
      case parse printfP s of
        Left _ -> numFormatSpecInTokens xs
        Right printfTokens -> helper printfTokens + numFormatSpecInTokens xs
          where
            helper = foldr (\x acc -> if isFormat x then acc + 1 else acc) 0
    ArgM _ -> numFormatSpecInTokens xs
numFormatSpecInTokens [] = 0

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
      constP "%" ModuloIf
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
uopP = constP "not" Not

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
sqStringValErrP :: Parser [ArgToken]
sqStringValErrP = between (char '\'') (many ((ArgM <$> errorStrParser) <|> (ArgS <$> (word <* many space)))) (char '\'')

stringValP :: Parser Value
stringValP = StringVal <$> (dqStringValP <|> sqStringValP)

valueP = intValP <|> boolValP <|> stringValP

-- | Parses non-cnditional expressions
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
    baseP = Var <$> varP <|> Val <$> valueP

-- wordP :: Parser Value
-- wordP = Word <$> wsP word

-- | Parses conditional expressions
ifExpP :: Parser IfExpression
ifExpP = bopexpP
  where
    bopexpP = uopexpP `chainl1` (flip IfOp2 <$> ifBopP)
    uopexpP = baseP <|> IfOp1 <$> ifUopP <*> uopexpP
    baseP = IfVar <$> varP <|> parens ifExpP <|> IfVal <$> valueP

-- >>> parse ifExpP "-o"
-- Left "\t<PARSING ERROR> No more characters to parse."

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

varP :: Parser Var
varP = V <$> (char '$' *> wsP word)

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
  (some (satisfy (`notElem` regOps)) *> (string "$" <|> string "+" <|> string "?") <* many get)
    <|> (many (satisfy (/= '^')) *> string "^" <* some get)
    <|> (some (satisfy (/= '|')) *> string "|" <* some get)
    <|> (many (satisfy (/= '(')) *> char '(' *> some (satisfy (/= ')')) <* char ')' <* many get)
    <|> (many (satisfy (/= '[')) *> char '[' *> some (satisfy (/= ']')) <* char ']' <* many get)
    <|> (many (satisfy (/= '*')) *> string "*" <* some get)
    <|> (some (satisfy (/= '*')) *> string "*" <* many get)
    <|> (many (satisfy (/= '/')) *> string "/" <* some get)

-- | Parses anything that's not an operator, quote, or space
notQuoteOrSpaceP :: Parser Char
notQuoteOrSpaceP = satisfy (\c -> c /= '"' && c /= '\'' && not (isSpace c) && c /= ')')

notQuoteOrSpaceOrNewLineP = satisfy (\c -> not (isSpace c) && c /= '\n')

-- parses a name from a string
word :: Parser String
word = (:) <$> notQuoteOrSpaceP <*> many notQuoteOrSpaceP

reserved :: [String]
reserved = ["!", "fi", "then", "elif", "else", "if", "<tilde>", "escape>"]

operators :: [String]
operators = ["&&", "||", ";;", "<<", ">>", "<&", ">&", "<>", "<<-", ">|", "+", "-", "*", "/", "%", "=", "==", "!=", "<", ">", "<=", ">=", "!", "(", ")", "{", "}", "[", "]", ";", "&", "|", ">", "<", ">>", "<<", "<<<", ">>>"]

-- | Parses command name
commandP :: Parser Command
commandP = ExecName <$> wsP (filter isNotSpecial name)
  where
    isNotSpecial = not . (`elem` reserved ++ operators)

-- | parses single word as an arg
singleArgP :: Parser Arg
singleArgP = Arg <$> word

-- | parses quoted string as an arg
quotedArgP :: Parser Arg
quotedArgP = (SingleQuote <$> sqStringValErrP) <|> (DoubleQuote <$> dqStringValErrP)

-- | parses args
argP :: Parser Arg
argP = wsP (singleArgP <|> quotedArgP)

execCommandP :: Parser BashCommand
execCommandP = ExecCommand <$> commandP <*> many argP <* many (char '\n')

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
-- Right (ExecCommand (ExecName "echo") [DoubleQuote [ArgM Tilde]])

-- >>> parse arithmeticExpansion "$((3 + 4))"
-- Right "3 + 4"

-- >>> parse bashCommandP "x=1\nif [[ $z -eq \"hii\" ]]\nthen\n  echo \"$y\"\nelse\n  echo \"hi\"\nfi\n"
-- Left "if [[ $z -eq \"hii\" ]]\nthen\n  echo \"$y\"\nelse\n  echo \"hi\"\nfi\n"

-- >>> parse bashCommandP "echo \"hi\" \"hi\""
-- Left " \"hi\" "

-- >>> parse bashCommandP "x=(hi, hi)"
-- Right (Assign (V "x") (Arr "hi, hi"))

-- >>> parse bashCommandP "ls -l -a awefew wefjkl"
-- Left " -a awefew wefjkl"

blockP :: Parser Block
blockP = Block <$> many (wsP bashCommandP)

-- | Parses inputted file entirely
parseShellScript :: String -> IO (Either String Block)
parseShellScript = parseFromFile (const <$> blockP <*> eof)
