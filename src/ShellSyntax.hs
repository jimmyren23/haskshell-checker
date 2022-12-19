module ShellSyntax where

import Control.Monad (liftM2, liftM3, liftM4, liftM5)
import Data.Char
import Data.Char qualified as Char
import Data.Map (Map)
import Data.Map qualified as Map
import Test.QuickCheck as QC
import Text.PrettyPrint (Doc, (<+>))

-- | Overall representation of different types of commands supported
data BashCommand
  = ExecCommand Command [Arg] -- "basic" command in the form of a keyword followed by tokenized list of arguments
  | Conditional IfExpression Block Block -- conditional statements in the form of "if ... then .. else .. fi"
  | Assign Var Expression -- variable assignments in the form of "{var name}={value}"
  | PossibleAssign PossibleAssign -- potential assignment commands with syntax errors
  deriving (Eq, Show)

genArgs :: Gen [Arg]
genArgs = QC.sized gen
  where
    gen 0 = return []
    gen n = do
      arg <- arbitrary
      args <- gen (n `div` 2)
      return (arg : args)

instance Arbitrary BashCommand where
  arbitrary :: Gen BashCommand
  arbitrary = oneof [Assign <$> arbitrary <*> arbitrary, PossibleAssign <$> arbitrary, ExecCommand <$> (ExecName <$> genName) <*> genArgs, Conditional <$> arbitrary <*> arbitrary <*> arbitrary]

-- >>> QC.sample' (arbitrary :: Gen BashCommand)
-- [ExecCommand (ExecName "T") [],ExecCommand (ExecName "hA") [Arg "*",Arg "|"],ExecCommand (ExecName "h8P") [Arg "c",Arg "M",Arg "8"],PossibleAssign (PossibleAssignDS (V "TpAWsM") "=" (Val (IntVal 6))),ExecCommand (ExecName "kUH0J") [Arg ",",Arg ";",Arg "7",Arg "."],ExecCommand (ExecName "soGOHi") [Arg "h",Arg "+",Arg "n",Arg "|"],PossibleAssign (PossibleAssignWS (V "JL3ZEPyljc") "     " "=" "     " (Val (BoolVal True))),PossibleAssign (PossibleAssignDS (V "Xd1uz") "=" (Val (StringVal ":Or,=.J8l}7T"))),ExecCommand (ExecName "A8mmy") [Arg "N",Arg "t",Arg "7",Arg "rD8]",Arg "w"],Assign (V "y9bJUEEmLfv8iByuXBw") (Val (StringVal "C;\\rJ")),PossibleAssign (PossibleAssignWS (V "_xonmqSsk03mw_g") "  " "=" "      " (Val (StringVal "Rmi*")))]

{- ExecCommand syntax -}

newtype Command = ExecName String
  deriving (Eq, Show)

reserved :: [String]
reserved = ["!", "fi", "then", "elif", "else", "if"]

operators :: [String]
operators = ["&&", "||", ";;", "<<", ">>", "<&", ">&", "<>", "<<-", ">|", "+", "-", "*", "/", "%", "=", "==", "!=", "<", ">", "<=", ">=", "!", "(", ")", "{", "}", "[", "]", ";", "&", "|", ">", "<", ">>", "<<", "<<<", ">>>"]

isStartOfName :: Char -> Bool
isStartOfName c = isAlpha c || c == '_' && c /= '\1'

isEndOfName :: Char -> Bool
isEndOfName c = isStartOfName c || isDigit c

isName :: String -> Bool
isName (x : xs) =
  isStartOfName x
    && all isEndOfName xs
    && (x : xs) `notElem` reserved
    && (x : xs) `notElem` operators
isName [] = False

genName :: Gen String
genName = elements stringCharsStart >>= \c -> QC.listOf (elements stringCharsEnd) >>= \cs -> return (c : cs)
  where
    stringCharsStart :: [Char]
    stringCharsStart = Prelude.filter (\c -> isAlpha c || c == '_' && Char.isPrint c) ['\NUL' .. '~']
    stringCharsEnd :: [Char]
    stringCharsEnd = Prelude.filter (\c -> isAlphaNum c || c == '_' && Char.isPrint c) ['\NUL' .. '~']

instance Arbitrary Command where
  arbitrary :: Gen Command
  arbitrary = ExecName <$> genName

-- | Representation of arguments
data Arg
  = SingleQuote [ArgToken] -- argument surrounded by single quotes
  | DoubleQuote [ArgToken] -- argument surrounded by double quotes
  | Arg String -- "basic" argument : consecutive characters excluding space and quotes
  deriving (Eq, Show)

-- | Generates a word string
genFirstArg :: Gen Arg
genFirstArg = elements argChars >>= \c -> QC.frequency [(50, return (Arg [c])), (1, QC.listOf (elements argChars) >>= \cs -> return (Arg (c : cs)))]
  where
    argChars = Prelude.filter (\c -> c /= '"' && c /= '\'' && c /= '~' && c /= ')' && c /= '=' && not (isSpace c) && Char.isPrint c) ['\NUL' .. '~']

instance Arbitrary Arg where
  arbitrary :: Gen Arg
  arbitrary = QC.frequency [(1, SingleQuote <$> QC.listOf genToken), (1, DoubleQuote <$> QC.listOf genToken), (100, genFirstArg)]

-- | Generates a word string
genWord :: Gen String
genWord = elements tokenChars >>= \c -> QC.frequency [(50, return [c]), (1, QC.listOf (elements tokenChars) >>= \cs -> return (c : cs))]
  where
    tokenChars = Prelude.filter (\c -> c /= '"' && c /= '\'' && c /= '~' && c /= ')' && not (isSpace c) && Char.isPrint c) ['\NUL' .. '~']

genToken :: Gen ArgToken
genToken = QC.frequency [(80, ArgS <$> genWord), (1, possibleTokens)]
  where
    possibleTokens :: Gen ArgToken = elements [ArgM Esc, ArgM Tilde]

type TokenS = String

data ArgToken
  = ArgS String
  | ArgM Misc
  deriving (Eq, Show)

data Misc
  = Tilde -- ~
  | Esc -- \'
  deriving (Eq, Show, Enum, Bounded)

{- Conditional syntax -}

-- | Representation of "if [...]" or "if [[...]]" or if ((...)) expression (see IfOp3)
-- | By default, assume conditionals to be of the form "if [...]" or "if [[...]]"
data IfExpression
  = IfVar Var -- global variables
  | IfVal Value -- literal values
  | IfOp1 IfUop IfExpression -- unary operators
  | IfOp2 IfExpression IfBop IfExpression -- binary operators
  | IfOp3 IfExpression IfBop IfExpression -- arithmetic expression enclosed by ((...))
  deriving (Eq, Show)

instance Arbitrary IfExpression where
  arbitrary :: Gen IfExpression
  arbitrary = QC.frequency [(10, IfVar <$> arbitrary), (10, IfVal <$> arbitrary), (10, liftM2 IfOp1 arbitrary arbitrary), (1, liftM3 IfOp2 arbitrary arbitrary arbitrary), (0, liftM3 IfOp3 arbitrary arbitrary arbitrary)]

-- >>> QC.sample' (arbitrary :: Gen IfExpression)
-- [IfOp2 (IfOp2 (IfOp3 (IfVal (IntVal 0)) GeNIf (IfVal (StringVal ""))) DivideIf (IfOp3 (IfOp2 (IfVal (BoolVal False)) Ne (IfOp1 Socket (IfOp1 GroupIdUser (IfVar (V "\\"))))) Nt (IfOp3 (IfVar (V "Q")) GeNIf (IfOp1 FolderExists (IfOp3 (IfOp3 (IfVal (StringVal "")) Ot (IfOp2 (IfVar (V "l")) Err (IfVal (StringVal "")))) EqNIf (IfOp2 (IfVal (BoolVal False)) Nt (IfOp2 (IfOp3 (IfVal (BoolVal True)) LeNIf (IfVal (IntVal 0))) Ne (IfVar (V "%"))))))))) Ef (IfVar (V "C")),IfOp2 (IfVar (V "|")) GeNIf (IfVar (V "M")),IfOp2 (IfVar (V "9")) ModuloIf (IfOp3 (IfVal (IntVal (-4))) LeNIf (IfOp1 FileSize (IfVar (V "{")))),IfOp3 (IfVal (IntVal (-5))) Nt (IfOp1 UserId (IfVal (BoolVal False))),IfVal (BoolVal False),IfVar (V ","),IfOp3 (IfOp1 ExecPermission (IfVal (BoolVal True))) LeIf (IfVar (V "*")),IfOp1 FileOrFolderExists (IfVal (StringVal "&?[")),IfVal (IntVal 15),IfOp2 (IfOp3 (IfVar (V "T")) AndIf (IfVal (IntVal 4))) ModuloIf (IfOp2 (IfOp1 Owner (IfOp2 (IfOp1 LengthZero (IfOp1 GroupIdUser (IfVal (BoolVal False)))) LeNIf (IfOp1 GroupIdUser (IfOp1 LengthNonZero (IfVal (IntVal 2)))))) GeIf (IfVal (BoolVal False))),IfOp1 WritePermission (IfOp2 (IfVar (V "z")) LtIf (IfOp1 ExecPermission (IfVar (V "w"))))]

-- | List of binary operators permitted for bash conditional expressions
-- | Note: This list may not be comprehensive of all permitted binary operators.
-- | Referenced : https://linuxhint.com/bash_operator_examples/#o73
data IfBop
  = PlusIf -- `+`  :: Int -> Int -> Int
  | MinusIf -- `-`  :: Int -> Int -> Int
  | TimesIf -- `*`  :: Int -> Int -> Int
  | DivideIf -- `//` :: Int -> Int -> Int   -- floor division
  | ModuloIf -- `%`  :: Int -> Int -> Int
  | Nt -- -nt file operator checking if a file is newer than the other
  | Ot -- -ot file operator checking if a file is older than the other
  | Ef -- -ef checking if the two hard links are pointing the same file or not
  | EqIf -- `==`
  | EqNIf -- -eq numerical operator
  | EqS -- = `=` string operaor
  | GtIf -- `>`  :: a -> a -> Bool
  | GtNIf -- `-gt` :: a -> a -> Bool
  | GeIf -- `>=` :: a -> a -> Bool
  | GeNIf -- `-ge` :: a -> a -> Bool
  | LtIf -- `<`  :: a -> a -> Bool
  | LtNIf -- `-lt` :: a -> a -> Bool
  | LeIf -- `<=` :: a -> a -> Bool
  | LeNIf -- `-le` :: a -> a -> Bool
  | Ne -- `!=` :: a -> a -> Bool
  | NeN -- `-ne` :: a -> a -> Bool
  | AndIf -- `&&` :: a -> a -> Bool
  | Reg -- `=~` :: a -> a -> Bool
  | Err -- Operators that are not allowed
  deriving (Eq, Show, Enum, Bounded)

instance Arbitrary IfBop where
  arbitrary = elements (Prelude.filter (/= Err) [minBound .. maxBound])

{- Assignment syntax -}

-- | Commands that could be intepreted as assignments
data PossibleAssign
  = PossibleAssignWS Var String Equal String Expression -- errors from extra spaces surrounding Equal("=") sign
  | PossibleAssignDS Var Equal Expression -- error from prefixing variable name with "$"
  deriving (Eq, Show)

genSpaces :: Gen String
genSpaces = QC.sized gen
  where
    gen :: Int -> Gen [Char]
    gen n = QC.frequency [(1, return " "), (n, liftM2 (:) (elements " ") (gen (n `div` 2)))]

genEqual :: Gen String
genEqual = QC.elements ["="]

instance Arbitrary PossibleAssign where
  arbitrary :: Gen PossibleAssign
  arbitrary = QC.frequency [(1, liftM5 PossibleAssignWS arbitrary genSpaces genEqual genSpaces arbitrary), (1, liftM3 PossibleAssignDS arbitrary genEqual arbitrary)]

-- >>> QC.sample' (arbitrary :: Gen PossibleAssign)
-- [PossibleAssignDS (V "]") "=" (Val (BoolVal False)),PossibleAssignDS (V "V") "=" (Var (V "c")),PossibleAssignDS (V "p") "=" (Var (V "]")),PossibleAssignWS (V ";") "" "=" "  " (Var (V ",")),PossibleAssignDS (V ")") "=" (Var (V "D")),PossibleAssignWS (V "[") " " "=" "  " (Val (IntVal 1)),PossibleAssignWS (V "Y") "  " "=" "  " (Var (V "i")),PossibleAssignWS (V ">") "  " "=" "" (Val (StringVal "zrW-$")),PossibleAssignDS (V "Z") "=" (Var (V "o")),PossibleAssignWS (V "8") "   " "=" "  " (Val (IntVal 18)),PossibleAssignDS (V "k") "=" (Val (StringVal "5rm;M"))]

-- | Representation of all expressions except conditional (see "IfExpression")
data Expression
  = Var Var -- global variables
  | Val Value -- literal values
  | Op1 Uop Expression -- unary operators
  | Op2 Expression Bop Expression -- binary operators
  | Arr String
  deriving (Eq, Show)

instance Arbitrary Expression where
  arbitrary :: Gen Expression
  arbitrary = QC.frequency [(30, Var <$> arbitrary), (30, Val <$> arbitrary), (1, liftM2 Op1 arbitrary arbitrary), (1, liftM3 Op2 arbitrary arbitrary arbitrary)]

type Equal = String -- "=" for variable assignments

-- | List of binary operators for expressions
data Bop
  = Plus -- `+`  :: Int -> Int -> Int
  | Minus -- `-`  :: Int -> Int -> Int
  | Times -- `*`  :: Int -> Int -> Int
  | Divide -- `//` :: Int -> Int -> Int   -- floor division
  | Modulo -- `%`  :: Int -> Int -> Int   -- modulo
  | Eq -- `==` :: a -> a -> Bool
  | Gt -- `>`  :: a -> a -> Bool
  | Ge -- `>=` :: a -> a -> Bool
  | Lt -- `<`  :: a -> a -> Bool
  | Le -- `<=` :: a -> a -> Bool
  | And -- && :: a -> a -> Bool
  | Concat -- `..` :: String -> String -> String
  deriving (Eq, Show, Enum, Bounded)

instance Arbitrary Bop where
  arbitrary = elements [minBound .. maxBound]

newtype Var = V String
  deriving (Eq, Ord, Show)

instance Arbitrary Var where
  arbitrary = V <$> genName

-- | Representation of different types of values
data Value
  = IntVal Int -- 1
  | BoolVal Bool -- false, true
  | StringVal String -- "abd"
  | Word String
  deriving (Eq, Show, Ord)

-- | Generate a string literal, being careful about the characters that it may contain
genStringLit :: Gen String
genStringLit = escape <$> QC.listOf (QC.elements stringLitChars)
  where
    -- escape special characters appearing in the string,
    escape :: String -> String
    escape = foldr Char.showLitChar ""
    -- generate strings containing printable characters or spaces, but not including '\"'
    stringLitChars :: [Char]
    stringLitChars = Prelude.filter (\c -> c /= '\"' && c /= '\'' && (Char.isSpace c || Char.isPrint c)) ['\NUL' .. '~']

shrinkStringLit :: String -> [String]
shrinkStringLit s = filter (\c -> c /= '\"' && c /= '\'') <$> shrink s

shrinkWord :: String -> [String]
shrinkWord s = filter (\c -> c /= '"' && c /= '\'') <$> shrink s

instance Arbitrary Value where
  arbitrary =
    QC.oneof
      [ IntVal <$> arbitrary,
        BoolVal <$> arbitrary,
        StringVal <$> genStringLit
      ]

  shrink (IntVal n) = IntVal <$> shrink n
  shrink (BoolVal b) = BoolVal <$> shrink b
  shrink (StringVal s) = StringVal <$> shrinkStringLit s
  shrink (Word s) = []

-- | Representation for a list of all commands
newtype Block = Block [BashCommand]
  deriving (Eq, Show)

genBlock :: Gen Block
genBlock = QC.sized gen
  where
    gen n = QC.frequency [(1, Block <$> vectorOf 1 arbitrary)]

instance Arbitrary Block where
  arbitrary = genBlock

-- >>> QC.sample' (arbitrary :: Gen Block)
-- [Block [Assign (V "w") (Val (BoolVal False))],Block [ExecCommand (ExecName "i") [Arg "9",Arg "L"]],Block [PossibleAssign (PossibleAssignWS (V "p9lKo") "  " "=" "   " (Var (V "Ukxe")))],Block [ExecCommand (ExecName "ZoJd") [DoubleQuote [ArgS "V",ArgS "T",ArgS "e",ArgS "G",ArgS "b",ArgS "["],Arg "w",Arg "w"]],Block [ExecCommand (ExecName "IvaW2ZCp") [Arg "*",Arg "0",Arg "`",Arg "L"]],Block [PossibleAssign (PossibleAssignWS (V "dUpDn1V") "   " "=" "  " (Val (StringVal "WT")))],Block [Assign (V "pEPFDR5If") (Var (V "s9"))],Block [Assign (V "Yg") (Var (V "w"))],Block [ExecCommand (ExecName "dKt_R") [Arg "4",Arg "5",Arg "X",Arg "H",Arg "M"]],Block [ExecCommand (ExecName "PVLhzMQPEpeVQ_2Rej") [Arg "\\",Arg "_",DoubleQuote [ArgS "$",ArgS "3",ArgS "j",ArgS "n",ArgS ",",ArgS "|",ArgS "q",ArgS "W",ArgS "[",ArgS "L",ArgS "A",ArgS "N",ArgS "%",ArgS "0"],Arg "o",Arg "x"]],Block [ExecCommand (ExecName "YUYzCCyCtwJ64lJQPT") [DoubleQuote [ArgS "*",ArgS "c",ArgS "-"],Arg ",",Arg "`",Arg "s",Arg "|"]]]

instance Semigroup Block where
  Block s1 <> Block s2 = Block (s1 <> s2)

instance Monoid Block where
  mempty = Block []

-- | List of unary operators for conditional expressions
data IfUop
  = NotIf -- `!` :: a -> Bool
  | AndU -- `-a` ::
  | BlockSpecial -- `-b` :: a -> Bool
  | CharSpecial -- `-c` :: a -> Bool
  | FolderExists -- `-d` :: a -> Bool
  | FileOrFolderExists -- `-e` :: a -> Bool
  | FileExists -- `-f` :: a -> Bool
  | GroupId -- `-g` :: a -> Bool
  | Symlink -- `-h` or `-L` :: a -> Bol
  | StickyBit -- `-k` :: a -> Bool
  | Pipe -- `-p` :: a -> Bool
  | ReadPermission -- `-r` :: a -> Bool
  | FileSize -- `-s` :: a -> Bool
  | Socket -- `-S` :: a -> Bool
  | Terminal -- `-t` :: a -> Bool
  | UserId -- `-u` :: a -> Bool
  | WritePermission -- `-w` :: a -> Bool
  | ExecPermission -- `-x` :: a -> Bool
  | Owner -- `-O` :: a -> Bool
  | GroupIdUser -- `-G` :: a -> Bool
  | Modified -- `-N` :: a -> Bool
  | LengthZero -- `-z` :: a -> Bool
  | LengthNonZero -- `-n` :: a -> Bool
  | Or -- `-o` :: a -> Bool
  | ErrU
  deriving (Eq, Show, Enum, Bounded)

instance Arbitrary IfUop where
  arbitrary :: Gen IfUop
  arbitrary = elements (filter (/= ErrU) [minBound .. maxBound])

-- | List of unary operators for non-conditional expressions
data Uop
  = Not -- `!` :: a -> Bool
  deriving (Eq, Show, Enum, Bounded)

instance Arbitrary Uop where
  arbitrary = elements [minBound .. maxBound]

-- | Data types to represent the different types of tokens in a printf format
data PrintfToken
  = Token ArgToken
  | FormatS
  | FormatD
  | FormatF
  | FormatB
  | FormatX
  | FormatO
  | FormatE
  | FormatG
  | FormatA
  deriving (Show, Eq)

genPrintFToken :: Gen String
genPrintFToken = suchThat arbitrary $ \s -> not (null s) && '%' `notElem` s

instance Arbitrary PrintfToken where
  arbitrary :: Gen PrintfToken
  arbitrary =
    frequency
      [ (20, Token <$> genToken),
        (1, pure FormatS),
        (1, pure FormatD),
        (1, pure FormatF),
        (1, pure FormatB),
        (1, pure FormatX),
        (1, pure FormatO),
        (1, pure FormatE),
        (1, pure FormatG),
        (1, pure FormatA)
      ]
  shrink :: PrintfToken -> [PrintfToken]
  shrink (Token s) = []
  shrink _ = []

{- Checker data types -}
-- Datatype to differentiate between messages
data Message = WarningMessage String | ErrorMessage String | None
  deriving (Show, Eq)

-- History is a map of variables to the command assigned to it
type History = Map Var BashCommand

-- VarFrequency is a map of variables to the number of times they have been used
type VarFrequency = Map Var Int

-- | Common operators used in regex
-- | Referenced: https://support.workiva.com/hc/en-us/articles/4407304269204-Regular-expression-operators
regOps :: [Char]
regOps = ['^', '$', '.', '|', '(', '[', '{', '*', '+', '?', '/']

-- | Numerical operators for conditional expressions
numOps :: [IfBop]
numOps = [GtNIf, LtNIf, EqNIf, GeNIf, LtNIf, LeNIf, NeN]

-- | Operations for arithmetic statements
arithmeticOps :: [IfBop]
arithmeticOps = [PlusIf, MinusIf, TimesIf, DivideIf, ModuloIf]
