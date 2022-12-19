module ShellSyntax where

import Control.Monad (liftM2, liftM3)
import Data.Char
import Data.Char qualified as Char
import Test.QuickCheck as QC
import Text.PrettyPrint (Doc, (<+>))

-- | Overall representation of different types of commands supported
data BashCommand
  = ExecCommand Command [Arg] -- "basic" command in the form of a keyword followed by tokenized list of arguments
  | Conditional IfExpression Block Block -- conditional statements in the form of "if ... then .. else .. fi"
  | PossibleConditional Expression Block Block -- potential conditional expressions with syntax errors
  | Assign Var Expression -- variable assignments in the form of "{var name}={value}"
  | PossibleAssign PossibleAssign -- potential assignment commands with syntax errors
  deriving (Eq, Show)

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
  = SingleQuote [Token] -- argument surrounded by single quotes
  | DoubleQuote [Token] -- argument surrounded by double quotes
  | Arg String -- "basic" argument : consecutive characters excluding space and quotes
  deriving (Eq, Show)

instance Arbitrary Arg where
  arbitrary :: Gen Arg
  arbitrary = QC.frequency [(1, SingleQuote <$> QC.listOf genToken), (1, DoubleQuote <$> QC.listOf genToken), (100, Arg <$> genWord)]

-- | Generates a word string
genWord :: Gen String
genWord = elements tokenChars >>= \c -> QC.frequency [(50, return [c]), (1, QC.listOf (elements tokenChars) >>= \cs -> return (c : cs))]
  where
    tokenChars = Prelude.filter (\c -> c /= '"' && c /= '\'' && c /= '~' && not (isSpace c) && Char.isPrint c) ['\NUL' .. '~']

genToken :: Gen Token
genToken = QC.frequency [(80, genWord), (1, possibleTokens)]
  where
    possibleTokens :: Gen Token = elements ["<escape>", "<tilde>"]

type Token = String

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
  arbitrary = QC.frequency [(1, IfVar <$> arbitrary), (1, IfVal <$> arbitrary), (1, liftM2 IfOp1 arbitrary arbitrary), (1, liftM3 IfOp2 arbitrary arbitrary arbitrary), (1, liftM3 IfOp3 arbitrary arbitrary arbitrary)]

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
  arbitrary = elements [minBound .. maxBound]

{- Assignment syntax -}

-- | Commands that could be intepreted as assignments
data PossibleAssign
  = PossibleAssignWS Var String Equal String Expression -- errors from extra spaces surrounding Equal("=") sign
  | PossibleAssignDS Var Equal Expression -- error from prefixing variable name with "$"
  deriving (Eq, Show)

-- | Representation of all expressions except conditional (see "IfExpression")
data Expression
  = Var Var -- global variables
  | Val Value -- literal values
  | Op1 Uop Expression -- unary operators
  | Op2 Expression Bop Expression -- binary operators
  | Arr String
  deriving (Eq, Show)

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
  arbitrary = V <$> genWord

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

instance Semigroup Block where
  Block s1 <> Block s2 = Block (s1 <> s2)

instance Monoid Block where
  mempty = Block []

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
  | ExecPermission -- `-e` :: a -> Bool
  | Owner -- `-O` :: a -> Bool
  | GroupIdUser -- `-G` :: a -> Bool
  | Modified -- `-N` :: a -> Bool
  | LengthZero -- `-z` :: a -> Bool
  | LengthNonZero -- `-n` :: a -> Bool
  | Or -- `-o` :: a -> Bool
  | ErrU
  deriving (Eq, Show, Enum, Bounded)

instance Arbitrary IfUop where
  arbitrary = elements [minBound .. maxBound]

data Uop
  = Neg -- `-` :: Int -> Int
  | Not -- `not` :: a -> Bool
  deriving (Eq, Show, Enum, Bounded)

instance Arbitrary Uop where
  arbitrary = elements [minBound .. maxBound]

-- | Data types to represent the different types of tokens in a printf format
data PrintfToken
  = Token String
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

-- >>> QC.sample' (arbitrary :: Gen PrintfToken)
-- [FormatA,FormatS,FormatB,FormatE,Token "p",FormatB,Token "{",FormatE,FormatX,FormatX,FormatA]

-- | Common operators used in regex
-- | Referenced: https://support.workiva.com/hc/en-us/articles/4407304269204-Regular-expression-operators
regOps :: [Char]
regOps = ['^', '$', '.', '|', '(', '[', '{', '*', '+', '?', '/']

-- | Numerical operators for conditional expressions
numOps :: [IfBop]
numOps = [GtNIf, LtNIf, EqNIf, GeNIf, LtNIf, LeNIf, NeN]

arithmeticOps :: [IfBop]
arithmeticOps = [PlusIf, MinusIf, TimesIf, DivideIf, ModuloIf]
