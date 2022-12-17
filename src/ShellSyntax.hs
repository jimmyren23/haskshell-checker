module ShellSyntax where

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

-- | Representation of arguments
data Arg 
  = SingleQuote [Token] -- argument surrounded by single quotes
  | DoubleQuote [Token] -- argument surrounded by double quotes
  | Arg String -- "basic" argument : consecutive characters excluding space and quotes
  deriving (Eq, Show)

type Token = String -- Type for each word in quotes

{- Conditional syntax -}

-- | Representation of "if [...]" or "if [[...]]" expression
data IfExpression
  = IfVar Var -- global variables
  | IfVal Value -- literal values
  | IfOp1 Uop IfExpression -- unary operators
  | IfOp2 IfExpression IfBop IfExpression -- binary operators
  deriving (Eq, Show)

-- | List of binary operators permitted for bash conditional expressions
-- | Note: This list may not be comprehensive of all permitted binary operators. 
-- | Referenced : https://linuxhint.com/bash_operator_examples/#o73
data IfBop
  = Nt -- -nt file operator checking if a file is newer than the other
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

newtype Var = V String
  deriving (Eq, Ord, Show)

-- | Representation of different types of values 
data Value
  = IntVal Int -- 1
  | BoolVal Bool -- false, true
  | StringVal String -- "abd"
  | Misc
  deriving (Eq, Show, Ord)

-- | Representation for a list of all commands
newtype Block = Block [BashCommand]
  deriving (Eq, Show)

instance Semigroup Block where
  Block s1 <> Block s2 = Block (s1 <> s2)

instance Monoid Block where
  mempty = Block []

data Uop
  = Neg -- `-` :: Int -> Int
  | Not -- `not` :: a -> Bool
  deriving (Eq, Show, Enum, Bounded)

data Misc
  = Tilde -- ~
  | Esc -- \'
   deriving (Eq, Show, Enum, Bounded)

-- | Numerical operators for conditional expressions
numOps :: [IfBop]
numOps = [GtNIf, LtNIf, EqNIf, GeNIf, LtNIf, LeNIf, NeN]