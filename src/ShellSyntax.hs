module ShellSyntax where

import Text.PrettyPrint (Doc, (<+>))

data BashCommand
  = ExecCommand Command [Arg]
  | PossibleAssign PossibleAssign
  | Conditional IfExpression Block Block
  | Assign Var Expression
  deriving (Eq, Show)

newtype Command = ExecName String
  deriving (Eq, Show)

type Token = String

type Equal = String

type NameToken = String

newtype Var = V String
  deriving (Eq, Ord, Show)

data PossibleAssign 
  = PossibleAssignWS Var String Equal String Expression -- extra spaces
  | PossibleAssignDS Var Equal Expression -- $
  deriving (Eq, Show)

data IfExpression
  = IfVar Var -- global variables x and table indexing
  | IfVal Value -- literal values
  | IfOp1 Uop IfExpression -- unary operators
  | IfOp2 IfExpression IfBop IfExpression -- binary operators
  deriving (Eq, Show)

data Arg 
  = Arg String
  | SingleQuote [Token]
  | DoubleQuote [Token]
  deriving (Eq, Show)

data Expression
  = Var Var -- global variables x and table indexing
  | Val Value -- literal values
  | Op1 Uop Expression -- unary operators
  | Op2 Expression Bop Expression -- binary operators
  deriving (Eq, Show)

newtype Block = Block [BashCommand]
  deriving (Eq, Show)

instance Semigroup Block where
  Block s1 <> Block s2 = Block (s1 <> s2)

instance Monoid Block where
  mempty = Block []

data Value
  = NilVal -- nil
  | IntVal Int -- 1
  | BoolVal Bool -- false, true
  | StringVal String -- "abd"
  | Misc
  deriving (Eq, Show, Ord)



data Bop
  = Plus -- `+`  :: Int -> Int -> Int
  | Minus -- `-`  :: Int -> Int -> Int
  | Times -- `*`  :: Int -> Int -> Int
  | Divide -- `//` :: Int -> Int -> Int   -- floor division
  | Modulo -- `%`  :: Int -> Int -> Int   -- modulo
  | Eq -- `==` :: a -> a -> Bool
  | EqN -- -eq
  | Gt -- `>`  :: a -> a -> Bool
  | GtN -- -gt
  | Ge -- `>=` :: a -> a -> Bool
  | Lt -- `<`  :: a -> a -> Bool
  | LtN -- -lt
  | Le -- `<=` :: a -> a -> Bool
  | And -- &&
  | Concat -- `..` :: String -> String -> String
  deriving (Eq, Show, Enum, Bounded)

-- Referenced : https://linuxhint.com/bash_operator_examples/#o73
data IfBop
  = Nt -- -nt file operator checking if a file is newer than the other
  | Ot -- -ot file operator checking if a file is older than the other
  | Ef -- -ef checking if the two hard links are pointing the same file or not
  | EqIf -- `==` 
  | EqNIf -- -eq numerical operator
  | EqS -- = `=` string operaor
  | GtIf -- `>`  :: a -> a -> Bool
  | GtNIf -- -gt
  | GeIf -- `>=` :: a -> a -> Bool
  | GeNIf -- -ge
  | LtIf -- `<`  :: a -> a -> Bool
  | LtNIf -- -lt
  | LeIf -- `<=` :: a -> a -> Bool
  | LeNIf -- -le
  | Ne -- != 
  | NeN -- -ne
  | AndIf -- &&
  | Reg -- =~
  | Err 
  deriving (Eq, Show, Enum, Bounded)
-- "-nt", "-ot", "-ef", "==", "!=", "<=", ">=", "-eq", "-ne", "-lt", "-le",
--     "-gt", "-ge", "=~", ">", "<", "=", "\\<", "\\>", "\\<=", "\\>="

data Uop
  = Neg -- `-` :: Int -> Int
  | Not -- `not` :: a -> Bool
  deriving (Eq, Show, Enum, Bounded)

data Misc
  = Tilde -- ~
  | Esc -- \'
   deriving (Eq, Show, Enum, Bounded)

-- | Numerical operators for conditionals
numOps :: [IfBop]
numOps = [GtNIf, LtNIf, EqNIf, GeNIf, LtNIf, LeNIf, NeN]

-- data NewBashCommand
--   = SimpleCommand CommandName [Suffix]
--   | CompoundCommand IfClause
--   deriving (Eq, Show)

-- data IfClause = IfClause CompoundList CompoundList ElsePart | IfClauseSimple CompoundList CompoundList
--   deriving (Eq, Show)

-- newtype CompoundList = CompoundList [NewBashCommand]
--   deriving (Eq, Show)

-- data ElsePart =

-- newtype CommandName = C ShellSyntax.Word
--  s deriving (Eq, Show)

-- newtype Suffix = S ShellSyntax.Word
--   deriving (Eq, Show)

-- type Word = String

-- import Text.PrettyPrint qualified as PP

-- What are we looking for when taking string and turning it into untyped shell
-- We are looking for the following:
-- 1. Assignments
-- Spaces between
-- 2. Conditional statements
-- 3. Exec commands and arguments

-- data UntypedBashCommand =  ExecCommandUntyped CommandToken [ArgToken]
--   -- = AssignUntyped AssignToken -- Tokens are on the left and right side of an = sign, but might no necessarily be valid
--   -- | ConditionalUntyped ConditionToken Block -- We are only considering If 'conditional' then expression
--   deriving (Eq, Show)

-- newtype VarToken = VarToken String
--   deriving (Eq, Show)

-- newtype ExpressionToken = ExpressionToken String
--   deriving (Eq, Show)

-- newtype ConditionToken = ConditionToken String
--   deriving (Eq, Show)

-- newtype ThenToken = ThenToken String
--   deriving (Eq, Show)

-- now that we have untyped commands
-- we can parse through them ? create the valid syntax
-- TODO: Define intermediate types (inbetween raw input and fully-parsed the input)?
-- TODO: Account for for-loops too?