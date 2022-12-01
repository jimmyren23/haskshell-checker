module ShellSyntax where

data BashCommand
  = ExecCommand Command [Arg]
  | Conditional Expression Block Block
  | Assign Var Expression
  deriving (Eq, Show)

newtype Command = ExecName String
  deriving (Eq, Show)

newtype Arg = Arg String
  deriving (Eq, Show)

newtype Var
  = V String
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
  deriving (Eq, Show, Ord)

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
  deriving (Eq, Show, Enum, Bounded)

data Uop
  = Neg -- `-` :: Int -> Int
  | Not -- `not` :: a -> Bool
  deriving (Eq, Show, Enum, Bounded)
