module ShellSyntax where

import Text.PrettyPrint (Doc, (<+>))
import Text.PrettyPrint qualified as PP

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


-- class PP a where
--   pp :: a -> Doc

-- -- | Default operation for the pretty printer. Displays using standard formatting
-- -- rules, with generous use of indentation and newlines.
-- pretty :: PP a => a -> String
-- pretty = PP.render . pp

-- -- | Compact version. Displays its argument without newlines.
-- oneLine :: PP a => a -> String
-- oneLine = PP.renderStyle (PP.style {PP.mode = PP.OneLineMode}) . pp

-- instance PP Uop where
--   pp Neg = PP.char '-'
--   pp Not = PP.text "not"

-- instance PP Bool where
--   pp True = PP.text "true"
--   pp False = PP.text "false"

-- instance PP String where
--   pp = PP.text

-- instance PP Int where
--   pp = PP.int

-- instance PP Var where
--   pp ( n) = PP.text n
--   pp (Dot (Var v) k) = pp v <> PP.text "." <> pp k
--   pp (Dot t k) = PP.parens (pp t) <> PP.text "." <> pp k
--   pp (Proj (Var v) k) = pp v <> PP.brackets (pp k)
--   pp (Proj t k) = PP.parens (pp t) <> PP.brackets (pp k)

-- instance PP Value where
--   pp (IntVal i) = pp i
--   pp (BoolVal b) = pp b
--   pp NilVal = PP.text "nil"
--   pp (StringVal s) = PP.text ("\"" <> s <> "\"")
--   pp (TableVal t) = PP.text "<" <> PP.text t <> PP.text ">"

-- isBase :: Expression -> Bool
-- isBase TableConst {} = True
-- isBase Val {} = True
-- isBase Var {} = True
-- isBase Op1 {} = True
-- isBase _ = False

-- instance PP Bop where
--   pp Plus = PP.char '+'
--   pp Minus = PP.char '-'
--   pp Times = PP.char '*'
--   pp Divide = PP.text "//"
--   pp Modulo = PP.text "%"
--   pp Gt = PP.char '>'
--   pp Ge = PP.text ">="
--   pp Lt = PP.char '<'
--   pp Le = PP.text "<="
--   pp Eq = PP.text "=="
--   pp Concat = PP.text ".."

-- instance PP Expression where
--   pp (Var v) = pp v
--   pp (Val v) = pp v
--   pp (Op1 o v) = pp o <+> if isBase v then pp v else PP.parens (pp v)
--   pp e@Op2 {} = ppPrec 0 e
--     where
--       ppPrec n (Op2 e1 bop e2) =
--         ppParens (level bop < n) $
--           ppPrec (level bop) e1 <+> pp bop <+> ppPrec (level bop + 1) e2
--       ppPrec _ e' = pp e'
--       ppParens b = if b then PP.parens else id
--   pp (TableConst fs) = PP.braces (PP.sep (PP.punctuate PP.comma (map pp fs)))

-- instance PP TableField where
--   pp (FieldName name e) = pp name <+> PP.equals <+> pp e
--   pp (FieldKey e1 e2) = PP.brackets (pp e1) <+> PP.equals <+> pp e2

-- instance PP Block where
--   pp (Block [s]) = pp s
--   pp (Block ss) = PP.vcat (map pp ss)

-- ppSS :: [Statement] -> Doc
-- ppSS ss = PP.vcat (map pp ss)

-- instance PP Statement where
--   pp (Assign x e) = pp x <+> PP.equals <+> pp e
--   pp (If guard b1 b2) =
--     PP.hang (PP.text "if" <+> pp guard <+> PP.text "then") 2 (pp b1)
--       PP.$$ PP.nest 2 (PP.text "else" PP.$$ pp b2)
--       PP.$$ PP.text "end"
--   pp (While guard e) =
--     PP.hang (PP.text "while" <+> pp guard <+> PP.text "do") 2 (pp e)
--       PP.$+$ PP.text "end"
--   pp Empty = PP.semi
--   pp (Repeat b e) =
--     PP.hang (PP.text "repeat") 2 (pp b)
--       PP.$+$ PP.text "until" <+> pp e

-- level :: Bop -> Int
-- level Times = 7
-- level Divide = 7
-- level Plus = 5
-- level Minus = 5
-- level Concat = 4
-- level _ = 3 -- comparison operators

-- instance PP a => PP (Map Value a) where
--   pp m = PP.braces (PP.vcat (map ppa (Map.toList m)))
--     where
--       ppa (StringVal s, v2) = PP.text s <+> PP.text "=" <+> pp v2
--       ppa (v1, v2) = PP.brackets (pp v1) <+> PP.text "=" <+> pp v2

-- instance PP a => PP (Map Name a) where
--   pp m = PP.braces (PP.vcat (map ppa (Map.toList m)))
--     where
--       ppa (s, v2) = PP.text s <+> PP.text "=" <+> pp v2

