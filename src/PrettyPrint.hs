{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module PrettyPrint where

import ShellSyntax
  ( Arg (..),
    ArgToken (..),
    BashCommand (..),
    Block (..),
    Bop (..),
    Command (..),
    Expression (..),
    IfBop (..),
    IfExpression (..),
    IfUop (..),
    Message (ErrorMessage, None, WarningMessage),
    Misc (..),
    PossibleAssign (..),
    PrintfToken (..),
    Uop (..),
    Value (..),
    Var (..),
  )
import Test.HUnit (Assertion, Counts, Test (..), assert, runTestTT, (~:), (~?=))
import Text.PrettyPrint (Doc, (<+>))
import Text.PrettyPrint qualified as PP

class PP a where
  pp :: a -> Doc

-- -- | Default operation for the pretty printer. Displays using standard formatting
-- -- rules, with generous use of indentation and newlines.
pretty :: PP a => a -> String
pretty = PP.render . pp

-- -- | Compact version. Displays its argument without newlines.
oneLine :: PP a => a -> String
oneLine = PP.renderStyle (PP.style {PP.mode = PP.OneLineMode}) . pp

instance PP PrintfToken where
  pp (Token s) = pp s
  pp FormatS = PP.text "%s"
  pp FormatD = PP.text "%d"
  pp FormatF = PP.text "%f"
  pp FormatE = PP.text "%e"
  pp FormatG = PP.text "%g"
  pp FormatX = PP.text "%x"
  pp FormatO = PP.text "%o"
  pp FormatB = PP.text "%b"
  pp FormatA = PP.text "%a"

instance PP [PrintfToken] where
  pp [] = PP.empty
  pp [token] = pp token
  pp (x : xs) = pp x <> pp xs

instance PP Uop where
  pp Not = PP.text "not"

instance PP IfUop where
  pp NotIf = PP.text "!"
  pp AndU = PP.text "-a"
  pp BlockSpecial = PP.text "-b"
  pp CharSpecial = PP.text "-c"
  pp FolderExists = PP.text "-d"
  pp FileOrFolderExists = PP.text "-e"
  pp FileExists = PP.text "-f"
  pp GroupId = PP.text "-g"
  pp Symlink = PP.text "-h"
  pp StickyBit = PP.text "-k"
  pp Pipe = PP.text "-p"
  pp ReadPermission = PP.text "-r"
  pp FileSize = PP.text "-s"
  pp Socket = PP.text "-S"
  pp Terminal = PP.text "-t"
  pp UserId = PP.text "-u"
  pp WritePermission = PP.text "-w"
  pp ExecPermission = PP.text "-x"
  pp Owner = PP.text "-O"
  pp GroupIdUser = PP.text "-G"
  pp Modified = PP.text "-N"
  pp LengthZero = PP.text "-z"
  pp LengthNonZero = PP.text "-n"
  pp Or = PP.text "-o"
  pp ErrU = PP.text "<op>"

instance PP Bool where
  pp True = PP.text "true"
  pp False = PP.text "false"

instance PP String where
  pp = PP.text

instance PP Int where
  pp = PP.int

instance PP Var where
  pp (V var) = PP.text var

instance PP Command where
  pp (ExecName comm) = PP.text comm

instance PP Arg where
  pp (Arg arg) = PP.text arg
  pp (SingleQuote args) = PP.text "\'" <> pp args <> PP.text "\'"
  pp (DoubleQuote args) = PP.text "\"" <> pp args <> PP.text "\""

instance PP [Arg] where
  pp [] = PP.empty
  pp [arg] = pp arg
  pp (x : xs) = pp x <+> pp xs

instance PP [ArgToken] where
  pp [] = PP.empty
  pp [arg] = pp arg
  pp (x : xs) = pp x <+> pp xs

-- >>> pp ([Arg "hello", Arg "world", SingleQuote [ArgS "hello", ArgS "world"], DoubleQuote [ArgS "hello", ArgS "world"]])
-- hello world 'hello world' "hello world"

instance PP ArgToken where
  pp (ArgS s) = PP.text s
  pp (ArgM m) = pp m

-- >>> pp (ArgM (Misc ))
instance PP Misc where
  pp Tilde = PP.text "~"
  pp Esc = PP.text "\\'"

instance PP Value where
  pp (IntVal i) = pp i
  pp (BoolVal b) = pp b
  pp (StringVal s) = PP.text ("'" <> s <> "'")
  pp (Word v) = pp v

isBase :: Expression -> Bool
isBase Val {} = True
isBase Var {} = True
isBase Op1 {} = True
isBase _ = False

ifIsBase :: IfExpression -> Bool
ifIsBase IfVal {} = True
ifIsBase IfVar {} = True
ifIsBase IfOp1 {} = True
ifIsBase _ = False

instance PP Bop where
  pp Plus = PP.char '+'
  pp Minus = PP.char '-'
  pp Times = PP.char '*'
  pp Divide = PP.text "//"
  pp Modulo = PP.text "%"
  pp Gt = PP.char '>'
  pp Ge = PP.text ">="
  pp Lt = PP.char '<'
  pp Le = PP.text "<="
  pp Eq = PP.text "=="
  pp Concat = PP.text ".."
  pp And = PP.text "&&"

instance PP IfBop where
  pp PlusIf = PP.char '+'
  pp MinusIf = PP.char '-'
  pp TimesIf = PP.char '*'
  pp DivideIf = PP.text "//"
  pp ModuloIf = PP.text "%"
  pp Nt = PP.text "-nt"
  pp Ot = PP.text "-ot"
  pp Ef = PP.text "-ef"
  pp EqIf = PP.text "=="
  pp EqNIf = PP.text "-eq"
  pp EqS = PP.text "="
  pp GtIf = PP.text ">"
  pp GtNIf = PP.text "-gt"
  pp GeIf = PP.text ">="
  pp GeNIf = PP.text "-ge"
  pp LtIf = PP.text "<"
  pp LtNIf = PP.text "-lt"
  pp LeIf = PP.text "<="
  pp LeNIf = PP.text "-le"
  pp Ne = PP.text "!="
  pp NeN = PP.text "-ne"
  pp AndIf = PP.text "&&"
  pp Reg = PP.text "=~"
  pp Err = PP.text "<op>"

instance PP Expression where
  pp (Var v) = PP.text "$" <> pp v
  pp (Val v) = pp v
  pp (Op1 o v) = pp o <+> if isBase v then pp v else PP.parens (pp v)
  pp e@Op2 {} = ppPrec 0 e
    where
      ppPrec n (Op2 e1 bop e2) =
        ppParens (level bop < n) $
          ppPrec (level bop) e1 <+> pp bop <+> ppPrec (level bop + 1) e2
      ppPrec _ e' = pp e'
      ppParens b = if b then PP.parens else id
  pp (Arr s) = PP.text "(" <> pp s <> PP.text ")"

instance PP IfExpression where
  pp (IfVar v) = PP.text "$" <> pp v
  pp (IfVal v) = pp v
  pp (IfOp1 o v) = pp o <+> if ifIsBase v then pp v else PP.parens (pp v)
  pp e@IfOp2 {} = ppPrec e
    where
      ppPrec (IfOp2 e1 bop e2) = PP.parens (pp e1 <+> pp bop <+> pp e2)
      ppPrec e' = pp e'
  pp e@IfOp3 {} = ppPrec e
    where
      ppPrec (IfOp3 e1 bop e2) = PP.text "((" <+> pp e1 <+> pp bop <+> pp e2 <+> PP.text "))"
      ppPrec e' = pp e'

instance PP Block where
  pp (Block [s]) = pp s
  pp (Block ss) = PP.vcat (map pp ss)

instance PP PossibleAssign where
  pp (PossibleAssignDS var eq exp) = PP.text "$" <> pp var <> pp eq <> pp exp
  pp (PossibleAssignWS var sp1 eq sp2 exp) = pp var <> pp sp1 <> pp eq <> pp sp2 <> pp exp

ppSS :: [BashCommand] -> Doc
ppSS ss = PP.vcat (map pp ss)

instance PP BashCommand where
  pp (Assign x e) = pp x <> PP.equals <> pp e
  pp (PossibleAssign pa) = pp pa
  -- TODO: update conditional
  pp (Conditional ifExp b1 b2) =
    PP.hang (PP.text "if" <+> pp ifExp <+> PP.text "then") 2 (pp b1)
      PP.$$ PP.nest 2 (PP.text "else" PP.$$ pp b2)
      PP.$$ PP.text "fi"
  pp (ExecCommand comm args) = pp comm <+> pp args

instance PP Message where
  pp (WarningMessage s) = pp s
  pp (ErrorMessage s) = pp s
  pp None = PP.text "none"

level :: Bop -> Int
level Times = 7
level Divide = 7
level Plus = 5
level Minus = 5
level Concat = 4
level _ = 3 -- comparison operators

test_prettyPrint :: Test
test_prettyPrint =
  TestList
    [ pretty (Assign (V "var1") (Val (StringVal "hi"))) ~?= "var1=\'hi\'",
      -- pretty (PossibleAssign (V "var1") (Val (StringVal "hi"))) ~?= "var1 = \"hi\"",
      pretty (ExecCommand (ExecName "echo") [Arg "a", Arg "b", Arg "c"]) ~?= "echo a b c"
    ]
