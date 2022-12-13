{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module PrettyPrint where

import ShellSyntax

import Text.PrettyPrint (Doc, (<+>))
import Text.PrettyPrint qualified as PP

import Test.HUnit (Assertion, Counts, Test (..), assert, runTestTT, (~:), (~?=))

class PP a where
  pp :: a -> Doc

-- -- | Default operation for the pretty printer. Displays using standard formatting
-- -- rules, with generous use of indentation and newlines.
pretty :: PP a => a -> String
pretty = PP.render . pp

-- -- | Compact version. Displays its argument without newlines.
oneLine :: PP a => a -> String
oneLine = PP.renderStyle (PP.style {PP.mode = PP.OneLineMode}) . pp

instance PP Uop where
  pp Neg = PP.char '-'
  pp Not = PP.text "not"

instance PP Bool where
  pp True = PP.text "true"
  pp False = PP.text "false"

instance PP String where
  pp = PP.text

instance PP Int where
  pp = PP.int

instance PP Var where
  pp (V var)= PP.text var

instance PP Command where
  pp (ExecName comm)= PP.text comm

instance PP Arg where
  pp (Arg arg) = PP.text arg

instance PP Value where
  pp (IntVal i) = pp i
  pp (BoolVal b) = pp b
  pp NilVal = PP.text "nil"
  pp (StringVal s) = PP.text ("\"" <> s <> "\"")

isBase :: Expression -> Bool
isBase Val {} = True
isBase Var {} = True
isBase Op1 {} = True
isBase _ = False

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

instance PP Expression where
  pp (Var v) = pp v
  pp (Val v) = pp v
  pp (Op1 o v) = pp o <+> if isBase v then pp v else PP.parens (pp v)
  pp e@Op2 {} = ppPrec 0 e
    where
      ppPrec n (Op2 e1 bop e2) =
        ppParens (level bop < n) $
          ppPrec (level bop) e1 <+> pp bop <+> ppPrec (level bop + 1) e2
      ppPrec _ e' = pp e'
      ppParens b = if b then PP.parens else id

instance PP Block where
  pp (Block [s]) = pp s
  pp (Block ss) = PP.vcat (map pp ss)

ppSS :: [BashCommand] -> Doc
ppSS ss = PP.vcat (map pp ss)

instance PP BashCommand where
  pp (Assign x e) = pp x <> PP.equals <> pp e
  pp (PossibleAssign x e) = pp x <>  PP.equals <> pp e
  -- TODO: update conditional
  pp (Conditional guard b1 b2) =
    PP.hang (PP.text "if" <+> pp guard <+> PP.text "then") 2 (pp b1)
      PP.$$ PP.nest 2 (PP.text "else" PP.$$ pp b2)
      PP.$$ PP.text "end"
  pp (ExecCommand comm args) = pp comm <+> PP.sep [pp arg | arg <- args]

level :: Bop -> Int
level Times = 7
level Divide = 7
level Plus = 5
level Minus = 5
level Concat = 4
level _ = 3 -- comparison operators

test_prettyPrint :: Test
test_prettyPrint =
  TestList [
    pretty (Assign (V "var1") (Val (StringVal "hi"))) ~?= "var1=\"hi\"",
    pretty (PossibleAssign (V "var1") (Val (StringVal "hi"))) ~?= "var1=\"hi\"",
    pretty (ExecCommand (ExecName "echo") [ Arg "a", Arg "b", Arg "c"]) ~?= "echo a b c"
  ]

-- >>> runTestTT test_prettyPrint
-- Counts {cases = 3, tried = 3, errors = 0, failures = 0}

-- >>> pretty (PossibleAssign (V "var1") (Val (StringVal "hi")))
-- "var1 = \"hi\""

-- >>> PP.equals
-- =


-- >>> pp "hi" <+> pp "hello"
-- hi hello
