module QuickCheck where

import Checker
import Control.Monad (liftM2)
import Data.Char (isAlpha, isDigit)
import Data.Char qualified as Char
import Data.Either
import Parsing as P
import PrettyPrint
import ShellParsing as SP
import ShellSyntax as S
import Test.QuickCheck (Arbitrary (..), Gen, Property, Testable (..), (==>))
import Test.QuickCheck qualified as QC
import Test.QuickCheck.Gen
import Test.QuickCheck.Property (Prop)

{- Parsing QuickCheck -}
prop_get :: Char -> Bool
prop_get c = P.parse get [c] == Right c

prop_char :: Char -> Bool
prop_char c = P.parse (P.char c) [c] == Right c

prop_string :: String -> Bool
prop_string s = P.parse (P.string s) (pretty s) == Right s

prop_int :: Int -> Bool
prop_int i = P.parse P.int (pretty i) == Right i

prop_stringP :: String -> Bool
prop_stringP s = P.parse (stringP s) (pretty s) == Right ()

prop_constP :: String -> Int -> Bool
prop_constP s a = P.parse (constP s a) (pretty s) == Right a

genStringExceptNewline :: Gen String
genStringExceptNewline = listOf (arbitrary `suchThat` (/= '\n'))

prop_untilNewline :: Property
prop_untilNewline = QC.forAll genStringExceptNewline $ \s -> P.parse untilNewline (s ++ "\n") == Right s

{- ShellParsing Properties -}
prop_startOfName :: Char -> Bool
prop_startOfName c = if isAlpha c || c == '_' then P.parse SP.startOfName [c] == Right c else P.parse SP.startOfName [c] == Left "\t<PARSING ERROR> No more characters to parse."

prop_restOfName :: Char -> Bool
prop_restOfName c = if isAlpha c || isDigit c || c == '_' then P.parse SP.restOfName [c] == Right c else P.parse SP.restOfName [c] == Left "\t<PARSING ERROR> No more characters to parse."

prop_nonPrintfArg :: String -> Bool
prop_nonPrintfArg s =
  if '%' `notElem` s
    then P.parse SP.nonPrintFArg (pretty s) == Right s
    else
      let (front, back) = break (== '%') s
       in P.parse SP.nonPrintFArg (pretty s) == Left back

prop_formatSpecP :: PrintfToken -> Property
prop_formatSpecP pfToken =
  isFormat pfToken ==> P.parse SP.formatSpecP (pretty pfToken) == Right pfToken

prop_printfToken :: PrintfToken -> Property
prop_printfToken pfToken =
  not (isFormat pfToken) ==> P.parse SP.printfTokenP (pretty pfToken) == Right pfToken

prop_typeCounter :: [PrintfToken] -> Bool
prop_typeCounter pfTokens = typeCounter pfTokens == length (Prelude.filter isFormat pfTokens)

prop_roundtrip_bop :: Bop -> Bool
prop_roundtrip_bop bop = P.parse SP.bopP (pretty bop) == Right bop

prop_roundtrip_ifBop :: IfBop -> Bool
prop_roundtrip_ifBop ifBop = P.parse SP.ifBopP (pretty ifBop) == Right ifBop

prop_roundtrip_uop :: Uop -> Bool
prop_roundtrip_uop uop = P.parse SP.uopP (pretty uop) == Right uop

prop_roundtrip_ifUop :: IfUop -> Property
prop_roundtrip_ifUop ifUop =
  ifUop /= ErrU ==> P.parse SP.ifUopP (pretty ifUop) == Right ifUop

prop_roundtrip_value :: Value -> Property
prop_roundtrip_value v =
  not (isWord v) ==> P.parse SP.valueP (pretty v) == Right v
  where
    isWord (Word _) = True
    isWord _ = False

prop_roundtrip_command :: Command -> Bool
prop_roundtrip_command c = P.parse SP.commandP (pretty c) == Right c

prop_roundtrip_arg :: Arg -> Bool
prop_roundtrip_arg a = P.parse SP.argP (pretty a) == Right a

prop_roundtrip_bash_command :: BashCommand -> Bool
prop_roundtrip_bash_command bc = P.parse SP.bashCommandP (pretty bc) == Right bc

prop_roundtrip_IfExpression :: IfExpression -> Bool
prop_roundtrip_IfExpression ie = P.parse SP.ifNonExp (pretty ie) == Right ie

prop_roundtrip_block :: Block -> Bool
prop_roundtrip_block b = P.parse SP.blockP (pretty b) == Right b

qc :: IO ()
qc = do
  putStrLn "Parsing QuickCheck"
  QC.quickCheck prop_get
  QC.quickCheck prop_char
  QC.quickCheck prop_string
  QC.quickCheck prop_int
  QC.quickCheck prop_stringP
  QC.quickCheck prop_constP
  QC.quickCheck prop_untilNewline
  putStrLn "ShellParsing QuickCheck"
  QC.quickCheck prop_startOfName
  QC.quickCheck prop_restOfName
  QC.quickCheck prop_nonPrintfArg
  QC.quickCheck prop_formatSpecP
  QC.quickCheck prop_printfToken
  QC.quickCheck prop_typeCounter
  QC.quickCheck prop_roundtrip_bop
  QC.quickCheck prop_roundtrip_ifBop
  QC.quickCheck prop_roundtrip_uop
  QC.quickCheck prop_roundtrip_ifUop
  QC.quickCheck prop_roundtrip_value

-- QC.quickCheck prop_roundtrip_command
-- QC.quickCheck prop_roundtrip_arg
-- QC.quickCheck prop_roundtrip_bash_command
-- QC.quickCheck prop_roundtrip_IfExpression
-- QC.quickCheck prop_roundtrip_block

{- Arbitrary Instances -}