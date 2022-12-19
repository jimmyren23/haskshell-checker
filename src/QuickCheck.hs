module QuickCheck where

import Checker
import Control.Monad (liftM2)
import Data.Either
import Parsing as P
import PrettyPrint
import ShellSyntax as S
import Test.QuickCheck (Arbitrary (..), Gen, Property, Testable (..), (==>))
import Test.QuickCheck qualified as QC
import Test.QuickCheck.Gen

{- Parsing QuickCheck -}
prop_firstRight :: Eq a => Either String a -> Either String a -> Bool
prop_firstRight e1 e2
  | isRight e1 = e1 == firstRight e1 e2
  | isRight e2 = e2 == firstRight e1 e2
  | otherwise = firstRight e1 e2 == e1

prop_get :: Char -> Bool
prop_get c = P.parse get [c] == Right c

-- prop_satisfy :: (Char -> Bool) -> Bool
-- prop_satisfy f = if f c then P.parse (P.satisfy f) [c] == Right c else P.parse (P.satisfy f) [c] == Left "error"
--   where
--     c = QC.choose

prop_char :: Char -> Bool
prop_char c = P.parse (P.char c) [c] == Right c

prop_string :: String -> Bool
prop_string s = P.parse (P.string s) (pretty s) == Right s

prop_int :: Int -> Bool
prop_int i = P.parse P.int (pretty i) == Right i

-- prop_chainl :: (Int -> Int -> Int) -> Int -> [Int] -> Bool
-- prop_chainl f i xs = P.parse (P.chainl (P.int) f i) (pretty xs) == Right (foldl f i xs)

prop_stringP :: String -> Bool
prop_stringP s = P.parse (stringP s) (pretty s) == Right ()

prop_constP :: String -> Int -> Bool
prop_constP s a = P.parse (constP s a) (pretty s) == Right a

-- prop_errP :: Int -> Bool
-- prop_errP a = P.parse (errP a) "error" == Left a
-- genStr :: Gen String
-- genStr =
--   QC.frequency
--     [ (1, return ""),
--       (1, liftM2 (++) arbitrary genChar)
--     ]

-- prop_eof :: Property
-- prop_eof = QC.forAll genStr $ \s -> if null s then P.parse P.eof s == Right () else P.parse P.eof (s ++ "\n") == Left ("\t<PARSING ERROR> Please check line: `" ++ s ++ "`.")

-- Generate a random character except for the newline character, with a higher probability of generating non-newline characters
genCharExceptNewline :: Gen Char
genCharExceptNewline = arbitrary `suchThat` (/= '\n')

genStringExceptNewline :: Gen String
genStringExceptNewline = listOf genCharExceptNewline

prop_untilNewline :: Property
prop_untilNewline = QC.forAll genStringExceptNewline $ \s -> P.parse untilNewline (s ++ "\n") == Right s

-- prop_roundtrip_exp :: Expression -> Bool
-- prop_roundtrip_exp e = P.parse expP (pretty e) == Right e
