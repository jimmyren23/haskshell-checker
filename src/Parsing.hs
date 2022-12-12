{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda-case" #-}
{-# HLINT ignore "Use $>" #-}

module Parsing where

import Control.Applicative (Alternative (..))
import Control.Monad (guard)
import Data.Char
  ( Char,
    isAlpha,
    isDigit,
    isLower,
    isSpace,
    isUpper,
  )
import Data.Foldable
import Data.Map hiding (filter)
import ShellSyntax
import Test.HUnit
import Prelude hiding (filter)

-- Untyped shell

-- | A parser is a function that takes a string and returns untyped shell
newtype Parser a = P {doParse :: String -> Maybe (a, String)}

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f p = P $ \s -> do
    (c, cs) <- doParse p s
    return (f c, cs)

instance Applicative Parser where
  pure :: a -> Parser a
  pure x = P $ \s -> Just (x, s)

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  p1 <*> p2 = P $ \s -> do
    (f, s') <- doParse p1 s
    (x, s'') <- doParse p2 s'
    return (f x, s'')

instance Alternative Parser where
  empty :: Parser a
  empty = P $ const Nothing

  (<|>) :: Parser a -> Parser a -> Parser a
  p1 <|> p2 = P $ \s -> doParse p1 s `firstJust` doParse p2 s

-- | Combine two Maybe values together, producing the first
-- successful result
firstJust :: Maybe a -> Maybe a -> Maybe a
firstJust (Just x) _ = Just x
firstJust Nothing y = y

newtype History = History (Map Var Expression)

-- | Filter the parsing results by a predicate
filter :: (a -> Bool) -> Parser a -> Parser a
filter f p = P $ \s -> do
  (c, cs) <- doParse p s
  guard (f c)
  return (c, cs)

type ParseResult = String

-- | Return the next character from the input
get :: Parser Char
get = P $ \s -> case s of
  (c : cs) -> Just (c, cs)
  [] -> Nothing

-- | Use a parser for a particular string. Note that this parser
-- combinator library doesn't support descriptive parse errors, but we
-- give it a type similar to other Parsing libraries.
parse :: Parser a -> String -> Either ParseResult a
parse parser str =
  case doParse parser str of
    Nothing -> Left "No parses"
    Just (a, m) ->
      case m of
        "" -> Right a -- empty quote returned means no error
        _ -> Left m -- m stores feedback from checker if there was an error

-- | Return the next character if it satisfies the given predicate
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = filter p get

-- | Parsers for specific sorts of characters
alpha, digit, upper, lower, space :: Parser Char
alpha = satisfy isAlpha
digit = satisfy isDigit
upper = satisfy isUpper
lower = satisfy isLower
space = satisfy isSpace

-- | Parses and returns the specified character
-- succeeds only if the input is exactly that character
char :: Char -> Parser Char
char c = satisfy (c ==)

-- | Parses and returns the specified string.
-- Succeeds only if the input is the given string
string :: String -> Parser String
string = Data.Foldable.foldr (\c p -> (:) <$> char c <*> p) (pure "")

-- | succeed only if the input is a (positive or negative) integer
int :: Parser Int
int = read <$> ((++) <$> string "-" <*> some digit <|> some digit)

-- | Parses one or more occurrences of @p@ separated by binary operator
-- parser @pop@.  Returns a value produced by a /left/ associative application
-- of all functions returned by @pop@.
-- See the end of the `Parsers` lecture for explanation of this operator.
chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` pop = Prelude.foldl comb <$> p <*> rest
  where
    comb x (op, y) = x `op` y
    rest = many ((,) <$> pop <*> p)

-- | @chainl p pop x@ parses zero or more occurrences of @p@, separated by @pop@.
-- If there are no occurrences of @p@, then @x@ is returned.
chainl :: Parser b -> Parser (b -> b -> b) -> b -> Parser b
chainl p pop x = chainl1 p pop <|> pure x

-- | Combine all parsers in the list (sequentially)
choice :: [Parser a] -> Parser a
choice = asum -- equivalent to: foldr (<|>) empty

-- | @between open close p@ parses @open@, followed by @p@ and finally
--   @close@. Only the value of @p@ is pureed.
between :: Parser open -> Parser a -> Parser close -> Parser a
between open p close = open *> p <* close

-- | @sepBy p sep@ parses zero or more occurrences of @p@, separated by @sep@.
--   Returns a list of values returned by @p@.
sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy p sep = sepBy1 p sep <|> pure []

-- | @sepBy1 p sep@ parses one or more occurrences of @p@, separated by @sep@.
--   Returns a list of values returned by @p@.
sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 p sep = (:) <$> p <*> many (sep *> p)

wsP :: Parser a -> Parser a
wsP p = p <* many space

stringP :: String -> Parser ()
stringP s = wsP (string s) *> pure ()

constP :: String -> a -> Parser a
constP s rtrnVal = wsP (string s) *> pure rtrnVal
