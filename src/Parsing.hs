{-# LANGUAGE InstanceSigs #-}
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
import Data.Foldable (Foldable (foldr), asum)
import Data.Map hiding (filter)
import ShellSyntax
import System.IO qualified as IO
import System.IO.Error qualified as IO
import Test.HUnit
import Prelude hiding (filter)

-- | A parser is a function that takes a string and returns untyped shell
newtype Parser a = P {doParse :: String -> Either String (a, String)}

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f p = P $ \s -> do
    (c, cs) <- doParse p s
    return (f c, cs)

instance Applicative Parser where
  pure :: a -> Parser a
  pure x = P $ \s -> Right (x, s)

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  p1 <*> p2 = P $ \s -> do
    (f, s') <- doParse p1 s
    (x, s'') <- doParse p2 s'
    return (f x, s'')

instance Alternative Parser where
  empty :: Parser a
  empty = P $ const $ Left "error"

  (<|>) :: Parser a -> Parser a -> Parser a
  p1 <|> p2 = P $ \s -> doParse p1 s `firstRight` doParse p2 s

-- | Combine two Maybe values together, producing the first
-- successful result
firstRight :: Either String a -> Either String a -> Either String a
firstRight (Right str) _ = Right str
firstRight _ (Right str) = Right str
firstRight (Left err) _ = Left err

newtype History = History (Map Var BashCommand)

-- | Filter the parsing results by a predicate
filter :: (a -> Bool) -> Parser a -> Parser a
filter f p = P $ \s -> do
  (c, cs) <- doParse p s
  if f c
    then return (c, cs)
    else do
      sn <- parse untilNewline cs
      Left ("[ParseError] Please check line:   " ++ sn ++ "   .")

-- | Return the next character from the input
get :: Parser Char
get = P $ \s -> case s of
  (c : cs) -> Right (c, cs)
  [] -> Left "[ParseError] No more characters to parse."

-- | Use a parser for a particular string.
parse :: Parser a -> String -> Either String a
parse parser str =
  case doParse parser str of
    Left err -> Left err
    Right (a, m) ->
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

newline = satisfy (== '\n')

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
wsP p = p <* many (space <|> newline)

stringP :: String -> Parser ()
stringP s = wsP (string s) *> pure ()

constP :: String -> a -> Parser a
constP s rtrnVal = wsP (string s) *> pure rtrnVal

errP :: a -> Parser a
errP rtrnVal = wsP (many (satisfy (/= ' '))) *> pure rtrnVal

-- many (satisfy (/= ' ')) <* string " " <* many get *> pure rtrnVal

-- >>> parse (errP ErrU) "-as"
-- Right ErrU

eof :: Parser ()
eof = P $ \s -> case s of
  [] -> Right ((), [])
  x -> do
    sn <- parse untilNewline x
    Left ("Please check line:   " ++ sn ++ "   ")

untilNewline :: Parser String
untilNewline = many (satisfy (/= '\n')) <* newline <* many get

-- >>> parse untilNewline "if sjallk\n ajslkdlf"
-- Right "if sjallk"

-- >>> parse bashCommandP "echo \"hi\""
-- Variable not in scope: bashCommandP :: Parser a

try1 :: FilePath -> IO String
try1 filename = do
  handle <- IO.openFile filename IO.ReadMode
  IO.hGetContents handle

-- >>> parse (many get <* (string "\n") <* many get) "if \n"
-- Left "err"

-- >>> try1 "test/conditional.txt"
-- "x=1\nif (( $z -eq \"hii\" ))\nthen\n  echo \"$y\"\nelse\n  echo \"hi\"\nfi\n"

{- File parsers -}

parseFromFile :: Parser a -> String -> IO (Either String a)
parseFromFile parser filename = do
  IO.catchIOError
    ( do
        handle <- IO.openFile filename IO.ReadMode
        str <- IO.hGetContents handle
        pure $ parse parser str
    )
    ( \e ->
        pure $ Left $ "Error:" ++ show e
    )
