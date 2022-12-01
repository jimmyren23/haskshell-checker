module Parsing where

import Control.Applicative (Alternative (..))
import ShellSyntax
import Data.Map

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

-- | Parses a line of input for an assignment
assignP :: Parser (BashCommand, History)
assignP = undefined

-- | Parses a line of input for a conditional statement
ifP :: Parser (BashCommand, History)
ifP = undefined

-- | Parses a line of input for for exec commands and arguments
execCommandP :: Parser (BashCommand, History)
execCommandP = undefined

bashCommandP :: Parser (BashCommand, History)
bashCommandP = assignP <|> ifP <|> execCommandP