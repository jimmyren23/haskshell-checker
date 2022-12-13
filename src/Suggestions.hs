{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Suggestions where

import Checker qualified as C
import Data.Map (Map)
import Data.Map qualified as Map
import GHC.Base (undefined)
import Parsing
import ShellSyntax
import State qualified as S
import System.IO qualified as IO
import System.IO.Error qualified as IO
import TempParsing qualified as T

-- | Action that updates the state
updHistory :: BashCommand -> S.State (Map Var Expression) ()
updHistory bc = case bc of
  PossibleAssign var ex -> do
    oldHistory <- S.get
    let newHistory = Map.insert var ex oldHistory
    S.put newHistory
  Assign var ex -> do
    oldHistory <- S.get
    let newHistory = Map.insert var ex oldHistory
    S.put newHistory
  _ -> do
    return ()

test :: ((), Map Var Expression)
test = S.runState (updHistory (Assign (V "x") (Val (IntVal 3)))) Map.empty

-- >>> test
-- ((),fromList [(V "x",Val (IntVal 3))])

-- | Read each line and update the history
readFile :: FilePath -> Parser BashCommand -> IO ()
readFile fileName parser = do
  contents <- IO.readFile fileName
  let lines = Prelude.lines contents
  return ()

-- | Reads line and outputs the warnings
parseLine :: String -> IO (Either ParseResult BashCommand)
parseLine line = do
  pure $ parse T.bashCommandP line

-- | For each line, parse and update the history
parseLines :: [String] -> S.State (Map Var Expression) ()
parseLines (x : xs) = do
  let res = parse T.bashCommandP x
  case res of
    Left err -> return ()
    Right bc -> updHistory bc
  parseLines xs
parseLines [] = return ()

-- >>> S.runState (parseLines ["x=3", "y=4", "ls -l -r"]) Map.empty
-- ((),fromList [(V "x",Val (IntVal 3)),(V "y",Val (IntVal 4))])
