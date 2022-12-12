{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Suggestions where

import Data.Map (Map)
import Data.Map qualified as Map
import GHC.Base (undefined)
import Parsing
import ShellSyntax
import State qualified as S
import System.IO qualified as IO
import System.IO.Error qualified as IO
import TempParsing qualified as T

-- | updates the histroy of when parsing the bash command
updHistory :: BashCommand -> S.State (Map Var Expression) ()
updHistory bc = case bc of
  PossibleAssign var ex -> do
    m <- S.get
    let l = Map.insert var ex m
    S.put (Map.insert var ex m)
  Assign var ex -> do
    m <- S.get
    let l = Map.insert var ex m
    S.put (Map.insert var ex m)
  _ -> return ()

-- | Reads line of strings and outputs the warnings
-- parseByLine :: Parser BashCommand -> [String] -> Maybe (S.State (Map Var Expression) ())
-- parseByLine p (x : xs) = do
--   parseByLine p xs
--   case parse p x of
--     Left err -> Nothing
--     Right bc -> Just (updHistory bc)
-- parseByLine p [] = Just (return ())

-- >>> parseByLine T.assignP ["x=3", "x=5", "x=8"]
-- No instance for (Show (State (Map Var Expression) ()))
--   arising from a use of ‘evalPrint’
-- There are instances for similar types:
--   instance [safe] Show State -- Defined in ‘Test.HUnit.Base’
