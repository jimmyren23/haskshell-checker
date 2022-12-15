module Suggestions where

import Checker qualified as C
import Control.Applicative
import Control.Monad.Except
  ( ExceptT,
    MonadError (throwError),
    runExceptT, 
  )
import Control.Monad.Identity
  ( Identity (runIdentity),
  )
import Control.Monad.State
  ( MonadState (get, put),
    StateT (runStateT),
  )
import Control.Monad.State qualified as State
import Data.Function ((&))
import Data.Map (Map)
import Data.Map qualified as Map
import GHC.Base (undefined)
import Parsing
import ShellParsing as S
import ShellSyntax
import System.IO qualified as IO
import System.IO.Error qualified as IO

-- | Action that updates the state
updHistory :: MonadState (Map Var (String, Bool)) m => BashCommand -> String -> m ()
updHistory bc s = case bc of
  PossibleAssign var ex -> do
    oldHistory <- State.get
    let newHistory = Map.insert var (s, False) oldHistory
    put newHistory
  Assign var ex -> do
    oldHistory <- State.get
    let newHistory = Map.insert var (s, True) oldHistory
    put newHistory
  _ -> do
    return ()

-- | displays the error message
errorS :: Show a => a -> String
errorS cmd = "Error: Unable to Parse Command " ++ show cmd

eitherOp :: Either String BashCommand -> Either String BashCommand -> Either String BashCommand
eitherOp (Left err1) _ = Left err1
eitherOp _  (Left err2) = Left err2
eitherOp (Right cmd) _  = Right cmd

evalLine :: (MonadError String m, MonadState (Map Var (String, Bool)) m) => String -> m BashCommand
evalLine s = do
  let res = parse S.bashCommandP s
  case res of
    Left err -> throwError $ errorS err
    Right bc -> do
      oldHistory <- State.get
      let res = C.checkUnassignedVar bc oldHistory `eitherOp` C.checkQuotedTildeExpansion bc in
        case res of
          Left err -> throwError $ errorS err
          Right _ -> do
            updHistory bc s
            return bc

evalAllLines :: (MonadError String m, MonadState (Map Var (String, Bool)) m) => [String] -> m BashCommand
evalAllLines [x] = do
  evalLine x
evalAllLines (x : xs) = do
  evalLine x
  evalAllLines xs
evalAllLines [] = undefined

showSt :: (a -> String) -> (a, Map Var (String, Bool)) -> String
showSt f (v, map) = f v ++ ", map: " ++ show map

-- | Show the result of runExceptT, parameterized by a function
-- to show the value
showEx :: (a -> String) -> Either ParseResult a -> String
showEx _ (Left m) = "Raise: " ++ m
showEx f (Right v) = "Result: " ++ f v

goExSt :: String -> String
goExSt e =
  evalLine e -- :: StateT Int (ExceptT String Identity) Int
    & flip runStateT Map.empty
    & runExceptT
    & runIdentity
    & showEx (showSt show)

goExStAll :: [String] -> String
goExStAll (x : xs) =
  evalAllLines (x : xs) -- :: StateT Int (ExceptT String Identity) Int
    & flip runStateT Map.empty
    & runExceptT
    & runIdentity
    & showEx (showSt show)
goExStAll [] = ""

-- >>> goExStAll ["x = 7", "y=10", "echo $y"]
-- "Raise: Error: Unable to Parse Command \"No parses\""

-- >>> goExStAll ["x = 3", "echo \"$x\""]
-- "Raise: Error: Unable to Parse Command \"Did you mean to assign variable x  when you wrote: x = 3? It was used later in: echo \\\"$x\\\"\""


goStEx e =
  evalLine e -- :: ExceptT String (StateT Int Identity) Int
    & runExceptT
    & flip runStateT Map.empty
    & runIdentity
    & showSt (showEx show)

-- >>> goExSt "x=3"
-- "Result: Assign (V \"x\") (Val (IntVal 3)), map: fromList [(V \"x\",Val (IntVal 3))]"

-- >> runStateT (parseLines ["x=3", "y=4", "ls -l -r"]) Map.empty
-- ((),fromList [(V "x",Val (IntVal 3)),(V "y",Val (IntVal 4))])

-- -- | Reads line and outputs the warnings and updates the state
-- parseLine :: String -> IO (Either ParseResult BashCommand)
-- parseLine line = do
--   pure $ parse S.bashCommandP line

-- -- | For each line, parse and update the history and output any errors
-- parseLines :: [String] -> StateT (Map Var Expression) IO ()
-- parseLines (x : xs) = do
--   let res = parse S.bashCommandP x
--   case res of
--     Left err -> return ()
--     Right bc -> updHistory bc
--   parseLines xs
-- parseLines [] = return ()

-- >>> runStateT (parseLines ["a=1", "b=2"]) Map.empty
-- ((),fromList [(V "a",Val (IntVal 1)),(V "b",Val (IntVal 2))])
