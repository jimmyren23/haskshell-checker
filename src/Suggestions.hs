module Suggestions where

import Checker qualified as C
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
import ShellSyntax
import System.IO qualified as IO
import System.IO.Error qualified as IO
import TempParsing qualified as T

-- | Action that updates the state
updHistory :: MonadState (Map Var Expression) m => BashCommand -> m ()
updHistory bc = case bc of
  PossibleAssign var ex -> do
    oldHistory <- State.get
    let newHistory = Map.insert var ex oldHistory
    put newHistory
  Assign var ex -> do
    oldHistory <- State.get
    let newHistory = Map.insert var ex oldHistory
    put newHistory
  _ -> do
    return ()

-- | displays the error message
errorS :: Show a => a -> String
errorS cmd = "Error: Unable to Parse Command " ++ show cmd

evalLine :: (MonadError String m, MonadState (Map Var Expression) m) => String -> m BashCommand
evalLine s = do
  let res = parse T.bashCommandP s
  case res of
    Left err -> throwError $ errorS err
    Right bc -> do
      oldHistory <- State.get
      case C.checkUnassignedVar bc oldHistory of
        Left err -> throwError $ errorS err
        Right _ -> do
          updHistory bc
          return bc

evalAllLines :: (MonadError String m, MonadState (Map Var Expression) m) => [String] -> m BashCommand
evalAllLines [x] = do
  evalLine x
evalAllLines (x : xs) = do
  evalLine x
  evalAllLines xs
evalAllLines [] = undefined

showSt :: (a -> String) -> (a, Map Var Expression) -> String
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

-- >>> goExStAll ["x=3", "y=4", "echo $y"]
-- "Result: ExecCommand (ExecName \"echo\") [Arg \"$y\"], map: fromList [(V \"x\",Val (IntVal 3)),(V \"y\",Val (IntVal 4))]"

-- >>> goExStAll ["x=3", "y=4", "echo $z", "echo $y"]
-- "Raise: Error: Unable to Parse Command \"Error: z is not assigned\""

goStEx :: String -> String
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
--   pure $ parse T.bashCommandP line

-- -- | For each line, parse and update the history and output any errors
-- parseLines :: [String] -> StateT (Map Var Expression) IO ()
-- parseLines (x : xs) = do
--   let res = parse T.bashCommandP x
--   case res of
--     Left err -> return ()
--     Right bc -> updHistory bc
--   parseLines xs
-- parseLines [] = return ()

-- >>> runStateT (parseLines ["a=1", "b=2"]) Map.empty
-- ((),fromList [(V "a",Val (IntVal 1)),(V "b",Val (IntVal 2))])