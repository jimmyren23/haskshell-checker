{-# LANGUAGE ImportQualifiedPost #-}

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

data MyState = MyState
  { history :: Map Var BashCommand,
    varFrequency :: Map Var Int
  }
  deriving (Show, Eq)

-- | Action that updates the history
updateHistory :: MonadState MyState m => Var -> BashCommand -> m ()
updateHistory var bc = do
  myState <- State.get
  let oldHistory = history myState
  let newHistory = Map.insert var bc oldHistory
  put myState {history = newHistory}

-- | Action that updates the varFrequency
updateVarFrequency :: MonadState MyState m => Var -> m ()
updateVarFrequency var = do
  myState <- State.get
  let oldVarFrequency = varFrequency myState
  let newVarFrequency = Map.insertWith (+) var 1 oldVarFrequency
  put myState {varFrequency = newVarFrequency}

-- | updates state using all commands within a block.
updateBlock :: MonadState MyState m => Block -> m ()
updateBlock (Block []) = return ()
updateBlock (Block (x : xs)) = do
  updateState x
  updateBlock (Block xs)

-- | Action that updates the state
updateState :: MonadState MyState m => BashCommand -> m ()
updateState bc = case bc of
  PossibleAssign pa@(PossibleAssignWS var _ eq _ exp) -> do
    updateHistory var bc
    updateVarFrequency var
  PossibleAssign pa@(PossibleAssignDS var eq exp) -> do
    updateHistory var bc
    updateVarFrequency var
  Assign var ex -> do
    updateHistory var bc
    updateVarFrequency var
  Conditional (IfVar var) block1 block2 -> do
    updateBlock block1
    updateBlock block2
    updateVarFrequency var
  PossibleConditional (Var v) block1 block2 -> do
    updateBlock block1
    updateBlock block2
    updateVarFrequency v
  _ ->
    return ()

-- | displays the error message
errorS :: Show a => a -> String
errorS = show

-- | Shows the history and variable reference frequency
showSt :: (a -> String) -> (a, MyState) -> String
showSt f (v, myState) = f v ++ ", history: " ++ show (history myState) ++ ", varFrequency: " ++ show (varFrequency myState)

-- | Show the result of runExceptT, parameterized by a function to show the value
showEx :: (a -> String) -> Either String a -> String
showEx _ (Left m) = "<Error>: " ++ m
showEx f (Right v) = "<Result>: " ++ f v

-- >>> showEx show (Left "Error")
-- "<Error>: Error"
-- >>> showEx show (Right 5)
-- "<Result>: 5"

-- | Evaluate a bashline
evalBashLine :: (MonadError String m, MonadState MyState m) => BashCommand -> m BashCommand
evalBashLine bc = do
  myState <- State.get
  let oldHistory = history myState
  case bc of
    Conditional _ (Block b1) (Block b2) ->
      let res = C.checkConditionalSt bc oldHistory -- Checker
       in case res of
            Left err -> throwError $ errorS err
            Right _ -> do
              -- Make sure to evaluate nested blocks too
              evalAllBashLines b1
              evalAllBashLines b2
              return bc
    _ ->
      let res = C.checkExecCommandArgs bc oldHistory
       in case res of
            Left err -> throwError $ errorS err
            Right _ -> do
              updateState bc
              return bc

-- | Evaluate all bashlines
evalAllBashLines :: (MonadError String m, MonadState MyState m) => [BashCommand] -> m BashCommand
evalAllBashLines [x] = do
  evalBashLine x
evalAllBashLines (x : xs) = do
  evalBashLine x
  evalAllBashLines xs
evalAllBashLines [] = undefined

evalAll :: [BashCommand] -> String
evalAll bcs =
  evalAllBashLines bcs
    & flip runStateT (MyState {history = Map.empty, varFrequency = Map.empty})
    & runExceptT
    & runIdentity
    & showEx (showSt show)

-- | Entrypoint to the checker
evalScript :: String -> IO ()
evalScript filename = do
  res <- parseShellScript filename
  case res of
    Left err -> print (errorS err)
    Right (Block bcs) -> print (evalAll bcs)

-- | Evaluates a single line
-- evalLine :: (MonadError String m, MonadState MyState m) => String -> m BashCommand
-- evalLine s = case parse S.bashCommandP s of
--   Left err -> throwError $ errorS err
--   Right bc -> do
--     myState <- State.get
--     case C.checkExecCommandArgs bc (history myState) of
--       Left err -> throwError err
--       Right _ -> do
--         updateState bc
--         return bc

-- -- | Evaluates all lines in a script
-- evalAllLines :: (MonadError String m, MonadState MyState m) => [String] -> m BashCommand
-- evalAllLines [x] = do
--   evalLine x
-- evalAllLines (x : xs) = do
--   evalLine x
--   evalAllLines xs
-- evalAllLines [] = undefined
