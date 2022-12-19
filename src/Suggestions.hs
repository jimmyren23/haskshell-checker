{-# LANGUAGE ImportQualifiedPost #-}

module Suggestions where

import Checker qualified as C
import Control.Applicative ( Alternative(empty) )
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
import Parsing ()
import ShellParsing as S ( parseShellScript )
import ShellSyntax
    ( Block(..),
      PossibleAssign(PossibleAssignDS, PossibleAssignWS),
      IfExpression(IfVar),
      BashCommand(Conditional, PossibleAssign, Assign),
      Var, Message (..) )
import System.IO qualified as IO
import System.IO.Error qualified as IO
import PrettyPrint (pretty)

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
  _ ->
    return ()

-- | displays the error message
errorS :: Message -> String
errorS m =
  case m of
    ErrorMessage s -> "<ERROR> " ++ s
    WarningMessage s -> "<WARNING> " ++ s ++ "\n\t"
    None -> empty

toStringMessages :: (BashCommand, [Message]) -> String
toStringMessages (bc, msgs) = foldr (\m acc -> errorS m ++ acc) "" msgs

-- | Shows the history and variable reference frequency
showSt :: ((BashCommand, [Message]) -> String) -> ((BashCommand, [Message]), MyState) -> String
showSt f (v, myState) = f v

-- | Show the result of runExceptT, parameterized by a function to show the value
showEx :: (((BashCommand, [Message]), MyState) -> String) -> Either String ((BashCommand, [Message]), MyState) -> String
showEx _ (Left m) = m
showEx f (Right v) = 
  case v of
    ((bc, []), _) -> "<SUCCESS> No issues detected ☺! \n\t"
    _ -> f v

-- | Evaluate a bashline
evalBashLine :: (MonadError String m, MonadState MyState m) => BashCommand -> [Message] -> m (BashCommand, [Message])
evalBashLine bc msgs = do
  myState <- State.get
  let oldHistory = history myState
  let oldVarFrequency = varFrequency myState
  case C.mainChecker oldHistory oldVarFrequency bc of
    Left err -> throwError $ errorS err
    Right (_, messages) -> do
      updateState bc
      return (bc, msgs ++ messages)

-- | Evaluate all bashlines
evalAllBashLines :: (MonadError String m, MonadState MyState m) => [BashCommand] -> [Message] -> m (BashCommand, [Message])
evalAllBashLines [x] msgs = do
  (bc, nMsgs) <- evalBashLine x msgs
  return (bc, nMsgs)
evalAllBashLines (x : xs) msgs = do
  (bc, nMsgs) <- evalBashLine x msgs
  evalAllBashLines xs nMsgs
evalAllBashLines [] msgs = undefined

evalAll :: [BashCommand] -> String
evalAll bcs =
  evalAllBashLines bcs []
    & flip runStateT (MyState {history = Map.empty, varFrequency = Map.empty})
    & runExceptT
    & runIdentity
    & showEx (showSt toStringMessages)

-- >>> parseShellScript "test/conditional.txt"

-- | Entrypoint to the checker
evalScript :: String -> IO ()
evalScript filename = do
  res <- parseShellScript filename
  case res of
    Left err -> putStrLn err
    Right (Block bcs) -> putStrLn ("\t" ++ evalAll bcs)

