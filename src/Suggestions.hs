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

-- | Action that updates the state
updHistory :: MonadState (Map Var BashCommand) m => BashCommand -> m ()
updHistory bc = case bc of
  PossibleAssign pa@(PossibleAssignWS var _ eq _ exp) -> do
    oldHistory <- State.get
    let newHistory = Map.insert var bc oldHistory
    put newHistory
  PossibleAssign pa@(PossibleAssignDS var eq exp) -> do
    oldHistory <- State.get
    let newHistory = Map.insert var bc oldHistory
    put newHistory
  Assign var ex -> do
    oldHistory <- State.get
    let newHistory = Map.insert var bc oldHistory
    put newHistory
  _ -> do
    return ()

-- | displays the error message
errorS :: Show a => a -> String
errorS = show

evalLine :: (MonadError String m, MonadState (Map Var BashCommand) m) => String -> m BashCommand
evalLine s = do
  let res = parse S.bashCommandP s
  case res of
    Left err -> throwError $ errorS err
    Right bc -> do
      oldHistory <- State.get
      let res = C.checkExecCommandArgs bc oldHistory
       in case res of
            Left err -> throwError err
            Right _ -> do
              updHistory bc
              return bc

evalAllLines :: (MonadError String m, MonadState (Map Var BashCommand) m) => [String] -> m BashCommand
evalAllLines [x] = do
  evalLine x
evalAllLines (x : xs) = do
  evalLine x
  evalAllLines xs
evalAllLines [] = undefined

showSt :: (a -> String) -> (a, Map Var BashCommand) -> String
showSt f (v, map) = f v ++ ", map: " ++ show map

-- | Show the result of runExceptT, parameterized by a function
-- to show the value
showEx :: (a -> String) -> Either String a -> String
showEx _ (Left m) = "<HaskShell> " ++ m
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

evalBashLine :: (MonadError String m, MonadState (Map Var BashCommand) m) => BashCommand -> m BashCommand
evalBashLine bc = do
  oldHistory <- State.get
  case bc of
    Conditional _ (Block b1) (Block b2) ->
      let res = C.checkConditionalSt bc oldHistory
       in case res of
            Left err -> throwError err
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
              updHistory bc
              return bc

evalAllBashLines :: (MonadError String m, MonadState (Map Var BashCommand) m) => [BashCommand] -> m BashCommand
evalAllBashLines [x] = do
  evalBashLine x
evalAllBashLines (x : xs) = do
  evalBashLine x
  evalAllBashLines xs
evalAllBashLines [] = undefined

evalAll :: [BashCommand] -> String
evalAll bcs =
  evalAllBashLines bcs
    & flip runStateT Map.empty
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
