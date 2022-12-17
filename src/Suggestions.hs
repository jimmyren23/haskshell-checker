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

-- | Main entry point that takes a bash script and checks it
evalScript :: String -> IO ()
evalScript filename = do
  res <- parseShellScript filename
  case res of
    Left err -> print (errorS err)
    Right (Block bcs) -> print (evalAll bcs)



-- >>> parse

-- >>> 

-- >>> parseShellScript "test/conditional.sh"
-- Right (Block [PossibleAssign (PossibleAssignWS (V "y") "" "=" " " (Val (IntVal 1))),Conditional (Op2 (Var (V "y")) Lt (Val (IntVal 1))) (Block [Assign (V "x") (Val (IntVal 2))]) (Block [Assign (V "x") (Val (IntVal 3))])])

-- >>> parseShellScript "test/conditional.sh"
-- Left "No parses"

-- >>> goExStAll ["echo $x"]

-- >>> goExStAll ["x=3", "echo $x"]
-- "Result: ExecCommand (ExecName \"echo\") [Arg \"$x\"], map: fromList [(V \"x\",Assign (V \"x\") (Val (IntVal 3)))]"

-- Just (ExecCommand (ExecName "echo") [DoubleQuote ["<tilde>"]],"")
-- Just (ExecCommand (ExecName "echo") [DoubleQuote ["<tilde>"]],"")

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
