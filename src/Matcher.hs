{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda-case" #-}

module Matcher where

import Control.Applicative (Alternative (..))
import Control.Monad (guard)
import Data.Char
import Data.Foldable (asum)
import System.IO qualified as IO
import System.IO.Error qualified as IO
import Prelude hiding (filter)
