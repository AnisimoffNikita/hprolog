module Main where

import Text.ParserCombinators.Parsec
import Text.Parsec (ParsecT, State)
import qualified Data.Map as M
import           Control.Monad.Writer
import           Control.Monad.State
-- import           Control.Monad.Trans.State
import           Control.Monad.Trans.Maybe

import Prolog.Syntax
import Prolog.Parser
import Prolog.Unification

main :: IO ()
main = print "!"



