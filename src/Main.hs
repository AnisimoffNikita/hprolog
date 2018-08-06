module Main where

import Text.ParserCombinators.Parsec
import Text.Parsec (ParsecT)
import qualified Data.Map as M
import           Control.Monad.Writer
import           Control.Monad.State
import           Control.Monad.Trans.Maybe

import qualified Language.Prolog.Syntax as Syn
import Language.Prolog.Semantics
import Language.Prolog.Parser
import Language.Prolog.IO
import Language.Prolog

main :: IO ()
main = undefined



