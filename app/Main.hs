{-# LANGUAGE LambdaCase #-}
module Main where
import System.Environment
import Control.Monad
import Control.Monad.Except

import Parse
import Eval
import Errors
import Types
import REPL
import Variables

main :: IO ()
main = do
    getArgs >>= \case
        [] -> runRepl
        (x:_) -> startEnv x
    -- putStrLn arg
    


