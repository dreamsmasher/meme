{-# LANGUAGE LambdaCase #-}
module Main where
import System.Environment
import Control.Monad
import Control.Monad.Except

import Parse
import Eval
import REPL
import Environment

main :: IO ()
main = do
    getArgs >>= \case
        [] -> runRepl
        ("-i" : args) -> do
            env <- startEnv args
            runRepl
        args -> startEnv args
    


