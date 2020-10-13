{-# LANGUAGE LambdaCase #-}
module REPL where

import System.IO
import Control.Monad
import Control.Monad.Except

import Parse
import Eval
import Environment

flushStr :: String -> IO ()
flushStr s = putStr s >> hFlush stdout

readPrompt :: String -> IO String
readPrompt s = flushStr s >> getLine

evalString :: Env -> String -> IO String
evalString env exp = runIOThrows . liftM show $ (liftThrows $ readExpr exp) >>= eval env 

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt act = do
    res <- prompt
    if (pred res) 
       then return ()
       else act res >> until_ pred prompt act

startEnv :: String -> IO ()
startEnv expr = bindPrimitives >>= flip evalAndPrint expr

runRepl :: IO ()
runRepl = do
    putStrLn "\n(✿ ╹◡ ╹) Welcome to normie-scheme 0.1.0!"
    putStrLn "Copyright 2020 Norman Liu.\n"
    bindPrimitives >>= until_ ("quit" ==) (readPrompt "λ > ") . evalAndPrint
    putStrLn "See you later! (✿ - ‿ ◦)\n"


