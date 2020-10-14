{-# LANGUAGE LambdaCase #-}
module REPL (
    runRepl,
    startEnv

    ) where

import System.IO
import Control.Monad
import Control.Monad.Except
import System.Console.Haskeline


import Parse
import Eval
import Environment
import IOFunctions

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

startEnv :: [String] -> IO ()
startEnv args = do 
    env <- bindPrimitives >>= flip bindVars [("args", List $ String <$> tail args)]
    evaled <- runIOThrows $ show <$> eval env (List [Atom "load", String (head args)])
    hPutStrLn stderr evaled
        
loop :: Env -> InputT IO ()
loop env = handle (\Interrupt -> outputStr "" >> loop env) $ do
    input <- liftIO (hFlush stdout) >> getInputLine "λ > "
    let exitNicely = outputStrLn "See you later! (✿ - ‿ ◦)\n"
    case input of
      Nothing -> exitNicely >> return ()
      Just "quit" -> exitNicely >> return ()
      Just expr -> do
          evaled <- liftIO $ evalString env expr
          outputStrLn evaled
          loop env

runRepl :: IO ()
runRepl = do
    putStrLn "\n(✿ ╹◡ ╹) Welcome to normie-scheme 0.1.0!"
    putStrLn "Copyright 2020 Norman Liu."
    putStrLn "Press Ctrl-D or type `quit` to exit.\n"
    env <- bindPrimitives
    runInputT defaultSettings $ withInterrupt (loop env)
        -- until_ ("quit" ==) (readPrompt "λ > ") . evalAndPrint $ env


