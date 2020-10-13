module Main where
import System.Environment
import Control.Monad
import Control.Monad.Except

import Parse
import Eval
import Errors
import Types

main :: IO ()
main = do
    arg <- head <$> getArgs
    -- putStrLn arg
    parsed <- pure $ show <$> readExpr arg
    case parsed of 
      Left err -> print err
      Right p -> putStrLn p
    evaled <- pure $ show <$> (readExpr arg >>= eval)
    (putStrLn . extractValue . trapError) evaled 


