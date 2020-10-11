module Main where
import System.Environment

import Parse

main :: IO ()
main = do
    (expr:_) <- getArgs
    putStrLn expr
    putStrLn (readExpr expr)


