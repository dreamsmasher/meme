module Main where
import System.Environment

import Parse
import Eval

main :: IO ()
main = do
    (expr:_) <- getArgs
    (print . eval . showTrace . readExpr) expr


