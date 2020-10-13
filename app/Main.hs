module Main where
import System.Environment

import Parse
import Eval
import Errors

main :: IO ()
main = do
    (expr:_) <- getArgs
    (print . eval . showTrace . readExpr) expr


