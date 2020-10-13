{-#LANGUAGE LambdaCase#-}
module Eval (
    eval, 
    evalExpr,
    apply,
    symbolToString,
    stringToSymbol,
    matchType,
    primitives

) where

import Text.ParserCombinators.Parsec ()
import Control.Applicative
import Parse
import Debug.Trace

eval :: LispVal -> LispVal
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func (fmap eval args)
eval (List (TypeProc t : args)) = apply t (fmap eval args)
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval val@(Character _) = val
eval (TypeProc t) = String t
eval incorrect = throwError $ BadSpecialForm "Unrecognized special form" incorrect

evalExpr :: String -> LispVal
evalExpr = eval . readExpr
apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives 

numericBinOp :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinOp f = (Number . foldl1 f . fmap unpackNum)

primitives :: [(String, [LispVal] -> LispVal)]
primitives = [
    ("+", numericBinOp (+)),
    ("-", numericBinOp (-)),
    ("*", numericBinOp (*)),
    ("/", numericBinOp div),
    ("mod", numericBinOp mod),
    ("remainder", numericBinOp rem),
    ("quotient", numericBinOp quot),
    ("boolean?", booleanUnOp (matchType (Bool True))),
    ("list?"   , booleanUnOp (matchType (List []) )),
    ("number?" , booleanUnOp (matchType (Number 0))),
    ("symbol?" , booleanUnOp (matchSymbol)),
    ("char?"   , booleanUnOp (matchType (Character 'a'))),
    ("pair?"   , booleanUnOp (matchAny [List [], DottedList [] (Bool True)]))
 ]

booleanUnOp :: (LispVal -> LispVal) -> [LispVal] -> LispVal
booleanUnOp f [l@(List _)] = f l
booleanUnOp f [n@(Number _)] = f n
booleanUnOp f [a@(Atom _)] = f a
booleanUnOp f [d@(DottedList _ _)] = f d
booleanUnOp f [c@(Character _)] = f c
booleanUnOp _ v = error $ "Incorrect argument count in call " ++ show v

matchSymbol :: LispVal -> LispVal
matchSymbol (Atom _) = Bool True
matchSymbol (List [Atom _]) = Bool True
matchSymbol x = trace (show x) $ Bool False
         
matchAny :: [LispVal] -> LispVal -> LispVal
matchAny fs = Bool . any (\(Bool b) -> b) . liftA2 matchType fs . pure
-- partially apply matchType to all valid types within fs

matchType :: LispVal -> LispVal -> LispVal
matchType a = Bool . matchType' a
    where 
        matchType' (Atom _) (Atom _) = True 
        matchType' (String _) (String _) = True
        matchType' (Number _) (Number _) = True
        matchType' (List _) (List _) = True
        matchType' (Character _) (Character _) = True
        matchType' (DottedList _ _) (DottedList _ _) = True
        matchType' _ _ = False


unpackNum :: LispVal -> Integer
unpackNum lv = case lv of
    Number n -> n
    _        -> 0

         -- atom    -> atom    -> bool
eqSymbol :: LispVal -> LispVal -> LispVal
eqSymbol (Atom a) (Atom b) = Bool (a == b)

symbolToString :: LispVal -> LispVal
symbolToString (Atom s) = String s
symbolToString (List [Atom "quote", x]) = symbolToString x

stringToSymbol :: LispVal -> LispVal
stringToSymbol (String s) = Atom s



