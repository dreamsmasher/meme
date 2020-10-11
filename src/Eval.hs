module Eval (
    eval, 
    matchProc,
    apply,
    primitives

) where

import Text.ParserCombinators.Parsec ()
import Parse

eval :: LispVal -> LispVal
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func (fmap eval args)
eval (List (TypeProc t : args)) = undefined --matchProc t (fmap eval args)
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval val@(Character _) = val
eval (TypeProc t) = String t

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

primitives :: [(String, [LispVal] -> LispVal)]
primitives = [
    ("+", numericBinOp (+)),
    ("-", numericBinOp (-)),
    ("*", numericBinOp (*)),
    ("/", numericBinOp div),
    ("mod", numericBinOp mod),
    ("remainder", numericBinOp rem),
    ("quotient", numericBinOp quot)
 ]

matchProc :: String -> Either (LispVal -> LispVal) (LispVal -> LispVal -> LispVal)-- return Bool
matchProc s = f 
 where 
     bt = Bool True
     bf = Bool False
     f = case s of
        "boolean" -> Left $ \x -> case x of 
                            (Bool b) -> x
                            _        -> bf 
        "list"    -> Left $ \l -> case l of
                            (List l) -> bt
                            _        -> bf
        "pair"    -> Left $ \p -> case p of
                            (List _) -> bt
                            (DottedList _ _) -> bt
                            _ -> bf
        "symbol"  -> Left $ \s -> case s of
                            List [Atom "quote", List []] -> bf
                            Bool _ -> s
                            String _ -> bf
                            _ -> bt
        "char"    -> Left $  \c -> case c of
                            Char _ -> bt
                            _      -> bf
                            
        "eqv"     -> Right g
          
deconstruct :: LispVal -> a        
deconstruct lv = case lv of
    Bool b -> b
    Atom a -> a
    Number n -> n
    Char c -> c
    TypeProc t -> t
    String s -> s
    List l -> l

numericBinOp :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinOp f = (Number . foldl1 f . fmap unpackNum)

unpackNum :: LispVal -> Integer
unpackNum lv = case lv of
    Number n -> n
    String s -> let p = reads s :: [(Integer, String)]
                 in case p of
                    [] -> 0
                    ((n, _) : _) -> n
    List [n] -> unpackNum n
    _        -> 0




