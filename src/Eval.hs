{-# LANGUAGE LambdaCase, ExistentialQuantification #-}

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
import Types
import Errors
import Control.Monad.Except
import Debug.Trace

eval :: LispVal -> ThrowsError LispVal
eval = \case
    (List [Atom "quote", val]) -> return val
    (List [Atom "if", p, t, e]) -> do
                    eval p >>= \case
                        Bool False -> eval e    
                        Bool True  -> eval t
                        notBool    -> throwError $ TypeMismatch "bool" notBool
    (List (Atom func : args)) -> mapM eval args >>= apply func
    -- (List (TypeProc t : args)) -> mapM eval args >>= apply t
    val@(String _) -> return val
    val@(Number _) -> return val
    val@(Atom _)  -> return val -- probably gonna break something
    val@(Bool _) -> return val
    val@(Character _) -> return val
    incorrect -> throwError $ BadSpecialForm "Unrecognized special form" incorrect

evalExpr :: String -> ThrowsError LispVal
evalExpr = eval . extractValue . readExpr

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe funcErr ($ args) (lookup func primitives)
    where funcErr = throwError $ NotFunction "Unrecognized primitive function args" func

numericBinOp :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinOp f = \case
        []   -> throwError $ NumArgs 2 []
        [x]  -> throwError $ NumArgs 2 [x]
        args -> mapM unpackNum args >>= return . Number. foldl1 f

primitives :: [(String, [LispVal] -> ThrowsError LispVal)] -- refactor this into a case expr?
primitives = [
    ("+", numericBinOp (+)),
    ("-", numericBinOp (-)),
    ("*", numericBinOp (*)),
    ("/", numericBinOp div),
    ("mod", numericBinOp mod),
    ("remainder", numericBinOp rem),
    ("quotient", numericBinOp quot),
    ("=", numBoolBinOp (==)),
    ("/=", numBoolBinOp (/=)),
    ("<", numBoolBinOp (<)),
    (">", numBoolBinOp (>)),
    ("<=", numBoolBinOp (<=)),
    (">=", numBoolBinOp (>=)),
    ("&&", boolBoolBinOp (&&)),
    ("||", boolBoolBinOp (||)),
    ("cons", cons),
    ("car", car),
    ("cdr", cdr),
    ("eqv?", eqv),
    ("eq?", eqv),
    ("equal?", equal),
    ("string=?", strBoolBinOp (==)),
    ("string<?", strBoolBinOp (<)),
    ("string>?", strBoolBinOp (>)),
    ("string<=?", strBoolBinOp (<=)),
    ("string>=?", strBoolBinOp (>=)),
    ("boolean?", booleanUnOp (matchType (Bool True))),
    ("list?"   , booleanUnOp (matchType (List []) )),
    ("number?" , booleanUnOp (matchType (Number 0))),
    ("string?" , booleanUnOp (matchType (String ""))),
    ("symbol?" , booleanUnOp (matchSymbol)),
    ("char?"   , booleanUnOp (matchType (Character 'a'))),
    ("pair?"   , booleanUnOp (matchAny [List [], DottedList [] (Bool True)]))
    ]

booleanUnOp :: (LispVal -> LispVal) -> [LispVal] -> ThrowsError LispVal
booleanUnOp f = \case
    [x] -> return $ f x
    v   -> throwError $ NumArgs 1 v
    {-
    [l@(List _)] -> return $ f l
    [n@(Number _)] -> return $ f n
    [a@(Atom _)] -> return $ f a
    [s@(String _)] -> return $ f s
    [d@(DottedList _ _)] -> return $ f d
    [c@(Character _)] -> return $ f c
    v -> throwError $ NumArgs 1 v
    -}

boolBinOp :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinOp unpacker op = \case
                        [x, y] -> do
                            l <- unpacker x
                            r <- unpacker y
                            return (Bool $ op l  r)
                        args -> throwError $ NumArgs 2 args

numBoolBinOp :: (Integer -> Integer -> Bool) -> [LispVal] -> ThrowsError LispVal
numBoolBinOp = boolBinOp unpackNum

boolBoolBinOp :: (Bool -> Bool -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBoolBinOp = boolBinOp unpackBool

strBoolBinOp :: (String -> String -> Bool) -> [LispVal] -> ThrowsError LispVal
strBoolBinOp = boolBinOp unpackStr

matchSymbol :: LispVal -> LispVal
matchSymbol (Atom _) = Bool True
matchSymbol (List [Atom _]) = Bool True
matchSymbol x = Bool False
         
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

eqv :: [LispVal] -> ThrowsError LispVal
eqv =  -- this is ugly
    let eqv' (Bool a) (Bool b) = a == b
        eqv' (Number a) (Number b) = a == b
        eqv' (String a) (String b) = a == b
        eqv' (Atom a) (Atom b) = a == b
        eqv' (DottedList a a') (DottedList b b') = a == b && a' == b'
        eqv' (List a) (List b) = eqv'' a b
            where eqv''  [] []     = True
                  eqv'' [] (y:ys) = False
                  eqv''  (x:xs) [] = False
                  eqv'' (x:xs) (y:ys) = case eqv [x, y] of
                                          Left err -> False
                                          Right (Bool b) -> b && eqv' (List xs) (List ys)
        eqv' _ _ = False
     in \case
            [x, y] -> (return . Bool) $ eqv' x y
            xyz    -> throwError $ NumArgs 2 xyz
unpackNum :: LispVal -> ThrowsError Integer
unpackNum = \case
    Number n -> return n
    String s -> let parsed = reads s in 
                    if null parsed
                       then throwError $ TypeMismatch "number" $ String s
                       else return . fst. head $ parsed
    List [n] -> unpackNum n
    nAn      -> throwError $ TypeMismatch "number" nAn


unpackStr :: LispVal -> ThrowsError String
unpackStr = \case
    (String s) -> return s
    (Number s) -> return $ show s
    (Bool s)   -> return $ show s
    notaStr    -> throwError $ TypeMismatch "string" notaStr

unpackBool :: LispVal -> ThrowsError Bool
unpackBool = \case
    (Bool b) -> return b
    notaBool -> throwError $ TypeMismatch "bool" notaBool

-- atom    -> atom    -> bool
eqSymbol :: LispVal -> LispVal -> LispVal
eqSymbol (Atom a) (Atom b) = Bool (a == b)

symbolToString :: LispVal -> LispVal
symbolToString (Atom s) = String s
symbolToString (List [Atom "quote", x]) = symbolToString x

stringToSymbol :: LispVal -> LispVal
stringToSymbol (String s) = Atom s



car :: [LispVal] -> ThrowsError LispVal
car = \case
    [List (x:xs)] -> return x
    [DottedList (x:xs) _] -> return x
    [ridiculous] -> throwError $ TypeMismatch "pair" ridiculous
    ridiculousList -> throwError $ NumArgs 1 ridiculousList

cdr :: [LispVal] -> ThrowsError LispVal
cdr = \case
    [List (_:xs)] -> return $ List xs
    [DottedList [] z] -> return z
    [DottedList (x:xs) z] -> return $ DottedList xs z
    [invalid] -> throwError $ TypeMismatch "pair" invalid
    invalidList -> throwError $ NumArgs 1 invalidList

cons :: [LispVal] -> ThrowsError LispVal
cons = \case
    [x, List xs] -> return $ List (x:xs)
    [x, DottedList xs l] -> return $ DottedList (x:xs) l
    [x, y] -> return $ DottedList [x] y
    consequences -> throwError $ NumArgs 2 consequences

unpackEquals :: LispVal -> LispVal -> Unpack -> ThrowsError Bool
unpackEquals lv lz (Unpack r) = do
        ulv <- r lv
        ulz <- r lz
        return $ ulv == ulz
    `catchError` (const $ return False)

equal :: [LispVal] -> ThrowsError LispVal
equal = \case
    [x, y] -> do
        primEq <- liftM or $ mapM (unpackEquals x y) [Unpack unpackBool, Unpack unpackNum, Unpack unpackStr] -- need to wrap these in Unpack
        eqvEq  <- eqv [x, y]
        return . Bool $ (primEq || (\(Bool b) -> b) eqvEq)
    z      -> throwError $ NumArgs 2 z
