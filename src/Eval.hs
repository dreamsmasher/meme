{-# LANGUAGE ExistentialQuantification, LambdaCase #-}

module Eval (
    eval, 
    evalExpr,
    apply,
    symbolToString,
    stringToSymbol,
    matchType,
    primitives,
    makeFunc

) where

import Text.ParserCombinators.Parsec ()
import Control.Applicative
import Control.Monad.Except
import Debug.Trace

import Parse
import IOPrims
import Environment
-- wrap Env within a reader monad? 
eval :: Env -> LispVal -> IOThrowsError LispVal
eval env = \case
    val@(String _) -> pure val
    val@(Number _) -> pure val
    val@(Atom a)   -> getVar env a
    val@(Bool _) -> pure val
    val@(Character _) -> pure val
    (List [Atom "quote", val]) -> pure val
    (List [Atom "if", p, t, e]) -> do
                    eval env p >>= \case
                        Bool False -> eval env e    
                        Bool True  -> eval env t
                        notBool    -> throwError $ TypeMismatch "bool" notBool
    (List (Atom "cond": args)) -> cond env args
    (List (Atom "case" : pred : args)) -> eval env pred >>= \p -> evalCase env p args
    (List (Atom "and": args)) -> evalAnd env args
    (List (Atom "or": args)) -> evalOr env args
    (List [Atom "set!", Atom var, form]) -> eval env form >>= setVar env var
    (List [Atom "define", Atom var, form]) -> eval env form >>= defineVar env var
    (List (Atom "define" : List (Atom var : params) : body)) ->
        makeNormalFunc env params body >>= defineVar env var
        -- e.g. (define (succ n) (+ 1 n))
    (List (Atom "define" : DottedList (Atom var : params) varArgs : body)) ->
        makeVarArgs varArgs env params body >>= defineVar env var
    (List (Atom "lambda" : List params : body)) ->
        makeNormalFunc env params body
        -- e.g. ((lambda n) (+ n 1) 3) -> 4
    (List (Atom "lambda" : DottedList params varArgs : body)) ->
        makeVarArgs varArgs env params body
    (List (Atom "lambda" : varArgs@(Atom _) : body)) ->
        makeVarArgs varArgs env [] body

    (List (func : args)) -> do
        f <- eval env func
        argVals <- mapM (eval env) args
        apply f argVals

    incorrect -> throwError $ BadSpecialForm "Unrecognized special form" incorrect

evalExpr :: Env -> String -> IOThrowsError LispVal
evalExpr env = eval env . extractValue . readExpr


makeFunc :: Maybe String -> Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeFunc v e p b = pure $ Func (fmap show p) v b e

makeNormalFunc :: Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeNormalFunc = makeFunc Nothing

makeVarArgs :: LispVal -> Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeVarArgs = makeFunc . Just . show
    {-
apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe funcErr ($ args ) (lookup func primitives)
    where funcErr = throwError $ NotFunction "Unrecognized primitive function args" func
-}

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply f args = case f of

    PrimitiveFunc g -> liftThrows (g args)

    Func p v b c    -> 
        if nParams /= nArgs && v == Nothing
          then throwError $ NumArgs nParams args
          else (liftIO $ bindVars c $ zip p args) >>= bindVarArgs v >>= evalBody

      where 
          getlen r = let l = length r in (l, fromIntegral l) -- save some length calls
          (lParams, nParams) = getlen p
          (lArgs,   nArgs)   = getlen args
          remainingArgs = drop lParams args
          evalBody env = liftM last $ mapM (eval env) b -- take the current state of the envRef
          bindVarArgs arg env = case arg of
              Nothing -> pure env
              Just name -> liftIO $ bindVars env [(name, List $ remainingArgs)]


numericBinOp :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinOp f = \case
        []   -> throwError $ NumArgs 2 []
        [x]  -> throwError $ NumArgs 2 [x]
        args -> mapM unpackNum args >>= pure . Number. foldl1 f

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
    ("/=",numBoolBinOp (/=)),
    ("<",  numBoolBinOp (<)),
    (">",  numBoolBinOp (>)),
    ("<=",  numBoolBinOp (<=)),
    (">=",  numBoolBinOp (>=)),
    ("&&",  boolBoolBinOp (&&)),
    ("||",  boolBoolBinOp (||)),
    ("cons",  cons),
    ("car",  car),
    ("cdr",  cdr),
    ("eqv?",  eqv),
    ("eq?",  eqv),
    ("equal?",  equal),
    ("string=?",  strBoolBinOp (==)),
    ("string<?",  strBoolBinOp (<)),
    ("string>?",  strBoolBinOp (>)),
    ("string<=?", strBoolBinOp (<=)),
    ("string>=?", strBoolBinOp (>=)),
    ("boolean?",  booleanUnOp (matchType (Bool True))),
    ("list?"   ,  booleanUnOp (matchType (List []) )),
    ("number?" ,  booleanUnOp (matchType (Number 0))),
    ("string?" ,  booleanUnOp (matchType (String ""))),
    ("symbol?" ,  booleanUnOp (matchSymbol)),
    ("char?"   ,  booleanUnOp (matchType (Character 'a'))),
    ("pair?"   ,  booleanUnOp (matchAny [List [], DottedList [] (Bool True)]))
    ]

ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives = [
            ("apply", applyProc),
            ("open-input-file", makePort ReadMode),
            ("open-output-file", makePort WriteMode),
            ("close-input-port", closePort),
            ("close-output-port", closePort),
            ("read", readProc),
            ("write", writeProc),
            ("read-contents", readContents),
            ("read-all", readAll)
               ]

bindPrimitives :: IO Env
bindPrimitives = nullEnv >>= (flip bindVars $ 
    (fmap (makePrim IOFunc) ioPrimitives) <> (fmap (makePrim PrimitiveFunc) primitives))
      where makePrim c (v, f) = (v, c f)



applyProc :: [LispVal] -> IOThrowsError LispVal -- deconstructs argument lists
applyProc = \case
    [f, List args] -> apply f args
    (f : args)     -> apply f args


booleanUnOp :: (LispVal -> LispVal) -> [LispVal] -> ThrowsError LispVal
booleanUnOp f = \case
    [x] -> pure $ f x
    v   -> throwError $ NumArgs 1 v
    {-
    [l@(List _)] -> pure $ f l
    [n@(Number _)] -> pure $ f n
    [a@(Atom _)] -> pure $ f a
    [s@(String _)] -> pure $ f s
    [d@(DottedList _ _)] -> pure $ f d
    [c@(Character _)] -> pure $ f c
    v -> throwError $ NumArgs 1 v
    -}

boolBinOp :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinOp unpacker op = \case
                        [x, y] -> do
                            l <- unpacker x
                            r <- unpacker y
                            pure (Bool $ op l  r)
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

unpackNum :: LispVal -> ThrowsError Integer
unpackNum = \case
    Number n -> pure n
    String s -> let parsed = reads s in 
                    if null parsed
                       then throwError $ TypeMismatch "number" $ String s
                       else pure . fst. head $ parsed
    List [n] -> unpackNum n
    nAn      -> throwError $ TypeMismatch "number" nAn


unpackStr :: LispVal -> ThrowsError String
unpackStr = \case
    (String s) -> pure s
    (Number s) -> pure $ show s
    (Bool s)   -> pure $ show s
    notaStr    -> throwError $ TypeMismatch "string" notaStr

unpackBool :: LispVal -> ThrowsError Bool
unpackBool = \case
    (Bool b) -> pure b
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
    [List (x:xs)] -> pure x
    [DottedList (x:xs) _] -> pure x
    [ridiculous] -> throwError $ TypeMismatch "pair" ridiculous
    ridiculousList -> throwError $ NumArgs 1 ridiculousList

cdr :: [LispVal] -> ThrowsError LispVal
cdr = \case
    [List (_:xs)] -> pure $ List xs
    [DottedList [] z] -> pure z
    [DottedList (x:xs) z] -> pure $ DottedList xs z
    [invalid] -> throwError $ TypeMismatch "pair" invalid
    invalidList -> throwError $ NumArgs 1 invalidList

cons :: [LispVal] -> ThrowsError LispVal
cons = \case
    [x, List xs] -> pure $ List (x:xs)
    [x, DottedList xs l] -> pure $ DottedList (x:xs) l
    [x, y] -> pure $ DottedList [x] y
    consequences -> throwError $ NumArgs 2 consequences
    -- haha ha

unpackEquals :: LispVal -> LispVal -> Unpack -> ThrowsError Bool
unpackEquals lv lz (Unpack r) = do
        ulv <- r lv
        ulz <- r lz
        pure $ ulv == ulz
    `catchError` (const $ pure False)


    {-
eqv :: [LispVal] -> ThrowsError LispVal
eqv =  -- this is ugly
    let eqv' (Bool a) (Bool b)     = a == b
        eqv' (Number a) (Number b) = a == b
        eqv' (String a) (String b) = a == b
        eqv' (Atom a) (Atom b)     = a == b
        eqv' (DottedList a a') (DottedList b b') = a == b && a' == b'
        eqv' (List a) (List b) = eqv'' a b
            where eqv'' [] []     = True
                  eqv'' [] (y:ys) = False
                  eqv'' (x:xs) [] = False
                  eqv'' (x:xs) (y:ys) = case eqv [x, y] of
                                          Left err -> False
                                          Right (Bool b) -> b && eqv' (List xs) (List ys)
        eqv' _ _ = False
     in \case
            [x, y] -> (pure . Bool) $ eqv' x y
            xyz    -> throwError $ NumArgs 2 xyz
            -}
eqv :: [LispVal] -> ThrowsError LispVal
eqv = \case
    [x, y] -> pure . Bool $ x == y
    notBinList -> throwError $ NumArgs 2 notBinList

equal :: [LispVal] -> ThrowsError LispVal
equal = \case
    [x@(List xs), y@(List ys)] -> listEqual xs ys
    [x, y] -> do
        primEq <- liftM or $ mapM (unpackEquals x y) [Unpack unpackBool, Unpack unpackNum, Unpack unpackStr] -- need to wrap these in Unpack
        eqvEq  <- eqv [x, y] 
        pure . Bool $ (primEq || (\(Bool b) -> b) eqvEq)
    z      -> throwError $ NumArgs 2 z

listEqual :: [LispVal] -> [LispVal] -> ThrowsError LispVal
listEqual []     [] = pure (Bool True)
listEqual (a:as) [] = pure (Bool False)
listEqual [] (b:bs) = pure (Bool False)
listEqual (a:as) (b:bs) = do 
    r <- liftM or $ mapM (unpackEquals a b) [Unpack unpackBool, Unpack unpackNum, Unpack unpackStr]
    if r 
     then listEqual as bs
     else pure (Bool False)

cond :: Env -> [LispVal] -> IOThrowsError LispVal
cond env = \case
    [] -> pure $ List [] -- in scheme repl, empty conds / conds that don't evaluate to anything 
    (List [p, r] : xs) -> do
        eval env p >>= \case
            Bool True -> eval env r
            Bool False -> cond env xs
            notBool -> throwError $ TypeMismatch "bool" notBool

evalAnd :: Env -> [LispVal] -> IOThrowsError LispVal
evalAnd env = \case
    [] -> pure $ Bool True
    (exp : xs) -> do
        res <- eval env exp
        case res of
          Bool True  -> evalAnd env xs
          _          -> pure res

evalOr :: Env -> [LispVal] -> IOThrowsError LispVal
evalOr env = \case
    [] -> pure (Bool False)
    (exp : xs) -> do
        res <- eval env exp
        case res of
          Bool False -> evalOr env xs
          _          -> pure res

evalCase :: Env -> LispVal -> [LispVal] -> IOThrowsError LispVal
evalCase env pred = \case
    [] -> pure (Bool False)
    (List [Atom "else", e]: _) -> eval env e >>= pure
    (List [l@(List _), c] : xs) -> 
        pure (anyEq pred l) >>= liftThrows >>= \case
          Bool True -> eval env c >>= pure
          _         -> evalCase env pred xs

    
anyEq :: LispVal -> LispVal -> ThrowsError LispVal
anyEq _ (List []) = pure $ Bool False
anyEq p (List (l:ls)) = eqv [p, l] >>= \case 
    Bool True -> pure $ Bool True
    _         -> anyEq p (List ls)


-- TODO: make this work again
makeString :: Env -> [LispVal] -> IOThrowsError LispVal
makeString env = (\case
         [List [k]] -> mkStr k '\x0'
         [List [k, (Character c)]] -> mkStr k c
         (err: _) -> throwError $ TypeMismatch "char" err -- temp placeholder error
             )
 where mkStr k c = do
        n <- eval env k >>= \e ->  (liftThrows $ unpackNum e)
        pure (String $ replicate (fromIntegral n) c)

charToStr :: [LispVal] -> IOThrowsError LispVal
charToStr = \case
    [Character c] -> pure (String [c])
    (notChr: _)        -> throwError $ TypeMismatch "char" notChr

strLen :: [LispVal] -> IOThrowsError LispVal
strLen = \case
    [String s] -> pure (Number . fromIntegral $ length s)
    (notStr:_) -> throwError $ TypeMismatch "string" notStr

strRef :: [LispVal] -> IOThrowsError LispVal
strRef  = \case
    [String s, Number k] -> pure . Character . (s !!) . fromIntegral $ k
    (notStrNum:_) -> throwError $ TypeMismatch "string, num" notStrNum

-- strSet :: [LispVal] -> IOThrowsError LispVal
-- strSet = \case
    -- [String s, Character c] -> 
    --
