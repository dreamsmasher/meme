{-# LANGUAGE ExistentialQuantification, LambdaCase #-}
module Environment (
    LispVal (..),
    NumPrecision (..),
    LispError (..),
    ThrowsError,
    IOThrowsError,
    Env,
    Unpack (..),
    nullEnv,
    unwordsList,
    runIOThrows,
    liftThrows,
    setVar,
    defineVar,
    getVar,
    bindVars,
    extractValue,


    ) where


import Control.Monad.Except
import Control.Exception
import Text.ParserCombinators.Parsec.Error
import GHC.IO.Handle


import Data.IORef
import Control.Monad.Trans.Except
import Control.Monad.Except


type Env = IORef [(String, IORef LispVal)]
type IOThrowsError = ExceptT LispError IO


data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Character Char 
             -- | TypeProc String 
             | Bool Bool 
             | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
             | Func {
                params :: [String]
              , varArg :: (Maybe String)
              , body :: [LispVal]
              , closure :: Env} 
             | IOFunc ([LispVal] -> IOThrowsError LispVal)
             | Port Handle


instance Eq LispVal where
    Atom a == Atom b = a == b
    DottedList a x == DottedList b y = a == b && x == y
    Number a == Number b = a == b
    String a == String b = a == b
    Character a == Character b = a == b
    Bool a == Bool b = a == b
    PrimitiveFunc a == PrimitiveFunc b = error "comparing primitive functions is unimplemented"
    Func p v b c == Func q w d e = all id $ [p == q, v == w, b == d, c == e]
    List l == List r = l == r
    _ == _ = False

instance Show LispVal where
    show (Atom s) = s
    show (List lst) = "(" <> unwordsList lst <> ")"
    show (DottedList h t) = "(" <> unwordsList h <> "." <> show t <> ")"
    show (Number n) = show n
    show (String c) = "\"" <> c <> "\""
    show (Character c) = [c]
    -- show (TypeProc t) = t
    show (Bool b) = if b then "#t" else "#f"
    show (PrimitiveFunc _) = "<primitive>"
    show (Func a v b c) =
        "(lambda (" <> unwords (map show a) <> 
            (case v of 
               Nothing -> ""
               Just va -> " . " <> va) <> ") ...)"
    show (Port _) = "<IO port>"
    show (IOFunc _) = "<IO Primitive>"


data NumPrecision = Exact
                  | Inexact 
                  | Short 
                  | Single 
                  | Double 
                  | Long deriving (Eq, Ord, Enum, Show)

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

instance Show LispError where
    show = \case 
        (UnboundVar msg var) -> "UnboundVar:" <> msg <> ": " <> var
        (BadSpecialForm msg frm) -> "BadSpecialForm: " <> msg <> ": " <> show frm
        (NotFunction msg func) -> "NotFunction: " <> msg <> ": " <> func
        (NumArgs exp fnd) -> "NumArgs: expected " <> show exp <> " args; found values " <> unwordsList fnd
        (TypeMismatch exp fnd) -> "TypeMismatch: expected " <> exp <> ", found " <> show fnd
        (Parser parseErr) -> "Parse error at " <> show parseErr

type ThrowsError = Either LispError

data Unpack = forall a. Eq a => Unpack (LispVal -> ThrowsError a)

--newtype Unpack = Unpack {
                --runPack :: forall a. (Eq a) => (LispVal -> ThrowsError a)
                 --} -- hahahaha

trapError :: (MonadError a m, Show a) => m String -> m String
trapError action = catchError action (pure . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val


unwordsList :: [LispVal] -> String
unwordsList = unwords . map show

nullEnv :: IO Env
nullEnv = newIORef []

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows = \case
    Left err -> throwError err
    Right val  -> pure val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows a = runExceptT (trapError a) >>= pure . extractValue

isBound :: Env -> String -> IO Bool
isBound env var = readIORef env >>= pure . maybe False (const True) . lookup var 

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do
    env <- liftIO $ readIORef envRef -- get env from our ref,
    maybe (throwError $ UnboundVar "Getting an unbound variable" var)
          (liftIO . readIORef) -- and read a variable if there's
          (lookup var env)     -- a match

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do
    env <- liftIO $ readIORef envRef -- get env from our ref
    maybe (throwError $ UnboundVar "Setting an unbound variable" var)
          (liftIO . (flip writeIORef value)) -- and only write if there's
          (lookup var env) -- a match
    pure value -- pure the changed value if so (not actually necessary)


defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
    isDef <- liftIO (isBound envRef var)
    if isDef 
        then setVar envRef var value >> pure value
        else liftIO $ do
            valueRef <- newIORef value
            env <- readIORef envRef
            writeIORef envRef ((var, valueRef) : env)
            pure value

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars  envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
    where extendEnv bindings env = liftM (++ env) (mapM addBind bindings)
          addBind (var, val) = newIORef val >>= \ref -> pure (var, ref)

