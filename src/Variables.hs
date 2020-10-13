{-# LANGUAGE LambdaCase #-}

module Variables where

import Data.IORef
import Control.Monad.Trans.Except
import Control.Monad.Except

import Parse
import Types
import Errors

type Env = IORef [(String, IORef LispVal)]
type IOThrowsError = ExceptT LispError IO

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
