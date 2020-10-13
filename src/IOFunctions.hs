{-# LANGUAGE LambdaCase #-}
module IOFunctions where

import GHC.IO.Handle
import GHC.IO.IOMode
import System.IO
import Control.Monad
import Control.Monad.Except

import System.IO
import Environment
import Parse

makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [String filename] = Port <$> (liftIO $ openFile filename mode)

closePort :: [LispVal] -> IOThrowsError LispVal
closePort = \case
    [Port p] -> liftIO (hClose p >> pure (Bool True))
    _        -> pure (Bool False)

readProc :: [LispVal] -> IOThrowsError LispVal
readProc = \case
    []         -> readProc [Port stdin]
    [Port p]   -> (liftIO $ hGetLine p) >>= liftThrows . readExpr
    (notPort:_)-> throwError $ TypeMismatch "port" notPort


writeProc :: [LispVal] -> IOThrowsError LispVal
writeProc = \case
    [    obj    ] -> writeProc [obj, Port stdout]
    [obj, Port p] -> liftIO $ hPrint p obj >> pure (Bool True)

readContents :: [LispVal] -> IOThrowsError LispVal
readContents = \case
    [String filename] -> String <$> liftIO (readFile filename)
    (notAFilename:_)  -> throwError $ TypeMismatch "string" notAFilename

load :: String -> IOThrowsError [LispVal]
load filename = (liftIO $ readFile filename) >>= liftThrows . readExprList

readAll :: [LispVal] -> IOThrowsError LispVal
readAll = \case
    [String filename] -> List <$> load filename
    (notAFilename:_)  -> throwError $ TypeMismatch "string" notAFilename


