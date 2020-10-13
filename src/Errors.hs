{-# LANGUAGE LambdaCase #-}
module Errors (
    LispError (..),
    trapError
    ) where

import Control.Monad.Except
import Text.ParserCombinators.Parsec.Error

import Parse
import Eval

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

instance Show LispError where
    show = \case 
        (UnboundVar msg var) -> msg <> ": " <> var
        (BadSpecialForm msg frm) -> msg <> ": " <> show frm
        (NotFunction msg func) -> msg <> ": " <> func
        (NumArgs exp fnd) -> "Expected " <> show exp <> " args; found values " <> unwordsList fnd
        (TypeMismatch exp fnd) -> "Invalid type: expected " <> exp <> ", found " <> show fnd
        (Parser parseErr) -> "Parse error at " <> show parseErr

type ThrowsError = Either LispError

trapError :: (MonadError a m, Show a) => m String -> m String
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val


