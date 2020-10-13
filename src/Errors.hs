{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
module Errors (
    LispError (..),
    Unpack (..),
    ThrowsError,
    extractValue, 
    trapError
    ) where

import Control.Monad.Except
import Text.ParserCombinators.Parsec.Error

import Types

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

instance Show LispError where
    show = \case 
        (UnboundVar msg var) -> "UnboundVar" <> msg <> ": " <> var
        (BadSpecialForm msg frm) -> "BadSpecialForm " <> msg <> ": " <> show frm
        (NotFunction msg func) -> "NotFunction" <> msg <> ": " <> func
        (NumArgs exp fnd) -> "NumArgs: expected " <> show exp <> " args; found values " <> unwordsList fnd
        (TypeMismatch exp fnd) -> "TypeMismatch: Invalid type: expected " <> exp <> ", found " <> show fnd
        (Parser parseErr) -> "Parse error at " <> show parseErr

type ThrowsError = Either LispError

--newtype Unpack = Unpack {
                --runPack :: forall a. (Eq a) => (LispVal -> ThrowsError a)
                 --} -- hahahaha
data Unpack = forall a. Eq a => Unpack (LispVal -> ThrowsError a)

trapError :: (MonadError a m, Show a) => m String -> m String
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val


