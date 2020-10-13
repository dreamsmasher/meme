module Types (
    LispVal (..),
    NumPrecision (..),
    unwordsList
    ) where

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Character Char 
             | TypeProc String 
             | Bool Bool deriving (Eq)

instance Show LispVal where
    show (Atom s) = "Atom " <> s
    show (List lst) = "(" ++ unwordsList lst ++ ")"
    show (DottedList h t) = "(" ++ unwordsList h ++ "." ++ show t ++ ")"
    show (Number n) = show n
    show (String c) = "string \"" ++ c ++ "\""
    show (Character c) = [c]
    show (TypeProc t) = t
    show (Bool b) = if b then "#t" else "#f"

data NumPrecision = Exact
                  | Inexact 
                  | Short 
                  | Single 
                  | Double 
                  | Long deriving (Eq, Ord, Enum, Show)

unwordsList :: [LispVal] -> String
unwordsList = unwords . map show



