{-#LANGUAGE OverloadedStrings #-}
module Parse (
    LispVal (..),
    parseExpr,
    unwordsList,
    readExpr,
    showTrace,
    parseTest,
    parseNumber,
    parseAtom,
    parserTrace,
    parserTraced,
    parseString,
    parseList
) where

import Text.ParserCombinators.Parsec hiding (spaces)
import Text.Parsec.Combinator
import Control.Monad.Except
import Control.Monad
import Numeric (readOct, readHex)
import Data.Char
import Debug.Trace
import Text.Parsec.Token 
import Text.Parsec.Language(haskellDef)

import Environment


backslash :: Char
backslash = '\\'

symbols :: Parser Char
symbols = oneOf "!$%&|-*+=/:<=>?@^_~"

readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case parse parser "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> pure val

readExpr :: String -> ThrowsError LispVal
readExpr = readOrThrow parseExpr

readExprList = readOrThrow (endBy parseExpr spaces)
-- | our main parsing function
parseExpr :: Parser LispVal
parseExpr = 
        -- <|> parseProc
        parseNumber 
        <|> parseString 
        <|> parseAtom 
        <|> parseBool 
        <|> parseChar 
        <|> parseQuoted 
        <|> do char '('
               x <- parseList <|> parseDottedList
               char ')'
               pure x


spaces :: Parser ()
spaces = skipMany space

showTrace :: (Show a) => a -> a -- remove later
showTrace x = trace (show x) x

delims :: (Char, Char) -> Parser LispVal -> Parser LispVal
delims (l, r) = between (char l) (char r)

quotes, parens, brackets :: Parser LispVal -> Parser LispVal
quotes = delims ('"', '"')
parens = delims ('(', ')')
brackets = delims ('[', ']')

parseString :: Parser LispVal -- this was beyond annoying
parseString = try $ stringLiteral lexer >>= pure . String
    where lexer = makeTokenParser haskellDef
    {-
    char '"'
    let escaped :: Parser Char
        escaped = do
            char '\\'
            c <- oneOf "\"btrnfv'\\"
            pure $ case c of
                       'n' -> '\n'
                       'r' -> '\r'
                       't' -> '\t'
                       'f' -> '\f'
                       'v' -> '\v'
                       '\\'-> '\\'
                       'b' -> '\b'
                       '"' -> '"'
    x <- many (try escaped <|> anyChar)
    char '"'
    pure (String x)
    -}

parseChar :: Parser LispVal
parseChar = try $ do  
        string ['#', backslash]
        -- o <- many anyChar >>= \x -> oneOf (symbols <|> eof) >> pure x
        o <- try (string "space") <|> try (string "newline") <|> do
            x <- anyChar
            notFollowedBy alphaNum
            pure [x]
        pure $ Character (case o of
                              "newline" -> '\n'
                              "space" -> ' '
                              _ -> head o)

    {- redundant
parseProc :: Parser LispVal
parseProc = try $ do
    s <- many letter
    char '?'
    pure $ TypeProc (s ++ "?")
    -}

parseBool :: Parser LispVal
parseBool = try $ char '#' >> oneOf "ft" >>= 
    \b -> (pure . Bool) (case b of
                            'f' -> False
                            't' -> True)

parseAtom :: Parser LispVal
parseAtom = try $ do
    first <- (try letter) <|> symbols
    rest <- many (try letter <|> try digit <|> symbols)
    pure $ Atom (first : rest)
        

getNum :: (String -> [(Integer, String)]) -> String -> Integer
getNum f = fst . head . f

parseHex :: Parser Integer
parseHex =  string "#x" >> many1 digit >>= \d ->  pure (getNum readHex d)

parseOct :: Parser Integer
parseOct =  string "#o" >> many1 digit >>= \o -> pure (getNum readOct o)

readBin :: (Num a) => String -> a
readBin = sum . zipWith (*) (iterate (* 2) 1) . reverse . map (fromIntegral . digitToInt)

parseBin :: Parser Integer
parseBin =  string "#b" >> many1 digit >>= \b -> pure  (readBin b)

parseDec :: Parser Integer
parseDec =  read <$> choice >>= pure 
    where choice = try (string "#d" >> many1 digit) <|> many1 digit -- helps avoid clashes with Atoms

parseNumber :: Parser LispVal
parseNumber = try $ do
    sign <- optionMaybe (char '-')
    let
        sign' = case sign of
            Nothing -> 1
            Just _  -> (-1)
    n <- (try parseDec <|> try parseBin <|> try parseOct <|> try parseHex)
    pure $ Number (sign' * n)

-- parsePrecision :: Parser NumPrecision --TODO : add support for floats, leaving this for later
-- parsePrecision = do
--     p <- optionMaybe (oneOf "eisdfl") 
--     case p of 
--       Nothing -> pure Exact
--       Just n  -> pure $ case n of
--                    'e' -> Exact
--                    'i' -> Inexact
--                    's' -> Short
--                    'f' -> Single
--                    'd' -> Double
--                    'l' -> Long

sepNumOrAtom :: Parser LispVal
sepNumOrAtom = undefined

parseList :: Parser LispVal
parseList = try $ liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = try $  do
    h <- endBy parseExpr spaces
    t <- char '.' >> spaces >> parseExpr
    pure $ DottedList h t

parseQuoted :: Parser LispVal
parseQuoted = do
    try $ char '\''
    x <- parseExpr
    pure $ List [Atom "quote", x]


