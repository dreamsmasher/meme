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
    parseString
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

import Errors 
import Types


backslash :: Char
backslash = '\\'

symbols :: Parser Char
symbols = oneOf "!$%&|-*+=/:<=>?@^_~"

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> return val

-- | our main parsing function
parseExpr :: Parser LispVal
parseExpr = 
        -- <|> parseProc
        parseNumber 
        <|> parseAtom 
        <|> parseString 
        <|> parseBool 
        <|> parseChar 
        <|> parseQuoted 
        <|> do char '('
               x <- try parseList <|> parseDottedList
               char ')'
               return x


spaces :: Parser ()
spaces = skipMany1 space

showTrace :: (Show a) => a -> a -- remove later
showTrace x = trace (show x) x

delims :: (Char, Char) -> Parser LispVal -> Parser LispVal
delims (l, r) = between (char l) (char r)

quotes, parens, brackets :: Parser LispVal -> Parser LispVal
quotes = delims ('"', '"')
parens = delims ('(', ')')
brackets = delims ('[', ']')

parseString :: Parser LispVal -- this was beyond annoying
parseString = try $ stringLiteral lexer >>= return . String
    where lexer = makeTokenParser haskellDef

parseChar :: Parser LispVal
parseChar = do  
        try $ string ['#', backslash]
        -- o <- many anyChar >>= \x -> oneOf (symbols <|> eof) >> return x
        o <- try (string "space") <|> try (string "newline") <|> do
            x <- anyChar
            notFollowedBy alphaNum
            return [x]
        return $ Character (case o of
                              "newline" -> '\n'
                              "space" -> ' '
                              _ -> head o)

parseProc :: Parser LispVal
parseProc = try $ do
    s <- many letter
    char '?'
    return $ TypeProc (s ++ "?")

parseBool :: Parser LispVal
parseBool = try $ char '#' >> oneOf "ft" >>= 
    \b -> (return . Bool) (case b of
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
parseHex =  string "#x" >> many1 digit >>= \d ->  return (getNum readHex d)

parseOct :: Parser Integer
parseOct =  string "#o" >> many1 digit >>= \o -> return (getNum readOct o)

readBin :: (Num a) => String -> a
readBin = sum . zipWith (*) (iterate (* 2) 1) . reverse . map (fromIntegral . digitToInt)

parseBin :: Parser Integer
parseBin =  string "#b" >> many1 digit >>= \b -> return  (readBin b)

parseDec :: Parser Integer
parseDec =  read <$> choice >>= return 
    where choice = try (string "#d" >> many1 digit) <|> many1 digit -- helps avoid clashes with Atoms

parseNumber :: Parser LispVal
parseNumber = try $ do
    sign <- optionMaybe (char '-')
    let
        sign' = case sign of
            Nothing -> 1
            Just _  -> (-1)
    n <- (try parseDec <|> try parseBin <|> try parseOct <|> try parseHex)
    return $ Number (sign' * n)

-- parsePrecision :: Parser NumPrecision --TODO : add support for floats, leaving this for later
-- parsePrecision = do
--     p <- optionMaybe (oneOf "eisdfl") 
--     case p of 
--       Nothing -> return Exact
--       Just n  -> return $ case n of
--                    'e' -> Exact
--                    'i' -> Inexact
--                    's' -> Short
--                    'f' -> Single
--                    'd' -> Double
--                    'l' -> Long

sepNumOrAtom :: Parser LispVal
sepNumOrAtom = undefined

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
    h <- endBy parseExpr spaces
    t <- char '.' >> spaces >> parseExpr
    return $ DottedList h t

parseQuoted :: Parser LispVal
parseQuoted = do
    try $ char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]


