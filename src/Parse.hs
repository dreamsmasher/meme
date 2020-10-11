module Parse where

import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Numeric (readOct, readHex)
import Data.Char
import Debug.Trace

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Character Char 
             | Bool Bool deriving (Eq, Show)

data NumPrecision = Exact | Inexact | Short | Single | Double | Long deriving (Eq, Ord, Enum, Show)

backslash :: Char
backslash = '\\'

symbols :: Parser Char
symbols = oneOf "!$%&|*+=/:<=>?@^_~\""

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " <> show err
    Right val -> "Found value: " <> show val


spaces :: Parser ()
spaces = skipMany1 space


showtrace x = trace (show x) x

escaped :: Parser Char
escaped = do
    char '\\'
    c <- oneOf "\\\"nrt"
    return $ case c of
           '\\' ->  c
           '"'  ->  c
           'n'  ->  '\n'
           'r'  -> '\r'
           't'  -> '\t'




parseString :: Parser LispVal
parseString = do
    try $ char '"'
    s <- showtrace <$> many (escaped <|> anyChar)
    char '"'
    return $ String s

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


parseBool :: Parser LispVal
parseBool = try $ char '#' >> oneOf "ft" >>= \b -> (return . Bool) (case b of
                                                                'f' -> False
                                                                't' -> True)
parseAtom :: Parser LispVal
parseAtom = try $ do
    first <- letter <|> symbols
    rest <- many (letter <|> digit <|> symbols)
    pure $ Atom (first : rest)
        

returnNum :: (String -> [(Integer, String)]) -> String -> Parser LispVal
returnNum f = return . Number . fst . head . f

parseHex :: Parser LispVal
parseHex =  string "#x" >> many1 digit >>= returnNum readHex

parseOct :: Parser LispVal
parseOct =  string "#o" >> many1 digit >>= returnNum readOct

readBin :: (Num a) => String -> a
readBin = sum . zipWith (*) (iterate (* 2) 1) . reverse . map (fromIntegral . digitToInt)

parseBin :: Parser LispVal
parseBin =  string "#b" >> many1 digit >>= \d -> (return . Number . readBin) d

parseDec :: Parser LispVal
parseDec =  read <$> choice >>= return . Number
    where choice = try (string "#d" >> many1 digit) <|> many1 digit -- helps avoid clashes with Atoms


parseNumber :: Parser LispVal
parseNumber = try parseDec <|> try parseBin <|> try parseOct <|> try parseHex


parsePrecision :: Parser NumPrecision --TODO : add support for floats, leaving this for later
parsePrecision = do
    p <- optionMaybe (oneOf "eisdfl") 
    case p of 
      Nothing -> return Exact
      Just n  -> return $ case n of
                   'e' -> Exact
                   'i' -> Inexact
                   's' -> Short
                   'f' -> Single
                   'd' -> Double
                   'l' -> Long

sepNumOrAtom :: Parser LispVal
sepNumOrAtom = undefined

parseExpr :: Parser LispVal
parseExpr = parseNumber <|> parseAtom <|> parseString <|> parseBool <|> parseChar <|> parseQuoted <|> do char '('
                                                                                                         x <- try parseList <|> parseDottedList
                                                                                                         char ')'
                                                                                                         return x

parseParens :: Parser LispVal
parseParens = do
    try $ char ')'
    x <- try parseList <|> parseDottedList
    char '('
    return x


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


