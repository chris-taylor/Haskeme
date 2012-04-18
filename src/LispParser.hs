module LispParser (readExpr, readExprList) where

import Control.Monad.Error
import Text.ParserCombinators.Parsec
import Numeric (readOct, readHex, readFloat)
import Data.Array
import Ratio
import Complex
import Data.Array
import Char (digitToInt)

import LispVal
import LispError

readExpr :: String -> ThrowsError LispVal
readExpr = readOrThrow parseExpr

readExprList :: String -> ThrowsError [LispVal]
readExprList = readOrThrow (sepBy parseExpr spaces1)

readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case parse parser "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> return val

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> try parseBool
        <|> try parseComplex
        <|> try parseFloat
        <|> try parseRatio
        <|> try parseNumber
        <|> try parseChar
        <|> parseQuote
        <|> parseQuasiquote
        <|> try parseUnquoteSplicing
        <|> parseUnquote
        <|> parseVector
        <|> parseList

parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    return $ Atom (first:rest)

parseString :: Parser LispVal
parseString = do
    char '"'
    x <- many $ escapedChar <|> noneOf "\""
    char '"'
    return $ String x

parseBool :: Parser LispVal
parseBool = do
    string "#"
    x <- oneOf "tf"
    return $ case x of
                't' -> Bool True
                'f' -> Bool False

parseComplex :: Parser LispVal
parseComplex = do
    x <- (try parseFloat <|> parseDec1)
    char '+'
    y <- (try parseFloat <|> parseDec1)
    char 'i'
    return $ Complex (toDouble x :+ toDouble y)

parseFloat :: Parser LispVal
parseFloat = do
    x <- many1 digit
    char '.'
    y <- many1 digit
    return $ Float $ fst . head $ readFloat (x ++ "." ++ y)

parseRatio :: Parser LispVal
parseRatio = do
    x <- many1 digit
    char '/'
    y <- many1 digit
    return $ Ratio $ (read x) % (read y)

parseNumber :: Parser LispVal
parseNumber = do
    num <- parseDec1 <|> parseDec2 <|> parseHex <|> parseOct <|> parseBin
    return num

parseDec1 :: Parser LispVal
parseDec1 = do
    x <- many1 digit
    (return . Number . read) x

parseDec2 :: Parser LispVal
parseDec2 = do
    try $ string "#d"
    x <- many1 digit
    (return . Number . read) x

parseHex :: Parser LispVal
parseHex = do
    try $ string "#x"
    x <- many1 hexDigit
    return $ Number (hex2dig x)

parseOct :: Parser LispVal
parseOct = do
    try $ string "#o"
    x <- many1 octDigit
    return $ Number (oct2dig x)

parseBin :: Parser LispVal
parseBin = do
    try $ string "#b"
    x <- many1 (oneOf "01")
    return $ Number (bin2dig x)

parseChar :: Parser LispVal
parseChar = liftM Char $ string "#\\" >> (newline <|> space <|> anyChar)
    where newline = string "newline" >> return '\n'
          space = string "space" >> return ' '

parseQuote :: Parser LispVal
parseQuote = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

parseQuasiquote :: Parser LispVal
parseQuasiquote = do
    char '`'
    x <- parseExpr
    return $ List [Atom "quasiquote", x]

parseUnquoteSplicing :: Parser LispVal
parseUnquoteSplicing = do
    string ",@"
    x <- parseExpr
    return $ List [Atom "unquotesplicing", x]

parseUnquote :: Parser LispVal
parseUnquote = do
    char ','
    x <- parseExpr
    return $ List [Atom "unquote", x]

parseList :: Parser LispVal
parseList = do
    char '(' >> spaces
    hd <- parseExpr `sepEndBy` spaces1
    do char '.' >> spaces1
       tl <- parseExpr
       spaces >> char ')'
       return $ DottedList hd tl
     <|> (spaces >> char ')' >> (return $ List hd))

parseVector :: Parser LispVal
parseVector = do
    string "#(" >> spaces
    vals <- sepEndBy parseExpr spaces1
    spaces >> char ')'
    return $ Vector (listArray (0, length vals - 1) vals)

-- Parsing helper functions

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

spaces1 :: Parser ()
spaces1 = skipMany1 space

escapedChar :: Parser Char
escapedChar = do
    char '\\'
    x <- oneOf "\\\"tn"
    return $ case x of
        '\\' -> '\\'
        '\"' -> '\"'
        'n'  -> '\n'
        't'  -> '\t'

sign :: Parser Char
sign = option ' ' $ char '-'

toDouble :: LispVal -> Double
toDouble (Float f)  = f
toDouble (Number n) = fromIntegral n

oct2dig x = fst (readOct x !! 0)
hex2dig x = fst (readHex x !! 0)
bin2dig   = bin2dig' 0
bin2dig' digint ""  = digint
bin2dig' digint (x:xs) = bin2dig' old xs where
    old = 2 * digint + (if x == '0' then 0 else 1)

--readBin :: Integral a => String -> a
--readBin (' ':s) = readBin s
--readBin ('-':s) = negate $ readBin s
--readBin s       = fst . head $ readInt 2 (`elem` "01") digitToInt s