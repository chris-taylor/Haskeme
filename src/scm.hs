module Main where

import System.Environment
import Control.Monad
import Text.ParserCombinators.Parsec hiding (spaces)

import Numeric (readInt)
import Char (digitToInt)

-- Data definitions

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | Char Char
             | String String
             | Bool Bool

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Char char) = ['#','\\',char]
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList hd tl) = "(" ++ unwordsList hd ++ " . " ++ showVal tl ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

instance Show LispVal where
    show = showVal

-- Main

main :: IO ()
main = do
    args <- getArgs
    putStrLn (readExpr (args !! 0))

-- Functions

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found: " ++ show val

-- Evaluation



-- Parsers

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

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

parseChar :: Parser LispVal
parseChar = liftM Char $ string "#\\" >> (newline <|> space <|> anyChar)
    where newline = string "newline" >> return '\n'
          space = string "space" >> return ' '

parseString :: Parser LispVal
parseString = do
    char '"'
    x <- many $ escapedChar <|> noneOf "\""
    char '"'
    return $ String x

parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    let atom = first:rest
    return $ case atom of
        "#t" -> Bool True
        "#f" -> Bool False
        _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = do
    digits <- try hex <|> try oct <|> try bin <|> dec
    return $ Number (read digits)
    where
        hex = do { string "#x"; s <- sign; y <- many1 hexDigit; return (s:"0x"++y) }
        oct = do { string "#o"; s <- sign; y <- many1 octDigit; return (s:"0o"++y) }
        bin = do { string "#b"; s <- sign; y <- many1 binDigit; return (show $ readBin $ s:y) }
        dec = do { optional (string "#d"); s <- sign; y <- many1 digit; return (s:y) }
        binDigit = oneOf "01"

parseExpr :: Parser LispVal
parseExpr = try parseChar
        <|> try parseNumber
        <|> parseAtom
        <|> parseString
        <|> parseQuoted
        <|> do char '('
               x <- try parseList <|> parseDottedList
               char ')'
               return x

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
    hd <- endBy parseExpr spaces
    tl <- char '.' >> spaces >> parseExpr
    return $ DottedList hd tl

parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

-- Helper functions

readBin :: Integral a => String -> a
readBin (' ':s) = readBin s
readBin ('-':s) = negate $ readBin s
readBin s       = fst . head $ readInt 2 (`elem` "01") digitToInt s
