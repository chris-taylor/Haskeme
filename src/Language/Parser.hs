module Language.Parser (readExpr, readExprList) where

import Control.Monad.Error
import Text.ParserCombinators.Parsec
import Text.Parsec.Language
import qualified Text.Parsec.Token as P
import Numeric (readOct, readHex, readFloat)
import Data.Array
import qualified Data.Map as Map
import Ratio
import Complex
import Data.Array
import Char (digitToInt)

import Language.Types

lispDef :: LanguageDef ()
lispDef = emptyDef
    { P.commentStart   = "#|"
    , P.commentEnd     = "|#"
    , P.commentLine    = ";"
    , P.nestedComments = True
    , P.identStart     = letter <|> symbol
    , P.identLetter    = letter <|> digit <|> symbol
    , P.caseSensitive  = True }

lexer      = P.makeTokenParser lispDef
dot        = P.dot lexer
parens     = P.parens lexer
brackets   = P.brackets lexer
identifier = P.identifier lexer
whiteSpace = P.whiteSpace lexer
lexeme     = P.lexeme lexer
natural    = P.natural lexer
float      = P.float lexer
stringLit  = P.stringLiteral lexer

readExpr :: String -> ThrowsError LispVal
readExpr = readOrThrow parseExpr

readExprList :: String -> ThrowsError [LispVal]
readExprList = readOrThrow exprList

readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case parse parser "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> return val

exprList :: Parser [LispVal]
exprList = whiteSpace >> many (lexeme parseExpr)

parseExpr :: Parser LispVal
parseExpr = try parseComplex
        <|> try parseFloat
        <|> try parseRatio
        <|> try parseNumber
        <|> try parseBool
        <|> try parseChar
        <|> parseFunction
        <|> parseNegated
        <|> parseAtom
        <|> parseString
        <|> parseQuote
        <|> parseQuasiquote
        <|> try parseUnquoteSplicing
        <|> parseUnquote
        <|> parseVector
        <|> parseHash
        <|> parseList

parseAtom :: Parser LispVal
parseAtom = identifier >>= return . Atom

parseGenAtom :: Parser LispVal
parseGenAtom = do
    atom <- identifier
    sep <- optionMaybe (oneOf ".")
    case sep of
        Nothing  -> return $ Atom atom
        Just '.' -> do
            next <- parseExpr
            return $ List [Atom atom, next]

parseString :: Parser LispVal
parseString = stringLit >>= return . String

parseBool :: Parser LispVal
parseBool = do
    string "#"
    x <- oneOf "tf"
    return $ case x of
                't' -> Bool True
                'f' -> Bool False

parseComplex :: Parser LispVal
parseComplex = let toDouble = either id fromIntegral in do
    s <- optionMaybe (oneOf "+-")
    x <- parseEither float natural
    t <- oneOf "+-"
    y <- parseEither float natural
    char 'i'
    let real = case s of
            Just '-' -> negate $ toDouble x
            _        -> toDouble x
        imag = case t of
            '-' -> negate $ toDouble y
            '+' -> toDouble y
    return $ Complex $ real :+ imag

parseFloat :: Parser LispVal
parseFloat = signed Float float

parseRatio :: Parser LispVal
parseRatio = signed Ratio rational

parseNumber :: Parser LispVal
parseNumber = signed Number natural

parseChar :: Parser LispVal
parseChar = liftM Char $ string "#\\" >> (try newline <|> try space <|> anyChar)
    where newline = string "newline" >> return '\n'
          space   = string "space" >> return ' '

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
    hd <- exprList
    do  char '.' >> whiteSpace
        tl <- parseExpr
        spaces >> char ')'
        return $ DottedList hd tl
      <|> (spaces >> char ')' >> (return $ List hd))

parseVector :: Parser LispVal
parseVector = do
    vals <- char '$' >> parens exprList
    return $ Vector (listArray (0, length vals - 1) vals)

parseHash :: Parser LispVal
parseHash = do
    vals <- char '#' >> parens exprList
    return $ Hash $ Map.fromList $ pairs vals

parseFunction :: Parser LispVal
parseFunction = do
    body <- brackets exprList
    return $ List [Atom "fn", List [Atom "_"], List body]

parseNegated :: Parser LispVal
parseNegated = do
    char '~'
    func <- parseAtom
    return $ List [Atom "complement", func]
    --return $ List [Atom "fn", List [Atom "_"], List [Atom "not", List [func, Atom "_"]]]

-- Parsing helper functions

symbol :: Parser Char
symbol = oneOf "!%&|*+-/:<=>?@^_~"

spaces1 :: Parser ()
spaces1 = skipMany1 space

rational :: Parser Rational
rational = do
    x <- natural
    char '/'
    y <- natural
    return (x % y)

parseEither :: Parser a -> Parser b -> Parser (Either a b)
parseEither left right = do
    l' <- optionMaybe (try left)
    case l' of
        Nothing -> right >>= return . Right
        Just l  -> return (Left l)

signed :: Num a => (a -> LispVal) -> Parser a -> Parser LispVal
signed constructor parser = do
    s <- optionMaybe (oneOf "+-")
    x <- parser
    return $ constructor $ case s of
        Just '-' -> negate x
        _        -> x