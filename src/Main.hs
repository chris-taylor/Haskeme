{-# LANGUAGE ExistentialQuantification #-}

module Main where

import System.Environment
import Control.Monad
import Control.Monad.Error
import Text.ParserCombinators.Parsec hiding (spaces)

import Numeric (readInt)
import Char (digitToInt)

import LispVal
import LispError
import LispParser

-- Main

main :: IO ()
main = getArgs >>= readEvalPrint . head

readEvalPrint :: String -> IO ()
readEvalPrint arg = do
    evaled <- return $ liftM show $ readExpr arg >>= eval
    putStrLn $ extractValue $ trapError evaled

repl :: IO ReplResult
repl = iterateUntil (== Quit) $ do
    putStr prompt
    line <- getLine
    case line of
        "quit" -> return Quit
        ""     -> return Continue
        _      -> readEvalPrint line >> return Continue

prompt :: String
prompt = "haskeme> "

data ReplResult = Continue | Quit deriving Eq

-- Evaluation

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Char _)   = return val
eval val@(Number _) = return val
eval val@(Bool _)   = return val
eval (List [Atom "quote", val]) = return val
eval (List [Atom "if", predicate, conseq, alt]) = do
    result <- eval predicate
    case result of
        Bool True  -> eval conseq
        Bool False -> eval alt
        notBool    -> throwError $ TypeMismatch "boolean" notBool
eval (List (Atom func : args))  = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func)
                        ($ args)
                        (lookup func primitives)

-- Functions

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [ ("+", numericBinop (+))
             , ("-", numericBinop (-))
             , ("*", numericBinop (*))
             , ("/", numericBinop div)
             , ("mod", numericBinop mod)
             , ("quotient", numericBinop quot)
             , ("remainder", numericBinop rem)
             , ("symbol?", typeQuery isSymbol)
             , ("boolean?", typeQuery isBool)
             , ("char?", typeQuery isChar)
             , ("number?", typeQuery isNumber)
             , ("string?", typeQuery isString)
             , ("=", numBoolBinop (==))
             , ("<", numBoolBinop (<))
             , (">", numBoolBinop (>))
             , ("<=", numBoolBinop (<=))
             , (">=", numBoolBinop (>=))
             , ("&&", boolBoolBinop (&&))
             , ("||", boolBoolBinop (||))
             , ("string=?", strBoolBinop (==))
             , ("string<?", strBoolBinop (<))
             , ("string>?", strBoolBinop (>))
             , ("string<=?", strBoolBinop (<=))
             , ("string>=?", strBoolBinop (>=))
             , ("car", car)
             , ("cdr", cdr)
             , ("cons", cons)
             , ("list", list)
             , ("eqv?", eqv)
             , ("eq?", eqv)
             , ("equal?", equal) ]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params        = mapM unpackNum params >>= return . Number . foldl1 op

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2
    then throwError $ NumArgs 2 args
    else do left <- unpacker $ args !! 0
            right <- unpacker $ args !! 1
            return $ Bool $ left `op` right

numBoolBinop  = boolBinop unpackNum
strBoolBinop  = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n in
                        if null parsed
                            then throwError $ TypeMismatch "number" $ String n
                            else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum     = throwError $ TypeMismatch "number" notNum

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s)   = return $ show s
unpackStr (Char s)   = return $ show s
unpackStr notString  = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool  = throwError $ TypeMismatch "boolean" notBool

typeQuery :: (LispVal -> Bool) -> [LispVal] -> ThrowsError LispVal
typeQuery p [x] = return $ Bool (p x)
typeQuery p xs  = throwError $ NumArgs 1 xs

isSymbol :: LispVal -> Bool
isSymbol (Atom _) = True
isSymbol _        = False

isBool :: LispVal -> Bool
isBool (Bool _) = True
isBool _        = False

isChar :: LispVal -> Bool
isChar (Char _) = True
isChar _        = False

isNumber :: LispVal -> Bool
isNumber (Number _) = True
isNumber _          = False

isString :: LispVal -> Bool
isString (String _) = True
isString _          = False

car :: [LispVal] -> ThrowsError LispVal
car [List (x:xs)]         = return x
car [DottedList (x:xs) _] = return x
car [notList]             = throwError $ TypeMismatch "list" notList
car badArgs               = throwError $ NumArgs 2 badArgs

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x:xs)]           = return $ List xs
cdr [DottedList [_] x]      = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [notList]               = throwError $ TypeMismatch "list" notList
cdr badArgs                 = throwError $ NumArgs 2 badArgs

cons :: [LispVal] -> ThrowsError LispVal
cons [x, List []]          = return $ List [x]
cons [x, List xs]          = return $ List (x:xs)
cons [x, DottedList xs tl] = return $ DottedList (x:xs) tl
cons [x, y]                = return $ DottedList [x] y
cons badArgs               = throwError $ NumArgs 2 badArgs

list :: [LispVal] -> ThrowsError LispVal
list xs = return $ List xs

listEquals :: ([LispVal] -> ThrowsError LispVal) -> LispVal -> LispVal -> ThrowsError LispVal
listEquals eq (List arg1) (List arg2) = return $ Bool $ (length arg1 == length arg2) &&
    (all eqPair $ zip arg1 arg2) where
        eqPair (x1, x2) = case eq [x1, x2] of
            Left err -> False
            Right (Bool val) -> val

eqv :: [LispVal] -> ThrowsError LispVal
eqv [Bool arg1, Bool arg2] = return $ Bool $ arg1 == arg2
eqv [Atom arg1, Atom arg2] = return $ Bool $ arg1 == arg2
eqv [Char arg1, Char arg2] = return $ Bool $ arg1 == arg2
eqv [String arg1, String arg2] = return $ Bool $ arg1 == arg2
eqv [Number arg1, Number arg2] = return $ Bool $ arg1 == arg2
eqv [DottedList xs x, DottedList ys y] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [l1@(List _), l2@(List _)] = listEquals eqv l1 l2
eqv [_ , _] = return $ Bool False
eqv badArgs = throwError $ NumArgs 2 badArgs

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) = do
        unpacked1 <- unpacker arg1
        unpacked2 <- unpacker arg2
        return $ unpacked1 == unpacked2
    `catchError` (const $ return False)

equal :: [LispVal] -> ThrowsError LispVal
equal [DottedList xs x, DottedList ys y] = equal [List $ xs ++ [x], List $ ys ++ [y]]
equal [l1@(List _), l2@(List _)] = listEquals equal l1 l2
equal [arg1, arg2] = do
    primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2)
                       [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
    eqvEquals <- eqv [arg1, arg2]
    return $ Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgs = throwError $ NumArgs 2 badArgs

-- Monad loops

iterateUntil :: (Monad m) => (a -> Bool) -> m a -> m a
iterateUntil p x = do
    y <- x
    if p y
        then return y
        else iterateUntil p x
