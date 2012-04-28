{-# LANGUAGE NoMonomorphismRestriction, TypeSynonymInstances, FlexibleInstances #-}

module Language.LispVal (
      LispVal (..)
    , LispError (..)
    , VectorType
    , HashType
    , EnvType
    , Env
    , ThrowsError
    , IOThrowsError
    , showVal, nil, eqv, unwordsList, pairs, unpairs
    , makeNormalFunc, makeVarArgs, makeNormalMacro, makeVarArgsMacro
    , trapError, extractValue, liftThrows, runIOThrows
    ) where

import IO
import Data.IORef
import Data.Array
import qualified Data.Map as Map
import Ratio
import Complex
import Control.Monad.Error
import Text.ParserCombinators.Parsec (ParseError)

type EnvType = Map.Map String (IORef LispVal)
type Env = IORef EnvType

type VectorType = Array Int LispVal
type HashType = Map.Map LispVal LispVal

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Vector VectorType
             | Hash HashType
             | Number Integer
             | Ratio Rational
             | Float Double
             | Complex (Complex Double)
             | Char Char
             | String String
             | Bool Bool
             | PrimitiveFunc String ([LispVal] -> ThrowsError LispVal)
             | IOFunc String ([LispVal] -> IOThrowsError LispVal)
             | Func { params :: [String], vararg :: Maybe String
                    , body :: [LispVal], closure :: Env }
             | Macro { macroParams :: [String], macroVararg :: Maybe String
                     , macroBody :: [LispVal], macroClosure :: Env }
             | Port Handle

instance Show LispVal where
    show = showVal

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Char char) = ['#','\\',char]
showVal (Atom name) = name
showVal (Number num) = show num
showVal (Ratio num) = show (numerator num) ++ "/" ++ show (denominator num)
showVal (Float num) = show num
showVal (Complex num) = show (realPart num) ++ "+" ++ show (imagPart num) ++ "i"
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList hd tl) = "(" ++ unwordsList hd ++ " . " ++ showVal tl ++ ")"
showVal (Vector arr) = "$(" ++ unwordsList (elems arr) ++ ")"
showVal (Hash hash) = "#(" ++ unwordsList (unpairs $ zip (Map.keys hash) (Map.elems hash)) ++ ")"
showVal (PrimitiveFunc name _) = "<primitive:" ++ name ++ ">"
showVal (IOFunc name _) = "<ioPrimitive:" ++ name ++ ">"
showVal (Func { params = args, vararg = varargs }) = showFunc "fn" args varargs
showVal (Macro { macroParams = args, macroVararg = varargs }) = showFunc "macro" args varargs
showVal (Port _) = "<IO port>"

showFunc :: String -> [String] -> Maybe String -> String
showFunc name args varargs = 
    "(" ++ name ++ " (" ++ unwords args ++
        (case varargs of
            Nothing  -> ""
            Just arg -> " . " ++ arg) ++ ") ...)"

nil :: LispVal
nil = List []

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

pairs :: [a] -> [(a,a)]
pairs [ ]            = []
pairs [_]            = []
pairs (x : y : rest) = (x, y) : pairs rest

unpairs :: [(a,a)] -> [a]
unpairs [] = []
unpairs ((x,y) : rest) = x : y : unpairs rest

makeFunc :: (Monad m) => Maybe String -> Env -> [LispVal] -> [LispVal] -> m LispVal
makeFunc varargs env params body = return $ Func (map showVal params) varargs body env

makeNormalFunc :: (Monad m) => Env -> [LispVal] -> [LispVal] -> m LispVal
makeNormalFunc = makeFunc Nothing

makeVarArgs :: (Monad m) => LispVal -> Env -> [LispVal] -> [LispVal] -> m LispVal
makeVarArgs = makeFunc . Just . showVal

makeMacro :: (Monad m) => Maybe String -> Env -> [LispVal] -> [LispVal] -> m LispVal
makeMacro varargs env params body = return $ Macro (map showVal params) varargs body env

makeNormalMacro :: (Monad m) => Env -> [LispVal] -> [LispVal] -> m LispVal
makeNormalMacro = makeMacro Nothing

makeVarArgsMacro :: (Monad m) => LispVal -> Env -> [LispVal] -> [LispVal] -> m LispVal
makeVarArgsMacro = makeMacro . Just . showVal

-- Error handling

data LispError = NumArgs Integer [LispVal]
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | TypeMismatch String LispVal
               | UnboundVar String String
               | OutOfRange Int (Int, Int) LispVal
               | KeyNotFound LispVal LispVal
               | Default String

type ThrowsError = Either LispError

type IOThrowsError = ErrorT LispError IO

instance Show LispError where
    show = showError

instance Error LispError where
    noMsg  = Default "An error has occured"
    strMsg = Default

showError :: LispError -> String
showError (NumArgs expected found) = "Expected " ++ show expected
    ++ " args; found values " ++ unwordsList found
showError (Parser parseErr) = "Parse error at " ++ show parseErr
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
    ++ ", found " ++ show found
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (OutOfRange n bounds obj) = "Index " ++ show n ++ " out of range "
    ++ show bounds ++ " for object: " ++ show obj
showError (KeyNotFound key hash) = "Key " ++ show key ++ " not found in hash: " ++ show hash

trapError :: (MonadError e m, Show e) => m String -> m String
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err)  = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runErrorT (trapError action) >>= return . extractValue

-- These guys are here for debugging - it allows me to derive an instance for
-- LispVal that will show me the underlying Haskell representation rather than
-- the pretty-printed Haskeme version. For these to work correctly I need the
-- TypeSynonymInstances and FlexibleInstances pragmas. If we're not using this
-- debug capability then those pragmas don't need to be there.

instance Show Env where
    show _ = "<environment>"

instance Show ([LispVal] -> ThrowsError LispVal) where
    show _ = "<primitive>"

instance Show ([LispVal] -> IOThrowsError LispVal) where
    show _ = "<ioPrimitive>"
    
-- Equivalance of LispVals (for use as keys in a hash)

instance Eq LispVal where
    x == y = eqv x y

eqv :: LispVal -> LispVal -> Bool
eqv (Bool arg1) (Bool arg2) = arg1 == arg2
eqv (Atom arg1) (Atom arg2) = arg1 == arg2
eqv (Char arg1) (Char arg2) = arg1 == arg2
eqv (String arg1) (String arg2) = arg1 == arg2
eqv (Number arg1) (Number arg2) = arg1 == arg2
eqv (Ratio arg1) (Ratio arg2) = arg1 == arg2
eqv (Float arg1) (Float arg2) = arg1 == arg2
eqv (Complex arg1) (Complex arg2) = arg1 == arg2
eqv (Vector xs) (Vector ys) = eqv (List $ elems xs) (List $ elems ys)
eqv (DottedList xs x) (DottedList ys y) = eqv (List $ xs ++ [x]) (List $ ys ++ [y])
eqv (List xs) (List ys) = length xs == length ys && and (map (uncurry eqv) $ zip xs ys)
eqv _ _ = False

-- Ordering of LispVals (for use as keys in a hash)

data KeyType = AtomType | CharType | StringType | BoolType | NumberType
             | RatioType | FloatType | NotKey deriving (Eq,Ord)

keyType :: LispVal -> KeyType
keyType (Atom _)   = AtomType
keyType (Char _)   = CharType
keyType (String _) = StringType
keyType (Bool _)   = BoolType
keyType (Number _) = NumberType
keyType (Ratio _)  = RatioType
keyType (Float _)  = FloatType
keyType _          = NotKey

instance Ord LispVal where
    compare (Atom x)   (Atom y)   = compare x y
    compare (Char x)   (Char y)   = compare x y
    compare (String x) (String y) = compare x y
    compare (Bool x)   (Bool y)   = compare x y
    compare (Number x) (Number y) = compare x y
    compare (Ratio x)  (Ratio y)  = compare x y
    compare (Float x)  (Float y)  = compare x y
    compare arg1       arg2       = compare (keyType arg1) (keyType arg2)
