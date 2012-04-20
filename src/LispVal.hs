{-# LANGUAGE NoMonomorphismRestriction, TypeSynonymInstances, FlexibleInstances #-}

module LispVal (
      LispVal (Atom,List,DottedList,Vector,Hash,Number,Ratio,Float,Complex,Char,String,Bool,PrimitiveFunc,IOFunc,Func,Macro,Port)
    , VectorType
    , HashType
    , LispError (NumArgs,Parser,BadSpecialForm,NotFunction,TypeMismatch,UnboundVar,OutOfRange,KeyNotFound,Default)
    , ThrowsError
    , IOThrowsError
    , Env
    , showVal, nil, eqv, unwordsList
    , makeNormalFunc, makeVarArgs, makeNormalMacro, makeVarArgsMacro
    ) where

import IO
import Data.IORef
import Data.Array
import qualified Data.Map as Map
import Ratio
import Complex
import Control.Monad.Error
import Text.ParserCombinators.Parsec (ParseError)

type Env = IORef [(String, IORef LispVal)]

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
             | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
             | IOFunc ([LispVal] -> IOThrowsError LispVal)
             | Func { params :: [String], vararg :: Maybe String
                    , body :: [LispVal], closure :: Env }
             | Macro { macroParams :: [String], macroVararg :: Maybe String
                     , macroBody :: [LispVal], macroClosure :: Env }
             | Port Handle

instance Show LispVal where
    show = showVal

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
showVal (PrimitiveFunc _) = "<primitive>"
showVal (IOFunc _) = "<IO primitive>"
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

-- These guys are here for debugging - it allows me to derive an instance for LispVal that will show me the underlying Haskell representation rather than the pretty-printed Haskeme version. For these to work correctly I need the TypeSynonymInstances and FlexibleInstances pragmas. If we're not using this debug capability then those pragmas don't need to be there.

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
