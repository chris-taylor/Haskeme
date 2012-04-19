{-# LANGUAGE NoMonomorphismRestriction, TypeSynonymInstances, FlexibleInstances #-}

module LispVal (
      LispVal (Atom,List,DottedList,Vector,Number,Ratio,Float,Complex,Char,String,Bool,PrimitiveFunc,IOFunc,Func,Macro,Port)
    , LispError (NumArgs,Parser,BadSpecialForm,NotFunction,TypeMismatch,UnboundVar,OutOfRange,Default)
    , ThrowsError
    , IOThrowsError
    , Env
    , showVal, unwordsList, makeNormalFunc, makeVarArgs, makeNormalMacro, makeVarArgsMacro
    ) where

import IO
import Data.IORef
import Data.Array
import Ratio
import Complex
import Control.Monad.Error
import Text.ParserCombinators.Parsec (ParseError)

type Env = IORef [(String, IORef LispVal)]

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Vector (Array Int LispVal)
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
showVal (Vector arr) = "#(" ++ unwordsList (elems arr) ++ ")"
showVal (PrimitiveFunc _) = "<primitive>"
showVal (IOFunc _) = "<IO primitive>"
showVal (Func { params = args, vararg = varargs }) = showFunc "lambda" args varargs
showVal (Macro { macroParams = args, macroVararg = varargs }) = showFunc "macro" args varargs
showVal (Port _) = "<IO port>"

showFunc :: String -> [String] -> Maybe String -> String
showFunc name args varargs = 
    "(" ++ name ++ " (" ++ unwords args ++
        (case varargs of
            Nothing  -> ""
            Just arg -> " . " ++ arg) ++ ") ...)"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

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
