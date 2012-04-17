{-# LANGUAGE NoMonomorphismRestriction #-}

module LispVal (
      LispVal (Atom,List,DottedList,Number,Ratio,Float,Complex,Char,String,Bool,PrimitiveFunc,Func,IOFunc,Port)
    , LispError (NumArgs,Parser,BadSpecialForm,NotFunction,TypeMismatch,UnboundVar,Default)
    , ThrowsError
    , IOThrowsError
    , Env
    , showVal, unwordsList
    ) where

import IO
import Data.IORef
import Data.Complex
import Control.Monad.Error
import Text.ParserCombinators.Parsec (ParseError)

type Env = IORef [(String, IORef LispVal)]

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | Ratio Rational
             | Float Double
             | Complex (Complex Double)
             | Char Char
             | String String
             | Bool Bool
             | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
             | Func { params :: [String], vararg :: Maybe String
                    , body :: [LispVal], closure :: Env }
             | IOFunc ([LispVal] -> IOThrowsError LispVal)
             | Port Handle

instance Show LispVal where
    show = showVal

data LispError = NumArgs Integer [LispVal]
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | TypeMismatch String LispVal
               | UnboundVar String String
               | Default String

type ThrowsError = Either LispError

type IOThrowsError = ErrorT LispError IO

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Char char) = ['#','\\',char]
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Ratio contents) = show contents
showVal (Float contents) = show contents
showVal (Complex contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList hd tl) = "(" ++ unwordsList hd ++ " . " ++ showVal tl ++ ")"
showVal (PrimitiveFunc _) = "<primitive>"
showVal (Func { params = args, vararg = varargs, body = body, closure = env }) = 
    "(lambda (" ++ unwords args ++
        (case varargs of
            Nothing  -> ""
            Just arg -> " . " ++ arg) ++ ") ...)"
showVal (IOFunc _) = "<IO primitive>"
showVal (Port _) = "<IO port>"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal
