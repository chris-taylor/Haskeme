{-# LANGUAGE NoMonomorphismRestriction, TypeSynonymInstances, FlexibleInstances #-}

module LispVal (
      LispVal (Atom,List,DottedList,Vector,Number,Ratio,Float,Complex,Char,String,Bool,PrimitiveFunc,Func,IOFunc,Port,Macro)
    , LispError (NumArgs,Parser,BadSpecialForm,NotFunction,TypeMismatch,UnboundVar,OutOfRange,Default)
    , ThrowsError
    , IOThrowsError
    , Env
    , showVal, unwordsList
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
             | Func { params :: [String], vararg :: Maybe String
                    , body :: [LispVal], closure :: Env }
             | IOFunc ([LispVal] -> IOThrowsError LispVal)
             | Port Handle
             | Macro { macroParams :: [String], macroVararg :: Maybe String
                     , macroBody :: [LispVal], macroClosure :: Env }

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
showVal (Func { params = args, vararg = varargs, body = body, closure = env }) = 
    "(lambda (" ++ unwords args ++
        (case varargs of
            Nothing  -> ""
            Just arg -> " . " ++ arg) ++ ") ...)"
showVal (IOFunc _) = "<IO primitive>"
showVal (Port _) = "<IO port>"
showVal (Macro {}) = "<macro>"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

-- These guys are here for debugging - it allows me to derive an instance for LispVal that will show me the underlying Haskell representation rather than the pretty-printed Haskeme version. For these to work correctly I need the TypeSynonymInstances and FlexibleInstances pragmas. If we're not using this debug capability then those pragmas don't need to be there.

instance Show Env where
  show _ = "<environment>"

instance Show ([LispVal] -> ThrowsError LispVal) where
  show _ = "<primitive>"

instance Show ([LispVal] -> IOThrowsError LispVal) where
  show _ = "<ioPrimitive>"
