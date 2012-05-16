{-# LANGUAGE NoMonomorphismRestriction, TypeSynonymInstances,
    FlexibleInstances, FlexibleContexts, MultiParamTypeClasses,
    FunctionalDependencies #-}

module Language.Types (
      LispVal (..)
    , LispError (..)
    , VectorType, HashType
    , Env (..), EnvType, Namespace, Var
    , ThrowsError, IOThrowsError
    , EvalM
    , errTypeMismatch, errNumArgs, errUser
    , unwordsList, pairs, unpairs
    , showVal, nil, eqv, truthVal, lispFalse, typeName, errorName, nullEnv
    , showError, trapError, extractValue, liftThrows, runIOThrows, runIOThrowsCompile
    ) where

import IO
import Data.IORef
import Data.Array
import qualified Data.Map as Map
import Ratio
import Complex
import Control.Monad.Error
import Text.ParserCombinators.Parsec (ParseError)

import Control.Monad.State

type Namespace = String
type Var = String

type EnvType = Map.Map (Namespace, Var) (IORef LispVal)

data Env = Environment { parent :: Maybe Env
                       , bindings :: IORef EnvType }

--- Monad Transformer stuff (experimental)

--newtype EnvT e m a = EnvT { runEnvT :: e -> m a }

--instance Monad m => Monad (EnvT e m) where
--    return a = EnvT $ \_ -> return a
--    m >>= f  = EnvT $ \e -> do
--        a <- runEnvT m e
--        runEnvT (f a) e

--instance MonadTrans (EnvT e) where
--    lift m = EnvT $ \_ -> m

--class Monad m => MonadEnv e m | m -> e where
--    rdEnv :: m e
--    inEnv :: (e -> e) -> m a -> m a

--instance Monad m => MonadEnv e (EnvT e m) where
--    rdEnv     = EnvT $ return
--    inEnv f m = EnvT $ \e -> runEnvT m (f e)

--type EvalM = EnvT Env IOThrowsError

-- StateT transformer stuff -- EXPERIMENTAL

type EvalM = StateT Env IOThrowsError

---

nullEnv :: IO Env
nullEnv = do
    nullBindings <- newIORef Map.empty
    return $ Environment Nothing nullBindings

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
             | Func { params :: [String]
                    , vararg :: Maybe String
                    , body :: [LispVal]
                    , closure :: Env }
             | HFunc { hparams :: [String]
                     , hvararg :: Maybe String
                     , hbody :: (Env -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal)
                     , hclosure :: Env }
             | Port Handle
             | Exception LispError
             | Nil

instance Show LispVal where
    show = showVal

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Char char) = ['#','\\',char]
showVal (Atom name) = name
showVal (Number num) = show num
showVal (Ratio num) = show (numerator num) ++ "/" ++ show (denominator num)
showVal (Float num) = show num
showVal (Complex num) = let re = realPart num
                            im = imagPart num
                        in show re ++ (case signum im of
                            1    -> "+"
                            0    -> if isNegativeZero im then "-" else "+"
                            (-1) -> "-") ++ show (myabs im) ++ "i"
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList hd tl) = "(" ++ unwordsList hd ++ " . " ++ showVal tl ++ ")"
showVal (Vector arr) = "$(" ++ unwordsList (elems arr) ++ ")"
showVal (Hash hash) = "#(" ++ unwordsList (unpairs $ zip (Map.keys hash) (Map.elems hash)) ++ ")"
showVal (PrimitiveFunc name _) = "<primitive:" ++ name ++ ">"
showVal (IOFunc name _) = "<ioPrimitive:" ++ name ++ ">"
showVal (HFunc _ _ _ _) = "<haskellFunction>"
showVal (Func { params = args, vararg = varargs }) = showFunc "fn" args varargs
showVal (Port _) = "<IO port>"
showVal (Exception e) = "<exception:" ++ errorName e ++ ">"
showval (Nil) = "<nil>"

-- This is required because Haskell evaluates abs (-0.0) to -0.0, which messes
-- up the printing of complex values then the imaginary part is negative zero.
myabs :: (Ord a, Num a) => a -> a
myabs x | x > 0  = x
        | x == 0 = 0
        | x < 0  = -x

showFunc :: String -> [String] -> Maybe String -> String
showFunc name args varargs = "(" ++ name ++ " " ++
    (case args of
        [] -> (case varargs of
            Nothing -> "()"
            Just arg -> arg)
        _  -> "(" ++ unwords args ++ (case varargs of
            Nothing  -> ""
            Just arg -> " . " ++ arg) ++ ")"
    ) ++ " ...)"

-- Names for each of the types

typeName :: LispVal -> String
typeName (Atom _)            = "symbol"
typeName (List [])           = "nil"
typeName (List _)            = "pair"
typeName (DottedList _ _)    = "pair"
typeName (Vector _)          = "vector"
typeName (Hash _)            = "hash"
typeName (Number _)          = "number"
typeName (Ratio _)           = "number"
typeName (Float _)           = "number"
typeName (Complex _)         = "number"
typeName (Char _)            = "char"
typeName (String _)          = "string"
typeName (Bool _)            = "boolean"
typeName (PrimitiveFunc _ _) = "procedure"
typeName (IOFunc _ _)        = "procedure"
typeName (Func {})           = "procedure"
typeName (Exception _)       = "exception"
typeName (Port _)            = "port"

-- Helper functions

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

-- Error handling

data LispError = NumArgs Integer [LispVal]
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | TypeMismatch String LispVal
               | UnboundVar String String
               | OutOfRange Int (Int, Int) LispVal
               | KeyNotFound LispVal LispVal
               | FileNotFound String
               | UserError String [LispVal]
               | Default String

type ThrowsError = Either LispError

type IOThrowsError = ErrorT LispError IO

instance Show LispError where
    show = showError

instance Error LispError where
    noMsg  = Default "An internal error has occured"
    strMsg = Default

errNumArgs :: MonadError LispError m => Integer -> [LispVal] -> m LispVal
errNumArgs num args = throwError $ NumArgs num args

errTypeMismatch :: MonadError LispError m => String -> LispVal -> m LispVal
errTypeMismatch expected found = throwError $ TypeMismatch expected found

errUser :: MonadError LispError m => String -> [LispVal] -> m LispVal
errUser name args = throwError $ UserError name args

errorName :: LispError -> String
errorName (NumArgs _ _)         = "numargs"
errorName (Parser _)            = "parseerror"
errorName (BadSpecialForm _ _)  = "badspecialform"
errorName (NotFunction _ _)     = "notfunction"
errorName (TypeMismatch _ _)    = "typemismatch"
errorName (UnboundVar _ _)      = "unboundvar"
errorName (OutOfRange _ _ _)    = "outofrange"
errorName (KeyNotFound _ _)     = "keynotfound"
errorName (FileNotFound _)      = "filenotfound"
errorName (UserError name _)    = name 

showError :: LispError -> String
showError (NumArgs expected found) = "NumArgs: expected " ++ show expected
    ++ " arg(s), found " ++ unwordsList found
showError (Parser parseErr) = "ParseError: " ++ show parseErr
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (TypeMismatch expected found) = "TypeMismatch: expected " ++ expected
    ++ ", found " ++ show found
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (OutOfRange n bounds obj) = "OutOfRange: index " ++ show n
    ++ " out of range " ++ show bounds ++ " for object " ++ show obj
showError (KeyNotFound key hash) = "KeyNotFound: key " ++ show key
    ++ " not found in hash " ++ show hash
showError (FileNotFound filename) = "FileNotFound: " ++ filename
showError (UserError name args) = name ++ ": " ++ unwordsList args
showError (Default msg) = "InternalError: " ++ msg

trapError :: (MonadError e m, Show e) => m String -> m String
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
extractValue (Left err)  = error "Unexpected error in extractValue"

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err)  = throwError err
liftThrows (Right val) = return val

-- Execute an IO action and return a result
-- Intended for use in contexts where a result is always required
runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runErrorT (trapError action) >>= return . extractValue

-- Execute an IO action, returning Nothing if no error is thrown
runIOThrowsCompile :: IOThrowsError String -> IO (Maybe String)
runIOThrowsCompile action = do
    runState <- runErrorT action
    case runState of
        Left err -> return $ Just (show err)
        Right _  -> return Nothing

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

-- Truth values (used in 'if special form)

lispFalse :: LispVal
lispFalse = List [Atom "quote", List []]

truthVal :: LispVal -> Bool
truthVal (Bool False) = False
truthVal (Number 0)   = False
truthVal (Ratio 0)    = False
truthVal (Float 0)    = False
truthVal (Complex 0)  = False
truthVal (String "")  = False
truthVal (List [])    = False
truthVal (Vector arr) = let (_, n) = bounds arr in n > 0
truthVal _            = True
    
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
