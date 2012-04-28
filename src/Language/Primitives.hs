{-# LANGUAGE ExistentialQuantification #-}

module Language.Primitives (primitives) where

import Control.Monad.Error
import Data.Array
import qualified Data.Map as Map
import Ratio
import Complex

import Language.LispNum
import Language.LispVal

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = numericPrimitives ++ 
             [ ("symbol?", unaryBoolOp isSymbol)
             , ("pair?", unaryBoolOp isPair)
             , ("boolean?", unaryBoolOp isBool)
             , ("char?", unaryBoolOp isChar)
             , ("string?", unaryBoolOp isString)
             , ("number?", unaryBoolOp isNumber)
             , ("integer?", unaryBoolOp isInteger)
             , ("rational?", unaryBoolOp isRatio)
             , ("real?", unaryBoolOp isFloat)
             , ("complex?", unaryBoolOp isComplex)
             , ("vector?", unaryBoolOp isVector)
             , ("hash?", unaryBoolOp isHash)
             , ("procedure?", unaryBoolOp isProcedure)
             , ("macro?", unaryBoolOp isMacro)
             , ("port?", unaryBoolOp isPort)
             , ("type", typeOf)
             , ("&&", boolBoolBinop (&&))
             , ("||", boolBoolBinop (||))
             , ("string=?", strBoolBinop (==))
             , ("string<?", strBoolBinop (<))
             , ("string>?", strBoolBinop (>))
             , ("string<=?", strBoolBinop (<=))
             , ("string>=?", strBoolBinop (>=))
             , ("vector", vector)
             , ("hash", hash)
             , ("keys", unaryOp keys)
             , ("vals", unaryOp vals)
             , ("len", len)
             , ("string->list", typeTrans stringToList)
             , ("list->string", typeTrans listToString)
             , ("symbol->string", typeTrans symbolToString)
             , ("string->symbol", typeTrans stringToSymbol)
             , ("vector->list", typeTrans vectorToList)
             , ("list->vector", typeTrans listToVector)
             , ("car", car)
             , ("cdr", cdr)
             , ("cons", cons)
             , ("is", is)
             , ("iso", equal) ]

typeOf :: [LispVal] -> ThrowsError LispVal
typeOf xs = case xs of
    [ ] -> throwError $ NumArgs 1 xs
    [x] -> return $ t x
    xs  -> return $ List $ map t xs
    where
        t (Atom _) = Atom "symbol"
        t (List []) = Atom "nil"
        t (List _) = Atom "pair"
        t (DottedList _ _) = Atom "pair"
        t (Vector _) = Atom "vector"
        t (Hash _) = Atom "hash"
        t (Number _) = Atom "number"
        t (Ratio _) = Atom "number"
        t (Float _) = Atom "number"
        t (Complex _) = Atom "number"
        t (Char _) = Atom "char"
        t (String _) = Atom "string"
        t (Bool _) = Atom "boolean"
        t (PrimitiveFunc _ _) = Atom "procedure"
        t (IOFunc _ _) = Atom "procedure"
        t (Func {}) = Atom "procedure"
        t (Macro {}) = Atom "macro"
        t (Port _) = Atom "port"

unaryOp :: (LispVal -> ThrowsError LispVal) -> [LispVal] -> ThrowsError LispVal
unaryOp op [x]   = op x
unaryOp op other = throwError $ NumArgs 1 other

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2
    then throwError $ NumArgs 2 args
    else do left <- unpacker $ args !! 0
            right <- unpacker $ args !! 1
            return $ Bool $ left `op` right

numBoolBinop  = boolBinop unpackNum
strBoolBinop  = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s)   = return $ show s
unpackStr (Char s)   = return $ show s
unpackStr notString  = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool  = throwError $ TypeMismatch "boolean" notBool

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n)    = return n
unpackNum (Ratio n)     = if denominator n == 1
                            then return $ numerator n
                            else throwError $ TypeMismatch "number" $ Ratio n
unpackNum (Float n)     = if isIntegral n
                            then return $ round n
                            else throwError $ TypeMismatch "number" $ Float n
unpackNum (Complex n)   = if imagPart n == 0 && isIntegral (realPart n)
                            then return $ round (realPart n)
                            else throwError $ TypeMismatch "number" $ Complex n
unpackNum (String n)    = let parsed = reads n in
                            if null parsed
                                then throwError $ TypeMismatch "number" $ String n
                                else return $ fst $ parsed !! 0
unpackNum (List [n])    = unpackNum n
unpackNum notNum        = throwError $ TypeMismatch "number" notNum

isIntegral :: Double -> Bool
isIntegral x = fracPart == 0 where fracPart = x - fromIntegral (round x)

unpackRat :: LispVal -> ThrowsError Rational
unpackRat (Number n)  = return $ fromIntegral n
unpackRat (Ratio n)   = return n
unpackRat (Float n)   = return $ toRational n
unpackRat (Complex n) = if imagPart n == 0
                            then return $ toRational (realPart n)
                            else throwError $ TypeMismatch "ratio" $ Complex n
unpackRat (List [n])  = unpackRat n
unpackRat notRat      = throwError $ TypeMismatch "ratio" notRat

unpackFloat :: LispVal -> ThrowsError Double
unpackFloat (Number n)  = return $ fromIntegral n
unpackFloat (Ratio n)   = return $ fromRational n
unpackFloat (Float n)   = return n
unpackFloat (Complex n) = if imagPart n == 0
                            then return (realPart n)
                            else throwError $ TypeMismatch "float" $ Complex n
unpackFloat (String n)  = let parsed = reads n in
                            if null parsed
                                then throwError $ TypeMismatch "number" $ String n
                                else return $ fst $ parsed !! 0
unpackFloat (List [n])  = unpackFloat n
unpackFloat notFloat    = throwError $ TypeMismatch "float" notFloat

unpackCplx :: LispVal -> ThrowsError (Complex Double)
unpackCplx (Number n)   = return $ fromIntegral n
unpackCplx (Ratio n)    = return $ fromRational n
unpackCplx (Float n)    = return $ n :+ 0
unpackCplx (Complex n)  = return n
unpackCplx (List [n])   = unpackCplx n
unpackCplx notComplex   = throwError $ TypeMismatch "complex" notComplex

unaryBoolOp :: (LispVal -> Bool) -> [LispVal] -> ThrowsError LispVal
unaryBoolOp p [x] = return $ Bool (p x)
unaryBoolOp p xs  = throwError $ NumArgs 1 xs

isSymbol :: LispVal -> Bool
isSymbol (Atom _) = True
isSymbol _        = False

isPair :: LispVal -> Bool
isPair (List (x:_))          = True
isPair (DottedList (x:_) tl) = True
isPair _                     = False

isBool :: LispVal -> Bool
isBool (Bool _) = True
isBool _        = False

isChar :: LispVal -> Bool
isChar (Char _) = True
isChar _        = False

isString :: LispVal -> Bool
isString (String _) = True
isString _          = False

isNumber :: LispVal -> Bool
isNumber (Number _)  = True
isNumber (Ratio _)   = True
isNumber (Float _)   = True
isNumber (Complex _) = True
isNumber _           = False

isInteger :: LispVal -> Bool
isInteger (Number _) = True
isInteger _          = False

isRatio :: LispVal -> Bool
isRatio (Ratio _) = True
isRatio _         = False

isFloat :: LispVal -> Bool
isFloat (Float _) = True
isFloat _         = False

isComplex :: LispVal -> Bool
isComplex (Complex _) = True
isComplex _           = False

isVector :: LispVal -> Bool
isVector (Vector _) = True
isVector _          = False

isHash :: LispVal -> Bool
isHash (Hash _) = True
isHash _        = False

isProcedure :: LispVal -> Bool
isProcedure (PrimitiveFunc _ _) = True
isProcedure (IOFunc _ _)        = True
isProcedure (Func _ _ _ _)      = True
isProcedure _                   = False

isMacro :: LispVal -> Bool
isMacro (Macro _ _ _ _) = True
isMacro _               = False

isPort :: LispVal -> Bool
isPort (Port _) = True
isPort _        = False

typeTrans :: (LispVal -> ThrowsError LispVal) -> [LispVal] -> ThrowsError LispVal
typeTrans f [x]  = f x
typeTrans f args = throwError $ NumArgs 1 args

listToString :: LispVal -> ThrowsError LispVal
listToString (List xs) = liftM String $ stringify xs where
    stringify []              = return ""
    stringify (Char c : rest) = liftM (c:) $ stringify rest
    stringify (other : rest)  = throwError $ TypeMismatch "char" other
listToString notList   = throwError $ TypeMismatch "list" notList

stringToList :: LispVal -> ThrowsError LispVal
stringToList (String cs) = return $ List (map Char cs)
stringToList notString   = throwError $ TypeMismatch "string" notString

symbolToString :: LispVal -> ThrowsError LispVal
symbolToString (Atom val) = return $ String val
symbolToString notAtom    = throwError $ TypeMismatch "symbol" notAtom

stringToSymbol :: LispVal -> ThrowsError LispVal
stringToSymbol (String val) = return $ Atom val
stringToSymbol notString    = throwError $ TypeMismatch "string" notString

vectorToList :: LispVal -> ThrowsError LispVal
vectorToList (Vector arr) = return $ List (elems arr)
vectorToList notVector    = throwError $ TypeMismatch "vector" notVector

listToVector :: LispVal -> ThrowsError LispVal
listToVector (List xs) = return $ Vector $ listArray (0, length xs - 1) xs
listToVector notList   = throwError $ TypeMismatch "list" notList

keys :: LispVal -> ThrowsError LispVal
keys (Hash hash) = return . List $ Map.keys hash
keys notHash     = throwError $ TypeMismatch "hash" notHash

vals :: LispVal -> ThrowsError LispVal
vals (Hash hash) = return . List $ Map.elems hash
vals notHash     = throwError $ TypeMismatch "hash" notHash

car :: [LispVal] -> ThrowsError LispVal
car [List (x:_)]          = return x
car [DottedList (x:_) _]  = return x
car [String (x:_)]        = return (Char x)
car [Vector arr]          = return (arr ! 0)
car [notList]             = throwError $ TypeMismatch "list" notList
car badArgs               = throwError $ NumArgs 2 badArgs

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x:xs)]           = return $ List xs
cdr [DottedList [_] x]      = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [String (x:xs)]         = return $ String xs
cdr [Vector arr]            = return $ Vector $ listArray (0, n-1) (drop 1 els) where
                                (_, n) = bounds arr
                                els    = elems arr
cdr [notList]               = throwError $ TypeMismatch "pair" notList
cdr badArgs                 = throwError $ NumArgs 1 badArgs

cons :: [LispVal] -> ThrowsError LispVal
cons [Char c, List []]     = return $ String [c]
cons [Char c, String s]    = return $ String (c:s)
cons [x, List xs]          = return $ List (x:xs)
cons [x, DottedList xs tl] = return $ DottedList (x:xs) tl
cons [x, String s]         = return $ List (x : map Char s)
cons [x, Vector arr]       = return $ Vector $ listArray (0, n+1) (x:els) where
                                (_, n) = bounds arr
                                els    = elems arr
cons [x, y]                = return $ DottedList [x] y
cons badArgs               = throwError $ NumArgs 2 badArgs

vector :: [LispVal] -> ThrowsError LispVal
vector xs = return $ Vector $ listArray (0, length xs - 1) xs

hash :: [LispVal] -> ThrowsError LispVal
hash xs = return $ Hash $ Map.fromList $ pairs xs

len :: [LispVal] -> ThrowsError LispVal
len [arg]   = len' arg >>= return . Number . fromIntegral
len badArgs = throwError $ NumArgs 1 badArgs

len' :: LispVal -> ThrowsError Int
len' (List xs)          = return $ length xs
len' (DottedList xs tl) = return $ 1 + length xs
len' (Vector arr)       = let (_, n) = bounds arr in return (n + 1)
len' (Hash hash)        = return $ length $ Map.keys hash
len' (String str)       = return $ length str
len' other = throwError $ TypeMismatch "pair, vector, hash or string" other

listEquals :: ([LispVal] -> ThrowsError LispVal) -> [LispVal] -> [LispVal] -> ThrowsError LispVal
listEquals eq arg1 arg2 = return $ Bool $ (length arg1 == length arg2) &&
    (all eqPair $ zip arg1 arg2) where
        eqPair (x1, x2) = case eq [x1, x2] of
            Left err -> False
            Right (Bool val) -> val

is :: [LispVal] -> ThrowsError LispVal
is [Bool arg1, Bool arg2] = return $ Bool $ arg1 == arg2
is [Atom arg1, Atom arg2] = return $ Bool $ arg1 == arg2
is [Char arg1, Char arg2] = return $ Bool $ arg1 == arg2
is [String arg1, String arg2]   = return $ Bool $ arg1 == arg2
is [Number arg1, Number arg2]   = return $ Bool $ arg1 == arg2
is [Ratio arg1, Ratio arg2]     = return $ Bool $ arg1 == arg2
is [Float arg1, Float arg2]     = return $ Bool $ arg1 == arg2
is [Complex arg1, Complex arg2] = return $ Bool $ arg1 == arg2
is [DottedList xs x, DottedList ys y] = listEquals is (xs++[x]) (ys++[y])
is [Vector xs, Vector ys]             = listEquals is (elems xs) (elems ys)
is [List xs, List ys]                 = listEquals is xs ys
is [_ , _] = return $ Bool False
is badArgs = throwError $ NumArgs 2 badArgs

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) = do
        unpacked1 <- unpacker arg1
        unpacked2 <- unpacker arg2
        return $ unpacked1 == unpacked2
    `catchError` (const $ return False)

equal :: [LispVal] -> ThrowsError LispVal
equal [DottedList xs x, DottedList ys y] = listEquals equal (xs++[x]) (ys++[y])
equal [Vector xs, Vector ys]             = listEquals equal (elems xs) (elems ys)
equal [List xs, List ys]                 = listEquals equal xs ys
equal [arg1, arg2] = do
    primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2)
                       [AnyUnpacker unpackNum, AnyUnpacker unpackRat,
                        AnyUnpacker unpackFloat, AnyUnpacker unpackCplx,
                        AnyUnpacker unpackStr, AnyUnpacker unpackBool]
    eqvEquals <- is [arg1, arg2]
    return $ Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgs = throwError $ NumArgs 2 badArgs
