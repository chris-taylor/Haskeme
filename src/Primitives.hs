{-# LANGUAGE ExistentialQuantification #-}

module Primitives (primitives) where

import Control.Monad.Error
import Data.Array
import Ratio
import Complex

import LispVal
import LispError
import LispNum

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = numericPrimitives ++ 
             [ ("symbol?", unaryBoolOp isSymbol)
             , ("pair?", unaryBoolOp isPair)
             , ("boolean?", unaryBoolOp isBool)
             , ("char?", unaryBoolOp isChar)
             , ("number?", unaryBoolOp isNumber)
             , ("integer?", unaryBoolOp isInteger)
             , ("ratio?", unaryBoolOp isRatio)
             , ("float?", unaryBoolOp isFloat)
             , ("complex?", unaryBoolOp isComplex)
             , ("string?", unaryBoolOp isString)
             , ("procedure?", unaryBoolOp isProcedure)
             , ("port?", unaryBoolOp isPort)
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
             , ("vector", vector)
             , ("symbol->string", typeTrans symbolToString)
             , ("string->symbol", typeTrans stringToSymbol)
             , ("vector->list", typeTrans vectorToList)
             , ("list->vector", typeTrans listToVector)
             , ("car", car)
             , ("cdr", cdr)
             , ("cons", cons)
             , ("eqv?", eqv)
             , ("eq?", eqv)
             , ("equal?", equal) ]

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

isBool :: LispVal -> Bool
isBool (Bool _) = True
isBool _        = False

isChar :: LispVal -> Bool
isChar (Char _) = True
isChar _        = False

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

isString :: LispVal -> Bool
isString (String _) = True
isString _          = False

isPair :: LispVal -> Bool
isPair (List (x:_))          = True
isPair (DottedList (x:_) tl) = True
isPair _                     = False

isProcedure :: LispVal -> Bool
isProcedure (PrimitiveFunc _) = True
isProcedure (IOFunc _)        = True
isProcedure (Func _ _ _ _)    = True
isProcedure _                 = False

isPort :: LispVal -> Bool
isPort (Port _) = True
isPort _        = False

typeTrans :: (LispVal -> ThrowsError LispVal) -> [LispVal] -> ThrowsError LispVal
typeTrans f [x]  = f x
typeTrans f args = throwError $ NumArgs 1 args

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

vector :: [LispVal] -> ThrowsError LispVal
vector xs = return $ Vector $ listArray (0, length xs - 1) xs

listEquals :: ([LispVal] -> ThrowsError LispVal) -> [LispVal] -> [LispVal] -> ThrowsError LispVal
listEquals eq arg1 arg2 = return $ Bool $ (length arg1 == length arg2) &&
    (all eqPair $ zip arg1 arg2) where
        eqPair (x1, x2) = case eq [x1, x2] of
            Left err -> False
            Right (Bool val) -> val

eqv :: [LispVal] -> ThrowsError LispVal
eqv [Bool arg1, Bool arg2] = return $ Bool $ arg1 == arg2
eqv [Atom arg1, Atom arg2] = return $ Bool $ arg1 == arg2
eqv [Char arg1, Char arg2] = return $ Bool $ arg1 == arg2
eqv [String arg1, String arg2]   = return $ Bool $ arg1 == arg2
eqv [Number arg1, Number arg2]   = return $ Bool $ arg1 == arg2
eqv [Ratio arg1, Ratio arg2]     = return $ Bool $ arg1 == arg2
eqv [Float arg1, Float arg2]     = return $ Bool $ arg1 == arg2
eqv [Complex arg1, Complex arg2] = return $ Bool $ arg1 == arg2
eqv [DottedList xs x, DottedList ys y] = listEquals eqv (xs++[x]) (ys++[y])
eqv [Vector xs, Vector ys]             = listEquals eqv (elems xs) (elems ys)
eqv [List xs, List ys]                 = listEquals eqv xs ys
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
equal [DottedList xs x, DottedList ys y] = listEquals equal (xs++[x]) (ys++[y])
equal [Vector xs, Vector ys]             = listEquals equal (elems xs) (elems ys)
equal [List xs, List ys]                 = listEquals equal xs ys
equal [arg1, arg2] = do
    primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2)
                       [AnyUnpacker unpackNum, AnyUnpacker unpackRat,
                        AnyUnpacker unpackFloat, AnyUnpacker unpackCplx,
                        AnyUnpacker unpackStr, AnyUnpacker unpackBool]
    eqvEquals <- eqv [arg1, arg2]
    return $ Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgs = throwError $ NumArgs 2 badArgs
