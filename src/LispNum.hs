{-# LANGUAGE ExistentialQuantification, Rank2Types #-}

module LispNum (numericPrimitives) where

import Control.Monad.Error
import Control.Applicative
import Data.Either
import Ratio
import Complex

import LispVal
import LispError

data NumType = IntType | RatioType | FloatType | ComplexType | NotANumber deriving (Eq,Ord)

typeOf :: LispVal -> NumType
typeOf (Number _)  = IntType
typeOf (Ratio _)   = RatioType
typeOf (Float _)   = FloatType
typeOf (Complex _) = ComplexType
typeOf _           = NotANumber

asComplex :: LispVal -> Complex Double
asComplex (Number n)  = fromInteger n
asComplex (Ratio n)   = fromRational n
asComplex (Float n)   = n :+ 0
asComplex (Complex n) = n

asFloat :: LispVal -> Double
asFloat (Number n) = fromInteger n
asFloat (Ratio n)  = fromRational n
asFloat (Float n)  = n

asRatio :: LispVal -> Rational
asRatio (Number n) = fromInteger n
asRatio (Ratio n)  = n

asNumber :: LispVal -> Integer
asNumber (Number n) = n

numericPrimitives :: [(String, [LispVal] -> ThrowsError LispVal)]
numericPrimitives =
    [ ("==", numericOrdOp (==))
    , ("<", numericOrdOp (<))
    , (">", numericOrdOp (>))
    , ("<=", numericOrdOp (<=))
    , (">=", numericOrdOp (>=))
    , ("+", numericBinOp (+))
    , ("-", numericBinOp (-))
    , ("*", numericBinOp (*))
    , ("/", fractionalBinOp (/))
    , ("sin", floatingUnOp sin)
    , ("cos", floatingUnOp cos)
    , ("tan", floatingUnOp tan)
    , ("asin", floatingUnOp asin)
    , ("acos", floatingUnOp acos)
    , ("atan", floatingUnOp atan)
    , ("cosh", floatingUnOp cosh)
    , ("sinh", floatingUnOp sinh)
    , ("tanh", floatingUnOp tanh)
    , ("acosh", floatingUnOp acosh)
    , ("asinh", floatingUnOp asinh)
    , ("atanh", floatingUnOp atanh)
    , ("exp", floatingUnOp exp)
    , ("log", floatingUnOp log)
    , ("sqrt", floatingUnOp sqrt)
    , ("expt", floatingBinOp (**))
    , ("div", integralBinOp div)
    , ("mod", integralBinOp mod)
    , ("quotient", integralBinOp quot)
    , ("remainder", integralBinOp rem) ]

foldLeftError :: (b -> a -> ThrowsError b) -> b -> [a] -> ThrowsError b
foldLeftError op accum xs = if length xs == 0
    then return accum
    else do
        accumulate <- accum `op` head xs
        foldLeftError op accumulate (tail xs)

foldLeft1Error :: (LispVal -> LispVal -> ThrowsError LispVal) -> [LispVal] -> ThrowsError LispVal
foldLeft1Error op xs = if length xs == 0
    then throwError $ NumArgs 1 xs
    else foldLeftError op (head xs) (tail xs)

numericBinOp :: (forall a. Num a => a -> a -> a) -> [LispVal] -> ThrowsError LispVal
numericBinOp op params = foldLeft1Error (promoteNumericBinaryOp op) params

numericOrdOp :: (forall a. (Ord a, Num a) => a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
numericOrdOp op params = case params of
    [x]            -> throwError $ NumArgs 2 [x]
    [x, y]         -> x `newOp` y
    (x : y : rest) -> do res@(Bool result) <- x `newOp` y
                         if result
                            then numericOrdOp op (y : rest)
                            else return res
    where newOp = promoteNumericOrdOp op

integralBinOp :: (forall a. Integral a => a -> a -> a) -> [LispVal] -> ThrowsError LispVal
integralBinOp op params = foldLeft1Error (promoteIntegralBinaryOp op) params

fractionalBinOp :: (forall a. Fractional a => a -> a -> a) -> [LispVal] -> ThrowsError LispVal
fractionalBinOp op params = foldLeft1Error (promoteFractionalBinaryOp op) params

floatingBinOp :: (forall a. Floating a => a -> a -> a) -> [LispVal] -> ThrowsError LispVal
floatingBinOp op args = foldLeft1Error (promoteFloatingBinaryOp op) args

floatingUnOp :: (forall a. Floating a => a -> a) -> [LispVal] -> ThrowsError LispVal
floatingUnOp op [arg] = promoteFloatingUnaryOp op arg
floatingUnOp _  args  = throwError $ NumArgs 1 args

promoteNumericBinaryOp :: (forall a. Num a => a -> a -> a) -> LispVal -> LispVal -> ThrowsError LispVal
promoteNumericBinaryOp op x y = case typeOf x `max` typeOf y of
    ComplexType -> return $ Complex (asComplex x `op` asComplex y)
    FloatType   -> return $ Float (asFloat x `op` asFloat y)
    RatioType   -> return $ Ratio (asRatio x `op` asRatio y)
    IntType     -> return $ Number (asNumber x `op` asNumber y)
    _           -> throwError $ TypeMismatch "number" x

promoteNumericOrdOp :: (forall a. (Num a, Ord a) => a -> a -> Bool) -> LispVal -> LispVal -> ThrowsError LispVal
promoteNumericOrdOp op x y = case typeOf x `max` typeOf y of
    FloatType -> return $ Bool (asFloat x `op` asFloat y)
    RatioType -> return $ Bool (asRatio x `op` asRatio y)
    IntType   -> return $ Bool (asNumber x `op` asNumber y)
    _         -> throwError $ TypeMismatch "int, rational, float" x

promoteIntegralBinaryOp :: (forall a. Integral a => a -> a -> a) -> LispVal -> LispVal -> ThrowsError LispVal
promoteIntegralBinaryOp op x y = case (typeOf x, typeOf y) of
    (IntType, IntType) -> return $ Number (asNumber x `op` asNumber y)
    (IntType, _)       -> throwError $ TypeMismatch "integer" x
    (_, _)             -> throwError $ TypeMismatch "integer" y

promoteFractionalBinaryOp :: (forall a. Fractional a => a -> a -> a) -> LispVal -> LispVal -> ThrowsError LispVal
promoteFractionalBinaryOp op x y = case typeOf x `max` typeOf y of
    NotANumber  -> throwError $ TypeMismatch "number" x
    ComplexType -> return $ Complex (asComplex x `op` asComplex y)
    FloatType   -> return $ Float (asFloat x `op` asFloat y)
    _           -> return $ Ratio (asRatio x `op` asRatio y)

promoteFloatingUnaryOp :: (forall a. Floating a => a -> a) -> LispVal -> ThrowsError LispVal
promoteFloatingUnaryOp op x = case typeOf x of
    NotANumber  -> throwError $ TypeMismatch "number" x
    ComplexType -> return $ Complex (op $ asComplex x)
    _           -> return $ Float (op $ asFloat x)

promoteFloatingBinaryOp :: (forall a. Floating a => a -> a -> a) -> LispVal -> LispVal -> ThrowsError LispVal
promoteFloatingBinaryOp op x y = case typeOf x `max` typeOf y of
    NotANumber  -> throwError $ TypeMismatch "number" x
    ComplexType -> return $ Complex (asComplex x `op` asComplex y)
    _           -> return $ Float (asFloat x `op` asFloat y)
