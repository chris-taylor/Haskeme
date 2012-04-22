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

numericBinOp :: (forall a. Num a => a -> a -> a) -> [LispVal] -> ThrowsError LispVal
numericBinOp op params = return $ foldl1 (promoteNumericBinaryOp op) params

numericOrdOp :: (forall a. (Ord a, Num a) => a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
numericOrdOp op params = case params of
    [x]            -> throwError $ NumArgs 2 [x]
    [x, y]         -> return $ x `o` y
    (x : y : rest) -> let res@(Bool result) = x `o` y in
                        if result
                            then numericOrdOp op (y : rest)
                            else return res
    where o = promoteNumericOrdOp op

integralBinOp :: (forall a. Integral a => a -> a -> a) -> [LispVal] -> ThrowsError LispVal
integralBinOp op params = return $ foldl1 (promoteIntegralBinaryOp op) params

fractionalBinOp :: (forall a. Fractional a => a -> a -> a) -> [LispVal] -> ThrowsError LispVal
fractionalBinOp op params = return $ foldl1 (promoteFractionalBinaryOp op) params

floatingUnOp :: (forall a. Floating a => a -> a) -> [LispVal] -> ThrowsError LispVal
floatingUnOp op [arg] = return $ (promoteFloatingUnaryOp op) arg
floatingUnOp _  args  = throwError $ NumArgs 1 args

floatingBinOp :: (forall a. Floating a => a -> a -> a) -> [LispVal] -> ThrowsError LispVal
floatingBinOp op args = return $ foldl1 (promoteFloatingBinaryOp op) args

promoteNumericBinaryOp :: (forall a. Num a => a -> a -> a) -> LispVal -> LispVal -> LispVal
promoteNumericBinaryOp op x y = case typeOf x `max` typeOf y of
    ComplexType -> Complex (asComplex x `op` asComplex y)
    FloatType   -> Float (asFloat x `op` asFloat y)
    RatioType   -> Ratio (asRatio x `op` asRatio y)
    IntType     -> Number (asNumber x `op` asNumber y)

promoteNumericOrdOp :: (forall a. (Num a, Ord a) => a -> a -> Bool) -> LispVal -> LispVal -> LispVal
promoteNumericOrdOp op x y = case typeOf x `max` typeOf y of
    FloatType -> Bool (asFloat x `op` asFloat y)
    RatioType -> Bool (asRatio x `op` asRatio y)
    IntType   -> Bool (asNumber x `op` asNumber y)

promoteIntegralBinaryOp :: (forall a. Integral a => a -> a -> a) -> LispVal -> LispVal -> LispVal
promoteIntegralBinaryOp op x y = case typeOf x `max` typeOf y of
    IntType     -> Number (asNumber x `op` asNumber y)

promoteFractionalBinaryOp :: (forall a. Fractional a => a -> a -> a) -> LispVal -> LispVal -> LispVal
promoteFractionalBinaryOp op x y = case typeOf x `max` typeOf y of
    ComplexType -> Complex (asComplex x `op` asComplex y)
    FloatType   -> Float (asFloat x `op` asFloat y)
    _           -> Ratio (asRatio x `op` asRatio y)

promoteFloatingUnaryOp :: (forall a. Floating a => a -> a) -> LispVal -> LispVal
promoteFloatingUnaryOp op x = case typeOf x of
    ComplexType -> Complex (op $ asComplex x)
    _           -> Float (op $ asFloat x)

promoteFloatingBinaryOp :: (forall a. Floating a => a -> a -> a) -> LispVal -> LispVal -> LispVal
promoteFloatingBinaryOp op x y = case typeOf x `max` typeOf y of
    ComplexType -> Complex (asComplex x `op` asComplex y)
    _           -> Float (asFloat x `op` asFloat y)
