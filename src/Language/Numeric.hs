{-# LANGUAGE ExistentialQuantification, Rank2Types #-}

module Language.Numeric (numericPrimitives) where

import Control.Monad.Error
import Control.Applicative
import Data.Either
import Ratio
import Complex

import Language.Types

data NumType = IntType | RatioType | FloatType | ComplexType | NotANumber
    deriving (Eq,Ord)

numType :: LispVal -> NumType
numType (Number _)  = IntType
numType (Ratio _)   = RatioType
numType (Float _)   = FloatType
numType (Complex _) = ComplexType
numType _           = NotANumber

numericPrimitives :: [(String, [LispVal] -> ThrowsError LispVal)]
numericPrimitives =
    [ ("integer?", numericBoolOp isInteger)
    , ("rational?", numericBoolOp isRatio)
    , ("real?", numericBoolOp isFloat)
    , ("complex?", numericBoolOp isComplex)
    , ("exact?", numericBoolOp isExact)
    , ("inexact?", numericBoolOp isInexact)
    , ("==", numericEqOp (==))
    , ("<", numericOrdOp (<))
    , (">", numericOrdOp (>))
    , ("<=", numericOrdOp (<=))
    , (">=", numericOrdOp (>=))
    , ("+", numericBinOp (+))
    , ("-", numericMinus)
    , ("*", numericBinOp (*))
    , ("/", fractionalBinOp (/))
    , ("^", numericIntegralBinOp (^))
    , ("abs", numericUnOp abs)
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
    , ("**", floatingBinOp (**))
    , ("div", integralBinOp div)
    , ("mod", integralBinOp mod)
    , ("quot", integralBinOp quot)
    , ("rem", integralBinOp rem)
    , ("gcd", integralBinOp gcd)
    , ("lcm", integralBinOp lcm)
    , ("as-integer", numericCast toNumber)
    , ("as-rational", numericCast toRatio)
    , ("as-real", numericCast toFloat)
    , ("as-complex", numericCast toComplex)
    , ("as-exact", numericCast toExact)
    , ("as-inexact", numericCast toInexact) ]

-- Coerce Haskell base types. Used for type promotion - never throw away
-- information.

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

_pushDown :: LispVal -> LispVal
_pushDown num@(Complex z) = if imagPart z == 0
    then Float $ realPart z
    else num
_pushDown num@(Ratio x)   = if denominator x == 1
    then Number $ numerator x
    else num
_pushDown num@(Float _)   = num
_pushDown num@(Number _)  = num

pushDown :: LispVal -> ThrowsError LispVal
pushDown x = case numType x of
    NotANumber -> throwError $ TypeMismatch "number" x
    _          -> return $ _pushDown x

-- Forced coercion within Lisp. These functions may throw away information.
-- Input comes via the function numericCast, which checks for type mismatches,
-- so we can assume that the input is always a numeric type.

toComplex :: LispVal -> LispVal
toComplex = Complex . asComplex

toFloat :: LispVal -> LispVal
toFloat (Complex n) = Float $ realPart n
toFloat x           = Float $ asFloat x

toRatio :: LispVal -> LispVal
toRatio (Complex n) = Ratio $ toRational $ realPart n
toRatio (Float n)   = Ratio $ toRational n
toRatio x           = Ratio $ asRatio x

toNumber :: LispVal -> LispVal
toNumber (Complex n)    = Number $ round $ realPart n
toNumber (Float n)      = Number $ round n
toNumber (Ratio n)      = Number $ round $ (fromInteger $ numerator n) / (fromInteger $ denominator n)
toNumber num@(Number n) = num

toExact :: LispVal -> LispVal
toExact n = if isExact n then n else _pushDown (toRatio n)

toInexact :: LispVal -> LispVal
toInexact n = if isExact n then toFloat n else n

numericCast :: (LispVal -> LispVal) -> [LispVal] -> ThrowsError LispVal
numericCast cast xs  = case xs of
    [ ] -> throwError $ NumArgs 1 xs
    [x] -> promote cast x
    _   -> liftM List $ mapM (promote cast) xs
    where
        promote cast x = case numType x of
            NotANumber -> throwError $ TypeMismatch "number" x
            _          -> return (cast x)

-- Type checking. Input to these functions is passed via numericBoolOp,
-- which checks for type mismatches, so we can always assume that we have
-- a numeric type.

isInteger :: LispVal -> Bool
isInteger (Number _) = True
isInteger _          = False

isRatio :: LispVal -> Bool
isRatio (Number _) = True
isRatio (Ratio _)  = True
isRatio _          = False

isFloat :: LispVal -> Bool
isFloat (Number _) = True
isFloat (Ratio _)  = True
isFloat (Float _)  = True
isFloat _          = False

isComplex :: LispVal -> Bool
isComplex _ = True

isExact :: LispVal -> Bool
isExact (Number _)  = True
isExact (Ratio _)   = True
isExact (Float _)   = False
isExact (Complex _) = False

isInexact :: LispVal -> Bool
isInexact = not . isExact

-- Combinators

foldLeftError :: (b -> a -> ThrowsError b) -> [a] -> b -> ThrowsError b
foldLeftError op xs res = if length xs == 0
    then return res
    else res `op` head xs >>= foldLeftError op (tail xs)

foldLeft1Error :: (LispVal -> LispVal -> ThrowsError LispVal) -> [LispVal] -> ThrowsError LispVal
foldLeft1Error op xs = if length xs == 0
    then throwError $ NumArgs 1 xs
    else foldLeftError op (tail xs) (head xs)

numericMinus :: [LispVal] -> ThrowsError LispVal
numericMinus [x] = numericUnOp negate [x]
numericMinus xs  = numericBinOp (-) xs

numericBoolOp :: (LispVal -> Bool) -> [LispVal] -> ThrowsError LispVal
numericBoolOp p [x] = if numType x == NotANumber
    then throwError $ TypeMismatch "number" x
    else return $ Bool (p x)
numericBoolOp p xs  = throwError $ NumArgs 1 xs

numericBinOp :: (forall a. Num a => a -> a -> a) -> [LispVal] -> ThrowsError LispVal
numericBinOp op params = foldLeft1Error (promoteNumericBinaryOp op) params >>= pushDown

numericIntegralBinOp :: (forall a b. (Num a, Integral b) => a -> b -> a) -> [LispVal] -> ThrowsError LispVal
numericIntegralBinOp op params = foldLeft1Error (promoteNumericIntegralBinaryOp op) params >>= pushDown

numericUnOp :: (forall a. Num a => a -> a) -> [LispVal] -> ThrowsError LispVal
numericUnOp op [arg] = promoteNumericUnaryOp op arg >>= pushDown
numericUnOp op args  = throwError $ NumArgs 1 args

numericEqOp :: (forall a. (Eq a, Num a) => a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
numericEqOp op params = case params of
    [x]            -> throwError $ NumArgs 2 [x]
    [x, y]         -> x `newOp` y
    (x : y : rest) -> do res@(Bool result) <- x `newOp` y
                         if result
                            then numericEqOp op (y : rest)
                            else return res
    where newOp = promoteNumericEqOp op

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
fractionalBinOp op params = foldLeft1Error (promoteFractionalBinaryOp op) params >>= pushDown

floatingBinOp :: (forall a. Floating a => a -> a -> a) -> [LispVal] -> ThrowsError LispVal
floatingBinOp op args = foldLeft1Error (promoteFloatingBinaryOp op) args >>= pushDown

floatingUnOp :: (forall a. Floating a => a -> a) -> [LispVal] -> ThrowsError LispVal
floatingUnOp op [arg] = promoteFloatingUnaryOp op arg >>= pushDown
floatingUnOp _  args  = throwError $ NumArgs 1 args

promoteNumericBinaryOp :: (forall a. Num a => a -> a -> a) -> LispVal -> LispVal -> ThrowsError LispVal
promoteNumericBinaryOp op x y = case numType x `max` numType y of
    ComplexType -> return $ Complex (asComplex x `op` asComplex y)
    FloatType   -> return $ Float (asFloat x `op` asFloat y)
    RatioType   -> return $ Ratio (asRatio x `op` asRatio y)
    IntType     -> return $ Number (asNumber x `op` asNumber y)
    _           -> throwError $ TypeMismatch "number" y

promoteNumericIntegralBinaryOp :: (forall a b. (Num a, Integral b) => a -> b -> a) -> LispVal -> LispVal -> ThrowsError LispVal
promoteNumericIntegralBinaryOp op x y = case numType x of
    IntType     -> return $ Number (asNumber x `op` asNumber y)
    RatioType   -> return $ Ratio (asRatio x `op` asNumber y)
    FloatType   -> return $ Float (asFloat x `op` asNumber y)
    ComplexType -> return $ Complex (asComplex x `op` asNumber y)

promoteNumericUnaryOp :: (forall a. Num a => a -> a) -> LispVal -> ThrowsError LispVal
promoteNumericUnaryOp op x = case numType x of
    IntType     -> return $ Number (op $ asNumber x)
    RatioType   -> return $ Ratio (op $ asRatio x)
    FloatType   -> return $ Float (op $ asFloat x)
    ComplexType -> return $ Complex (op $ asComplex x)
    NotANumber  -> throwError $ TypeMismatch "number" x

promoteNumericEqOp :: (forall a. (Num a, Eq a) => a -> a -> Bool) -> LispVal -> LispVal -> ThrowsError LispVal
promoteNumericEqOp op x y = case numType x `max` numType y of
    IntType     -> return $ Bool (asNumber x `op` asNumber y)
    RatioType   -> return $ Bool (asRatio x `op` asRatio y)
    FloatType   -> return $ Bool (asFloat x `op` asFloat y)
    ComplexType -> return $ Bool (asComplex x `op` asComplex y)
    _           -> throwError $ TypeMismatch "number" y

promoteNumericOrdOp :: (forall a. (Num a, Ord a) => a -> a -> Bool) -> LispVal -> LispVal -> ThrowsError LispVal
promoteNumericOrdOp op x y = case numType x `max` numType y of
    IntType   -> return $ Bool (asNumber x `op` asNumber y)
    RatioType -> return $ Bool (asRatio x `op` asRatio y)
    FloatType -> return $ Bool (asFloat x `op` asFloat y)
    _         -> throwError $ TypeMismatch "int, rational or float" y

promoteIntegralBinaryOp :: (forall a. Integral a => a -> a -> a) -> LispVal -> LispVal -> ThrowsError LispVal
promoteIntegralBinaryOp op x y = case (numType x, numType y) of
    (IntType, IntType) -> return $ Number (asNumber x `op` asNumber y)
    (IntType, _)       -> throwError $ TypeMismatch "integer" y
    (_, _)             -> throwError $ TypeMismatch "integer" x

promoteFractionalBinaryOp :: (forall a. Fractional a => a -> a -> a) -> LispVal -> LispVal -> ThrowsError LispVal
promoteFractionalBinaryOp op x y = case numType x `max` numType y of
    NotANumber  -> throwError $ TypeMismatch "number" y
    ComplexType -> return $ Complex (asComplex x `op` asComplex y)
    FloatType   -> return $ Float (asFloat x `op` asFloat y)
    _           -> return $ Ratio (asRatio x `op` asRatio y)

promoteFloatingUnaryOp :: (forall a. Floating a => a -> a) -> LispVal -> ThrowsError LispVal
promoteFloatingUnaryOp op x = case numType x of
    NotANumber  -> throwError $ TypeMismatch "number" x
    ComplexType -> return $ Complex (op $ asComplex x)
    _           -> return $ Float (op $ asFloat x)

promoteFloatingBinaryOp :: (forall a. Floating a => a -> a -> a) -> LispVal -> LispVal -> ThrowsError LispVal
promoteFloatingBinaryOp op x y = case numType x `max` numType y of
    NotANumber  -> throwError $ TypeMismatch "number" y
    ComplexType -> return $ Complex (asComplex x `op` asComplex y)
    _           -> return $ Float (asFloat x `op` asFloat y)
