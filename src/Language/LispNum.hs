{-# LANGUAGE ExistentialQuantification, Rank2Types #-}

module Language.LispNum (numericPrimitives) where

import Control.Monad.Error
import Control.Applicative
import Data.Either
import Ratio
import Complex

import Language.LispVal

data NumType = IntType | RatioType | FloatType | ComplexType | NotANumber
    deriving (Eq,Ord)

numType :: LispVal -> NumType
numType (Number _)  = IntType
numType (Ratio _)   = RatioType
numType (Float _)   = FloatType
numType (Complex _) = ComplexType
numType _           = NotANumber

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
toNumber (Complex n) = Number $ round $ realPart n
toNumber (Float n)   = Number $ round n
toNumber (Ratio n)   = Number $ round $ (fromInteger $ numerator n) / (fromInteger $ denominator n)
toNumber x           = Number $ asNumber x

numericPrimitives :: [(String, [LispVal] -> ThrowsError LispVal)]
numericPrimitives =
    [ ("==", numericOrdOp (==))
    , ("<", numericOrdOp (<))
    , (">", numericOrdOp (>))
    , ("<=", numericOrdOp (<=))
    , (">=", numericOrdOp (>=))
    , ("+", numericBinOp (+))
    , ("-", numericMinus)
    , ("*", numericBinOp (*))
    , ("/", fractionalBinOp (/))
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
    , ("expt", floatingBinOp (**))
    , ("div", integralBinOp div)
    , ("mod", integralBinOp mod)
    , ("quot", integralBinOp quot)
    , ("rem", integralBinOp rem)
    , ("as-integer", numericCast toNumber)
    , ("as-rational", numericCast toRatio)
    , ("as-real", numericCast toFloat)
    , ("as-complex", numericCast toComplex) ]

foldLeftError :: (b -> a -> ThrowsError b) -> [a] -> b -> ThrowsError b
foldLeftError op xs res = if length xs == 0
    then return res
    else res `op` head xs >>= foldLeftError op (tail xs)

foldLeft1Error :: (LispVal -> LispVal -> ThrowsError LispVal) -> [LispVal] -> ThrowsError LispVal
foldLeft1Error op xs = if length xs == 0
    then throwError $ NumArgs 1 xs
    else foldLeftError op (tail xs) (head xs)

numericCast :: (LispVal -> LispVal) -> [LispVal] -> ThrowsError LispVal
numericCast cast xs  = case xs of
    [ ] -> throwError $ NumArgs 1 xs
    [x] -> promote cast x
    _   -> liftM List $ mapM (promote cast) xs
    where
        promote cast x = case numType x of
            NotANumber -> throwError $ TypeMismatch "number" x
            _          -> return (cast x)

pushDown :: LispVal -> ThrowsError LispVal
pushDown num@(Complex z) = if imagPart z == 0
    then pushDown $ Float (realPart z)
    else return num
pushDown num@(Float x)   = if isFloatIntegral x
    then return $ Number $ round x
    else return num
pushDown num@(Ratio x)   = if isRatioIntegral x
    then return $ Number $ numerator x
    else return num
pushDown num@(Number n)  = return num
pushDown other           = throwError $ TypeMismatch "number" other

isFloatIntegral :: Double -> Bool
isFloatIntegral x = fracPart == 0 where fracPart = x - fromIntegral (round x)

isRatioIntegral :: Rational -> Bool
isRatioIntegral x = denominator x == 1

numericMinus :: [LispVal] -> ThrowsError LispVal
numericMinus [x] = numericUnOp negate [x]
numericMinus xs  = numericBinOp (-) xs

numericBinOp :: (forall a. Num a => a -> a -> a) -> [LispVal] -> ThrowsError LispVal
numericBinOp op params = foldLeft1Error (promoteNumericBinaryOp op) params >>= pushDown

numericUnOp :: (forall a. Num a => a -> a) -> [LispVal] -> ThrowsError LispVal
numericUnOp op [arg] = promoteNumericUnaryOp op arg >>= pushDown
numericUnOp op args  = throwError $ NumArgs 1 args

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

promoteNumericUnaryOp :: (forall a. Num a => a -> a) -> LispVal -> ThrowsError LispVal
promoteNumericUnaryOp op x = case numType x of
    IntType     -> return $ Number (op $ asNumber x)
    RatioType   -> return $ Ratio (op $ asRatio x)
    FloatType   -> return $ Float (op $ asFloat x)
    ComplexType -> return $ Complex (op $ asComplex x)
    NotANumber  -> throwError $ TypeMismatch "number" x

promoteNumericOrdOp :: (forall a. (Num a, Ord a) => a -> a -> Bool) -> LispVal -> LispVal -> ThrowsError LispVal
promoteNumericOrdOp op x y = case numType x `max` numType y of
    FloatType -> return $ Bool (asFloat x `op` asFloat y)
    RatioType -> return $ Bool (asRatio x `op` asRatio y)
    IntType   -> return $ Bool (asNumber x `op` asNumber y)
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
