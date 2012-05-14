{-# LANGUAGE ExistentialQuantification #-}

module Language.Primitives (primitiveBindings, gensym_) where

import System.IO
import System.Directory
import System.Random
import Control.Monad.Error
import Control.Exception
import Ratio
import Complex
import Data.Array
import Data.Unique
import qualified Data.Map as Map

import Language.Numeric
import Language.Types
import Language.Core
import Language.Parser
import Language.Variables

-- Initial environment

primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= (flip bindVars $ map (makeFunc IOFunc) ioPrimitives
                                              ++ map (makeFunc PrimitiveFunc) primitives)
    where makeFunc constructor (var, func) = (var, constructor var func)

-- Primitives that execute within the IO monad

ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives = [ ("apply", applyProc)
               -- Read/write
               , ("write", writeProc)
               , ("read", readProc)
               -- Console output
               , ("pr", printProc putStr)
               , ("prn", printProc putStrLn)
               -- File i/o
               , ("open-input-file", makePort ReadMode)
               , ("open-output-file", makePort WriteMode)
               , ("close-input-port", closePort)
               , ("close-output-port", closePort)
               , ("read-contents", readContents)
               , ("read-all", readAll)
               , ("file-exists", fileExists)
               , ("delete-file", deleteFile)
               -- Hash functions
               , ("hash-update", hashUpdate)
               -- Random numbers and unique symbols
               , ("random", rand)
               , ("uniq", gensym) ]

applyProc :: [LispVal] -> IOThrowsError LispVal
applyProc [func, List args] = apply func args
applyProc (func : args)     = apply func args

makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [String filename] = asIO Port $ openFile filename mode
makePort mode args = throwError $ NumArgs 1 args

closePort :: [LispVal] -> IOThrowsError LispVal
closePort [Port port] = liftIO $ hClose port >> (return $ Bool True)
closePort _           = return $ Bool False

readProc :: [LispVal] -> IOThrowsError LispVal
readProc []          = readProc [Port stdin]
readProc [Port port] = (liftIO $ hGetLine port) >>= liftThrows . readExpr
readProc [notPort]   = throwError $ TypeMismatch "port" notPort
readProc badArgs = throwError $ NumArgs 1 badArgs

writeProc :: [LispVal] -> IOThrowsError LispVal
writeProc [obj]            = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port obj >> return nil
writeProc [obj, notPort]   = throwError $ TypeMismatch "port" notPort
writeProb badArgs = throwError $ NumArgs 1 badArgs

printProc :: (String -> IO ()) -> [LispVal] -> IOThrowsError LispVal
printProc printer []     = liftIO $ printer "" >> return nil
printProc printer (x:xs) = liftIO (putStr $ pr x) >> printProc printer xs
    where pr (String s) = s
          pr (Char c)   = [c]
          pr other      = showVal other

fileExists :: [LispVal] -> IOThrowsError LispVal
fileExists [String filename] = liftIO (doesFileExist filename) >>= return . Bool
fileExists [notString]       = throwError $ TypeMismatch "string" notString
fileExists badArgs           = throwError $ NumArgs 1 badArgs

deleteFile :: [LispVal] -> IOThrowsError LispVal
deleteFile [String filename] = do
    exists <- liftIO $ doesFileExist filename
    if exists
        then do
            liftIO $ removeFile filename
            return $ Bool True
        else return $ Bool False
deleteFile [notString] = throwError $ TypeMismatch "string" notString
deleteFile badArgs     = throwError $ NumArgs 1 badArgs

readContents :: [LispVal] -> IOThrowsError LispVal
readContents [String filename] = asIO String $ readFile filename
readContents [notString]       = throwError $ TypeMismatch "string" notString
readContents badArgs           = throwError $ NumArgs 1 badArgs

readAll :: [LispVal] -> IOThrowsError LispVal
readAll [String filename] = liftM List $ load filename
readAll [notString]       = throwError $ TypeMismatch "string" notString
readAll badArgs           = throwError $ NumArgs 1 badArgs

-- Unique symbols

gensym :: [LispVal] -> IOThrowsError LispVal
gensym []              = gensym_ "#g"
gensym [String prefix] = gensym_ prefix
gensym [notString]     = throwError $ TypeMismatch "string" notString
gensym badArgs         = throwError $ NumArgs 1 badArgs

-- Random numbers

rand :: [LispVal] -> IOThrowsError LispVal
rand [modulus] = case modulus of
    Number n -> asIO Number $ randomRIO (0, n - 1)
    Float n  -> asIO Float $ randomRIO (0, n)
    Ratio n  -> asIO (Ratio . toRational) $ randomRIO (0 , fromRational n :: Double)
rand badArgs = throwError $ NumArgs 1 badArgs

-- Core primitives

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = numericPrimitives ++ 
             [ ("type", typeOf)
             -- String comparisons
             , ("str==", strBoolBinop (==))
             , ("str<", strBoolBinop (<))
             , ("str>", strBoolBinop (>))
             , ("str<=", strBoolBinop (<=))
             , ("str>=", strBoolBinop (>=))
             -- Vectors
             , ("vector", vector)
             -- Hashes
             , ("hash", hash)
             , ("hash-keys", unaryOp keys)
             , ("hash-vals", unaryOp vals)
             , ("hash-insert", hashInsert)
             , ("hash-lookup", hashLookup)
             , ("hash-elem", hashElem)
             -- Polymorphic length
             , ("len", len)
             -- Type conversion
             , ("string->list", typeTrans stringToList)
             , ("list->string", typeTrans listToString)
             , ("symbol->string", typeTrans symbolToString)
             , ("string->symbol", typeTrans stringToSymbol)
             , ("vector->list", typeTrans vectorToList)
             , ("list->vector", typeTrans listToVector)
             -- Fundamental operators
             , ("car", car)
             , ("cdr", cdr)
             , ("cons", cons)
             -- Exception-handling
             , ("new-exception", newException)
             , ("exception-type", exceptionType)
             , ("exception-args", exceptionArgs)
             , ("raise", raiseException)
             -- Comparison operators
             , ("is", is)
             , ("iso", iso) ]

typeOf :: [LispVal] -> ThrowsError LispVal
typeOf xs = case xs of
    [ ] -> throwError $ NumArgs 1 xs
    [x] -> return $ Atom $ typeName x
    xs  -> return $ List $ map (Atom . typeName) xs

newException :: [LispVal] -> ThrowsError LispVal
newException (Atom name : args) = return $ Exception $ UserError name args
newException args               = return $ Exception $ UserError "error" args

exceptionType :: [LispVal] -> ThrowsError LispVal
exceptionType [Exception e]  = return $ Atom $ errorName e
exceptionType [notException] = throwError $ TypeMismatch "exception" notException
exceptionType badArgs  = throwError $ NumArgs 1 badArgs

exceptionArgs :: [LispVal] -> ThrowsError LispVal
exceptionArgs [Exception (UserError _ args)] = return $ List args
exceptionArgs [Exception e]                  = return $ List [String $ showError e]
exceptionArgs [notException]                 = throwError $ TypeMismatch "exception" notException
exceptionArgs badArgs                        = throwError $ NumArgs 1 badArgs

raiseException :: [LispVal] -> ThrowsError LispVal
raiseException [Exception err] = throwError err
raiseException [notException]  = throwError $ TypeMismatch "exception" notException
raiseException  badArgs        = throwError $ NumArgs 1 badArgs

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

-- Hash functions

keys :: LispVal -> ThrowsError LispVal
keys (Hash hash) = return . List $ Map.keys hash
keys notHash     = throwError $ TypeMismatch "hash" notHash

vals :: LispVal -> ThrowsError LispVal
vals (Hash hash) = return . List $ Map.elems hash
vals notHash     = throwError $ TypeMismatch "hash" notHash

hashInsert :: [LispVal] -> ThrowsError LispVal
hashInsert [key, val, Hash hash] = return $ Hash $ Map.insert key val hash
hashInsert [key, val, notHash]   = errTypeMismatch "hash" notHash
hashInsert badArgs               = errNumArgs 3 badArgs

hashLookup :: [LispVal] -> ThrowsError LispVal
hashLookup [key, Hash hash] = case Map.lookup key hash of
    Just val -> return val
    Nothing  -> throwError $ KeyNotFound key (Hash hash)
hashLookup [key, notHash]   = errTypeMismatch "hash" notHash
hashLookup badArgs          = errNumArgs 2 badArgs

hashElem :: [LispVal] -> ThrowsError LispVal
hashElem [key, Hash hash] = return $ Bool $ Map.member key hash
hashElem [key, notHash]   = errTypeMismatch "hash" notHash
hashElem badArgs          = errNumArgs 2 badArgs

hashUpdate :: [LispVal] -> IOThrowsError LispVal
hashUpdate [key, func, Hash hash] = if Map.member key hash
    then case typeName func of
        "procedure"  -> do
            let oldValue = hash Map.! key
            newValue <- apply func [oldValue]
            return $ Hash $ Map.insert key newValue hash
        notProcedure -> errTypeMismatch "procedure" func
    else throwError $ KeyNotFound key (Hash hash)
hashUpdate [key, func, notHash]   = errTypeMismatch "hash" notHash
hashUpdate badArgs                = errNumArgs 3 badArgs 

-- Primitives

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
is [Hash xs, Hash ys]                 = return $ Bool $ xs == ys
is [_ , _] = return $ Bool False
is badArgs = throwError $ NumArgs 2 badArgs

iso :: [LispVal] -> ThrowsError LispVal
iso [DottedList xs x, DottedList ys y] = listEquals iso (xs++[x]) (ys++[y])
iso [Vector xs, Vector ys]             = listEquals iso (elems xs) (elems ys)
iso [List xs, List ys]                 = listEquals iso xs ys
iso [arg1, arg2] = do
    primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2)
                       [AnyUnpacker unpackNum, AnyUnpacker unpackRat,
                        AnyUnpacker unpackFloat, AnyUnpacker unpackCplx,
                        AnyUnpacker unpackStr, AnyUnpacker unpackBool]
    eqvEquals <- is [arg1, arg2]
    return $ Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)
iso badArgs = throwError $ NumArgs 2 badArgs

-- Helper functions

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) = do
        unpacked1 <- unpacker arg1
        unpacked2 <- unpacker arg2
        return $ unpacked1 == unpacked2
    `catchError` (const $ return False)

gensym_ :: String -> IOThrowsError LispVal
gensym_ prefix = do
    u <- liftIO $ newUnique
    return $ Atom $ prefix ++ (show $ toInteger $ hashUnique u)

{- liftIO :: MonadIO m => IO a -> m a
        Lifts IO values into an IO-compatible monad
    liftM :: Monad m => (a -> b) -> (m a -> m b)
        Converts arbitrary functions into monadic functions
    asIO :: MonadIO m => (a -> b) -> (IO a -> m b)
        Converts arbitrary functions into functions that map IO values to
        arbitrary IO-compatible monads -}

asIO :: (MonadIO m) => (a -> b) -> (IO a -> m b)
asIO func = liftM func . liftIO