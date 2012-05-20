{-# LANGUAGE ExistentialQuantification #-}

module Language.Primitives (primitiveBindings, gensym_, load) where

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
primitiveBindings = nullEnv >>= (extendEnv $ map (makeFunc EvalFunc) evalPrimitives
                                          ++ map (makeFunc IOFunc) ioPrimitives
                                          ++ map (makeFunc PrimitiveFunc) primitives)
    where makeFunc constructor (var, func) = (var, constructor var func)

-- Primitives that execute within the EvalM monad (can access the environment)

evalPrimitives :: [(String, [LispVal] -> EvalM LispVal)]
evalPrimitives = [ ("apply", applyProc)
                 , ("eval", unaryEvalFunc meval)
                 , ("expand", unaryEvalFunc expandAll)
                 , ("expand1", unaryEvalFunc expandOne)
                 , ("load", unaryEvalFunc loadProc)
                 , ("dump-env", unaryEvalFunc dumpEnv)
                 , ("bind", bindProc)
                 , ("unbind", unaryEvalFunc unbindProc)
                 , ("hash-update", hashUpdate) ]

unaryEvalFunc :: (LispVal -> EvalM LispVal) -> [LispVal] -> EvalM LispVal
unaryEvalFunc func [expr]  = func expr
unaryEvalFunc func badArgs = lift $ errNumArgs 1 badArgs

applyProc :: [LispVal] -> EvalM LispVal
applyProc [func, List args] = apply func args
applyProc (func : args)     = apply func args

loadProc :: LispVal -> EvalM LispVal
loadProc (String filename) = (lift $ load filename) >>= liftM last . mapM meval
loadProc other             = lift $ errTypeMismatch "string" other

dumpEnv :: LispVal -> EvalM LispVal
dumpEnv (String name) = do
    local <- getBindings >>= readRef
    let namespace = filter (\((n,_),_) -> n == name) (Map.toList local)
    forM_ namespace printVar
    return $ List []
    where
        printVar ((_,var),valRef) = do
            val <- readRef valRef
            liftIO $ putStrLn (var ++ " : " ++ show val)
dumpEnv notString = lift $ errTypeMismatch "string" notString

bindProc :: [LispVal] -> EvalM LispVal
bindProc [Atom var, val] = bindM (var, val) >> return (Atom "ok")
bindProc [notAtom, val]  = lift $ errTypeMismatch "atom" notAtom
bindProc badArgs         = lift $ errNumArgs 2 badArgs

unbindProc :: LispVal -> EvalM LispVal
unbindProc (Atom var) = unbindM var >> return (Atom "ok")
unbindProc notAtom    = lift $ errTypeMismatch "atom" notAtom

-- Primitives that execute within the IOThrowsError monad (can perform I/O)

ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives = -- Read/write
               [ ("write", writeProc)
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
               -- Random number generation
               , ("random", rand)
               -- Generate unique symbols
               , ("uniq", gensym) ]

makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [String filename] = asIO Port $ openFile filename mode
makePort mode args = errNumArgs 1 args

closePort :: [LispVal] -> IOThrowsError LispVal
closePort [Port port] = liftIO $ hClose port >> (return $ Bool True)
closePort _           = return $ Bool False

readProc :: [LispVal] -> IOThrowsError LispVal
readProc []          = readProc [Port stdin]
readProc [Port port] = (liftIO $ hGetLine port) >>= liftThrows . readExpr
readProc [notPort]   = errTypeMismatch "port" notPort
readProc badArgs = errNumArgs 1 badArgs

writeProc :: [LispVal] -> IOThrowsError LispVal
writeProc [obj]            = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port obj >> return nil
writeProc [obj, notPort]   = errTypeMismatch "port" notPort
writeProb badArgs = errNumArgs 1 badArgs

printProc :: (String -> IO ()) -> [LispVal] -> IOThrowsError LispVal
printProc printer []     = liftIO $ printer "" >> return nil
printProc printer (x:xs) = liftIO (putStr $ pr x) >> printProc printer xs
    where pr (String s) = s
          pr (Char c)   = [c]
          pr other      = showVal other

fileExists :: [LispVal] -> IOThrowsError LispVal
fileExists [String filename] = liftIO (doesFileExist filename) >>= return . Bool
fileExists [notString]       = errTypeMismatch "string" notString
fileExists badArgs           = errNumArgs 1 badArgs

deleteFile :: [LispVal] -> IOThrowsError LispVal
deleteFile [String filename] = do
    exists <- liftIO $ doesFileExist filename
    if exists
        then do
            liftIO $ removeFile filename
            return $ Bool True
        else return $ Bool False
deleteFile [notString] = errTypeMismatch "string" notString
deleteFile badArgs     = errNumArgs 1 badArgs

readContents :: [LispVal] -> IOThrowsError LispVal
readContents [String filename] = asIO String $ readFile filename
readContents [notString]       = errTypeMismatch "string" notString
readContents badArgs           = errNumArgs 1 badArgs

readAll :: [LispVal] -> IOThrowsError LispVal
readAll [String filename] = liftM List $ load filename
readAll [notString]       = errTypeMismatch "string" notString
readAll badArgs           = errNumArgs 1 badArgs

gensym :: [LispVal] -> IOThrowsError LispVal
gensym []              = gensym_ "#g"
gensym [String prefix] = gensym_ prefix
gensym [notString]     = errTypeMismatch "string" notString
gensym badArgs         = errNumArgs 1 badArgs

rand :: [LispVal] -> IOThrowsError LispVal
rand [modulus] = case modulus of
    Number n -> asIO Number $ randomRIO (0, n - 1)
    Float n  -> asIO Float $ randomRIO (0, n)
    Ratio n  -> asIO (Ratio . toRational) $ randomRIO (0 , fromRational n :: Double)
rand badArgs = errNumArgs 1 badArgs

-- Primitives that execute within the ThrowsError monad

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = -- Numeric functions
             numericPrimitives ++ 
             -- Type querying
             [ ("type", unaryOp typeOf)
             -- String comparisons
             --, ("str==", strBoolBinop (==))
             --, ("str<", strBoolBinop (<))
             --, ("str>", strBoolBinop (>))
             --, ("str<=", strBoolBinop (<=))
             --, ("str>=", strBoolBinop (>=))
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
             , ("exception-type", unaryOp exceptionType)
             , ("exception-args", unaryOp exceptionArgs)
             , ("raise", unaryOp raiseException)
             -- Comparison operators
             , ("is", is) ]
             --, ("iso", iso) ]

unaryOp :: (LispVal -> ThrowsError LispVal) -> [LispVal] -> ThrowsError LispVal
unaryOp op [x]   = op x
unaryOp op other = errNumArgs 1 other

typeOf :: LispVal -> ThrowsError LispVal
typeOf x = return $ Atom $ typeName x

-- Exceptions

newException :: [LispVal] -> ThrowsError LispVal
newException (Atom name : args) = return $ Exception $ UserError name args
newException args               = return $ Exception $ UserError "error" args

exceptionType :: LispVal -> ThrowsError LispVal
exceptionType (Exception e) = return $ Atom $ errorName e
exceptionType notException  = errTypeMismatch "exception" notException

exceptionArgs :: LispVal -> ThrowsError LispVal
exceptionArgs (Exception (UserError _ args)) = return $ List args
exceptionArgs (Exception e)                  = return $ List [String $ showError e]
exceptionArgs notException                   = errTypeMismatch "exception" notException

raiseException :: LispVal -> ThrowsError LispVal
raiseException (Exception err) = throwError err
raiseException notException    = errTypeMismatch "exception" notException

-- Type coercion

typeTrans :: (LispVal -> ThrowsError LispVal) -> [LispVal] -> ThrowsError LispVal
typeTrans f [x]  = f x
typeTrans f args = errNumArgs 1 args

listToString :: LispVal -> ThrowsError LispVal
listToString (List xs) = liftM String $ stringify xs where
    stringify []              = return ""
    stringify (Char c : rest) = liftM (c:) $ stringify rest
    stringify (other : rest)  = errTypeMismatch "char" other
listToString notList   = errTypeMismatch "list" notList

stringToList :: LispVal -> ThrowsError LispVal
stringToList (String cs) = return $ List (map Char cs)
stringToList notString   = errTypeMismatch "string" notString

symbolToString :: LispVal -> ThrowsError LispVal
symbolToString (Atom val) = return $ String val
symbolToString notAtom    = errTypeMismatch "symbol" notAtom

stringToSymbol :: LispVal -> ThrowsError LispVal
stringToSymbol (String val) = return $ Atom val
stringToSymbol notString    = errTypeMismatch "string" notString

vectorToList :: LispVal -> ThrowsError LispVal
vectorToList (Vector arr) = return $ List (elems arr)
vectorToList notVector    = errTypeMismatch "vector" notVector

listToVector :: LispVal -> ThrowsError LispVal
listToVector (List xs) = return $ Vector $ listArray (0, length xs - 1) xs
listToVector notList   = errTypeMismatch "list" notList

-- Hash functions

hash :: [LispVal] -> ThrowsError LispVal
hash xs = return $ Hash $ Map.fromList $ pairs xs

keys :: LispVal -> ThrowsError LispVal
keys (Hash hash) = return . List $ Map.keys hash
keys notHash     = errTypeMismatch "hash" notHash

vals :: LispVal -> ThrowsError LispVal
vals (Hash hash) = return . List $ Map.elems hash
vals notHash     = errTypeMismatch "hash" notHash

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

hashUpdate :: [LispVal] -> EvalM LispVal
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

-- Vector functions

vector :: [LispVal] -> ThrowsError LispVal
vector xs = return $ Vector $ listArray (0, length xs - 1) xs

-- Lisp primitives

car :: [LispVal] -> ThrowsError LispVal
car [List (x:_)]          = return x
car [DottedList (x:_) _]  = return x
car [String (x:_)]        = return (Char x)
car [Vector arr]          = case snd (bounds arr) of
                                (-1) -> errTypeMismatch "pair" (Vector arr)
                                _    -> return (arr ! 0)
car [notPair]             = errTypeMismatch "pair" notPair
car badArgs               = errNumArgs 2 badArgs

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x:xs)]           = return $ List xs
cdr [DottedList [_] x]      = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [String (x:xs)]         = return $ String xs
cdr [Vector arr]            = case n of
                                (-1) -> errTypeMismatch "pair" (Vector arr)
                                _    -> return $ Vector $ listArray (0, n-1) (drop 1 els)
                              where (_, n) = bounds arr
                                    els    = elems arr
cdr [notPair]               = errTypeMismatch "pair" notPair
cdr badArgs                 = errNumArgs 1 badArgs

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
cons badArgs               = errNumArgs 2 badArgs

-- Polymorphic length

len :: [LispVal] -> ThrowsError LispVal
len [arg]   = len' arg >>= return . Number . fromIntegral
len badArgs = errNumArgs 1 badArgs

len' :: LispVal -> ThrowsError Int
len' (List xs)          = return $ length xs
len' (DottedList xs _)  = return $ 1 + length xs
len' (Vector arr)       = let (_, n) = bounds arr in return (n + 1)
len' (Hash hash)        = return $ length $ Map.keys hash
len' (String str)       = return $ length str
len' other = errTypeMismatch "pair, vector, hash or string" other

-- Equality testing

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
is badArgs = errNumArgs 2 badArgs

-- Helper functions

gensym_ :: String -> IOThrowsError LispVal
gensym_ prefix = do
    u <- liftIO $ newUnique
    return $ Atom $ prefix ++ (show $ toInteger $ hashUnique u)

load :: String -> IOThrowsError [LispVal]
load filename = do
    result <- liftIO $ doesFileExist filename
    if result
      then (liftIO $ readFile filename) >>= liftThrows . readExprList
      else throwError $ FileNotFound filename

{- liftIO :: MonadIO m => IO a -> m a
        Lifts IO values into an IO-compatible monad
    liftM :: Monad m => (a -> b) -> (m a -> m b)
        Converts arbitrary functions into monadic functions
    asIO :: MonadIO m => (a -> b) -> (IO a -> m b)
        Converts arbitrary functions into functions that map IO values to
        arbitrary IO-compatible monads -}

asIO :: (MonadIO m) => (a -> b) -> (IO a -> m b)
asIO func = liftM func . liftIO