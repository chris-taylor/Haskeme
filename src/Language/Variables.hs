module Language.Variables where

import Data.IORef
import Data.Array
import qualified Data.Map as Map
import Control.Monad.Error

import Language.LispVal

nullEnv :: IO Env
nullEnv = newIORef Map.empty

varNamespace :: String
varNamespace = "v"

macroNamespace :: String
macroNamespace = "m"

varLookup :: String -> EnvType -> Maybe (IORef LispVal)
varLookup var env = Map.lookup (varNamespace, var) env

varInsert :: String -> IORef LispVal -> EnvType -> EnvType
varInsert var val env = Map.insert (varNamespace, var) val env

isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>=
    return . maybe False (const True) . varLookup var

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do
    env <- liftIO $ readIORef envRef
    maybe (throwError $ UnboundVar "Getting an unbound variable" var)
          (liftIO . readIORef)
          (varLookup var env)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var val = do
    env <- liftIO $ readIORef envRef
    maybe (throwError $ UnboundVar "Setting an unbound variable" var)
          (liftIO . (flip writeIORef val))
          (varLookup var env)
    return val

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
    alreadyDefined <- liftIO $ isBound envRef var
    if alreadyDefined
        then setVar envRef var value
        else liftIO $ do
            valueRef <- newIORef value
            env <- readIORef envRef
            writeIORef envRef $ varInsert var valueRef env
            return value

bindVars :: Env -> [(String,LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
    where extendEnv bindings env = liftM (Map.union env) newBindings
          newBindings = liftM Map.fromList $ mapM addBinding bindings
          addBinding (var, value) = do ref <- newIORef value
                                       return ((varNamespace, var), ref)

setCar :: Env -> String -> LispVal -> IOThrowsError LispVal
setCar envRef var val = do
    env <- liftIO $ readIORef envRef
    maybe (throwError $ UnboundVar "Setting car of an unbound variable" var)
          (\varRef -> do
            oldVal <- liftIO $ readIORef varRef
            case oldVal of
                List (_ : cdr)          -> setListCar varRef val cdr
                DottedList (_ : cdr) tl -> setDottedListCar varRef val cdr tl
                Vector arr              -> setVectorCar varRef val arr
                String (_ : cdr)        -> setStringCar varRef val cdr
                other    -> throwError $ TypeMismatch "pair, vector or string" other)
          (varLookup var env)
    return val

setListCar :: IORef LispVal -> LispVal -> [LispVal] -> IOThrowsError ()
setListCar varRef val cdr = liftIO $ writeIORef varRef $ List (val : cdr)

setDottedListCar :: IORef LispVal -> LispVal -> [LispVal] -> LispVal -> IOThrowsError ()
setDottedListCar varRef val cdr tl = liftIO $ writeIORef varRef $ DottedList (val : cdr) tl

setVectorCar :: IORef LispVal -> LispVal -> VectorType -> IOThrowsError ()
setVectorCar varRef val arr = liftIO $ writeIORef varRef $ Vector $ listArray bds (val : cdr) where
    bds       = bounds arr
    (_ : cdr) = elems arr

setStringCar :: IORef LispVal -> LispVal -> String -> IOThrowsError ()
setStringCar varRef val cdr = liftIO $ writeIORef varRef (case val of
    Char c -> String (c : cdr)
    _      -> List (val : map Char cdr))

setCdr :: Env -> String -> LispVal -> IOThrowsError LispVal
setCdr envRef var val = do
    env <- liftIO $ readIORef envRef
    maybe (throwError $ UnboundVar "Setting cdr of an unbound variable" var)
          (\varRef -> do
            oldVal <- liftIO $ readIORef varRef
            case oldVal of
                List (car : _)          -> setListCdr varRef car val
                DottedList (car : _) _  -> setListCdr varRef car val
                Vector arr              -> setVectorCdr varRef car val where car = arr ! 0
                String (car : _)        -> setStringCdr varRef car val
                notPair -> throwError $ TypeMismatch "pair, vector or string" notPair)
          (varLookup var env)
    return val

setListCdr :: IORef LispVal -> LispVal -> LispVal -> IOThrowsError ()
setListCdr varRef car val = liftIO $ writeIORef varRef (case val of
    List xs          -> List (car : xs)
    DottedList xs tl -> DottedList (car : xs) tl
    _                -> DottedList [car] val)

setVectorCdr :: IORef LispVal -> LispVal -> LispVal -> IOThrowsError ()
setVectorCdr varRef car val = liftIO $ writeIORef varRef (case val of
    Vector arr'      -> Vector $ listArray (0,n+1) (car : cdr) where
        (_, n) = bounds arr'
        cdr    = elems arr'
    List xs          -> List (car : xs)
    DottedList xs tl -> DottedList (car : xs) tl
    _                -> DottedList [car] val)

setStringCdr :: IORef LispVal -> Char -> LispVal -> IOThrowsError ()
setStringCdr varRef car val = liftIO $ writeIORef varRef (case val of
    String xs        -> String (car : xs)
    List xs          -> List (Char car : xs)
    DottedList xs tl -> DottedList (Char car : xs) tl
    _                -> DottedList [Char car] val)

setIndex :: Env -> String -> LispVal -> LispVal -> IOThrowsError LispVal
setIndex envRef var index val = do
    env <- liftIO $ readIORef envRef
    maybe (throwError $ UnboundVar "Setting index of an unbound variable" var)
          (\varRef -> do
            oldVal <- liftIO $ readIORef varRef
            case oldVal of
                String str -> setStringIndex varRef str index val
                Vector arr -> setVectorIndex varRef arr index val
                Hash hash  -> setHashKey varRef hash index val
                other -> throwError $ TypeMismatch "string, vector or hash" other)
          (varLookup var env)
    return val

setStringIndex :: IORef LispVal -> String -> LispVal -> LispVal -> IOThrowsError ()
setStringIndex varRef str index val = case index of
    Number n -> case val of
        Char c  -> liftIO $ writeIORef varRef (String $ replaceAt n str c)
        notChar -> throwError $ TypeMismatch "char" notChar
    notInt   -> throwError $ TypeMismatch "integer" notInt

replaceAt :: Integer -> [a] -> a -> [a]
replaceAt n lst val = xs ++ val:ys where
    (xs, _:ys) = splitAt (fromInteger n) lst

setVectorIndex :: IORef LispVal -> VectorType -> LispVal -> LispVal -> IOThrowsError ()
setVectorIndex varRef arr index val = case index of
    Number n -> liftIO $ writeIORef varRef (Vector $ arr // [(fromInteger n, val)])
    notInt   -> throwError $ TypeMismatch "integer" notInt

setHashKey :: IORef LispVal -> HashType -> LispVal -> LispVal -> IOThrowsError ()
setHashKey varRef hash key val = liftIO $ writeIORef varRef (Hash $ Map.insert key val hash)

genUniqueSym :: Env -> [LispVal] -> IOThrowsError LispVal
genUniqueSym envRef args = if null args
    then genUniqueSym envRef [String ""]
    else do
        env <- liftIO $ readIORef envRef
        sym <- genUniqueName env args
        liftIO $ do
            nullRef <- newIORef (List [])
            writeIORef envRef $ varInsert sym nullRef env
        return $ Atom sym

genUniqueName :: EnvType -> [LispVal] -> IOThrowsError String
genUniqueName env args = case args of
    [String name] -> return $ "#:" ++ name ++ show (1 + Map.size env)
    [notString]   -> throwError $ TypeMismatch "string" notString
    badArgs       -> throwError $ NumArgs 1 badArgs

