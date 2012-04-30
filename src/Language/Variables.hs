module Language.Variables where

import Data.IORef
import Data.Array
import qualified Data.Map as Map
import Control.Monad.Error

import Language.LispVal

nullEnv :: IO Env
nullEnv = newIORef Map.empty

varNamespace = "v"
macroNamespace = "m"

envLookup :: Namespace -> Var -> EnvType -> Maybe (IORef LispVal)
envLookup namespace var env = Map.lookup (namespace, var) env

varLookup = envLookup varNamespace
macroLookup = envLookup macroNamespace 

envInsert :: Namespace -> Var -> IORef LispVal -> EnvType -> EnvType
envInsert namespace var val env = Map.insert (namespace, var) val env

varInsert = envInsert varNamespace
macroInsert = envInsert macroNamespace

isBound :: Namespace -> Var -> Env -> IO Bool
isBound namespace var envRef = readIORef envRef >>=
    return . maybe False (const True) . envLookup namespace var

varIsBound = isBound varNamespace
macroIsBound = isBound macroNamespace

get :: Namespace -> Env -> Var -> IOThrowsError LispVal
get namespace envRef var = do
    env <- liftIO $ readIORef envRef
    maybe (throwError $ UnboundVar "Getting an unbound variable" var)
          (liftIO . readIORef)
          (envLookup namespace var env)

getVar = get varNamespace
getMacro = get macroNamespace

set :: Namespace -> Env -> String -> LispVal -> IOThrowsError LispVal
set namespace envRef var val = do
    env <- liftIO $ readIORef envRef
    maybe (throwError $ UnboundVar "Setting an unbound variable" var)
          (liftIO . (flip writeIORef val))
          (envLookup namespace var env)
    return val

setVar = set varNamespace
setMacro = set macroNamespace

define :: Namespace -> Env -> String -> LispVal -> IOThrowsError LispVal
define namespace envRef var value = do
    alreadyDefined <- liftIO $ isBound namespace var envRef
    if alreadyDefined
        then set namespace envRef var value
        else liftIO $ do
            valueRef <- newIORef value
            env <- readIORef envRef
            writeIORef envRef $ envInsert namespace var valueRef env
            return value

defineVar = define varNamespace
defineMacro = define macroNamespace

extendEnv :: [ (Var, LispVal) ] -> Env -> IO Env
extendEnv [] envRef               = return envRef
extendEnv (binding : rest) envRef = addBinding envRef binding >>= extendEnv rest
    where addBinding envRef (var, value) = do
            env <- readIORef envRef
            ref <- newIORef value
            newIORef $ Map.insert ("v", var) ref env

bindVars :: Env -> [(String,LispVal)] -> IO Env
bindVars = flip extendEnv
--bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
--    where extendEnv bindings env = liftM (Map.union env) newBindings
--          newBindings = liftM Map.fromList $ mapM addBinding bindings
--          addBinding (var, value) = do ref <- newIORef value
--                                       return ((varNamespace, var), ref)

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

-- Debugging

showNamespace :: Env -> LispVal -> IOThrowsError LispVal
showNamespace envRef (String name) = do
    env <- liftIO $ readIORef envRef
    let namespace = filter (\((n,_),_) -> n == name) (Map.toList env)
    forM_ namespace printVar
    return $ List []
    where
        printVar ((_,var),valRef) = do
            val <- liftIO $ readIORef valRef
            liftIO $ putStrLn (var ++ " : " ++ show val)
showNamespace _ other = throwError $ TypeMismatch "string" other