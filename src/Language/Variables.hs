module Language.Variables (
    defineVar, defineMacro, getVar, setVar,
    readRef,
    macRecLookup,
    bindM, unbindM,
    extendEnv,
    setCar, setCdr, setIndex) where

import Control.Monad.Error

import Data.IORef
import Data.Array
import Data.Maybe (isJust)
import qualified Data.Map as Map

import Language.Types

-- Lifting of IO operations to work within the EvalM monad

newRef :: a -> EvalM (IORef a)
newRef = lift . lift . newIORef

readRef :: IORef a -> EvalM a
readRef = lift . lift . readIORef

writeRef :: IORef a -> a -> EvalM ()
writeRef ref = lift . lift . writeIORef ref

modifyRef :: (a -> a) -> IORef a -> EvalM ()
modifyRef f ref = readRef ref >>= writeRef ref . f

pushToStack :: IORef LispVal -> EvalM ()
pushToStack val = getStack >>= modifyRef (val:)

popFromStack :: EvalM (Maybe (IORef LispVal))
popFromStack = getStack >>= \stackref -> do
    stackval <- readRef stackref
    if null stackval
        then return Nothing
        else do modifyRef tail stackref
                return $ Just $ head stackval

-- Functions to manipulate environments using a stack to store shadowed
-- values - this is highly experimental!

pushIfBound :: Var -> EvalM ()
pushIfBound var = do
    maybeVal <- getEnv >>= liftIO . localVarLookup var
    case maybeVal of
        Just val -> pushToStack val
        Nothing  -> return ()

popIfBound :: Var -> EvalM ()
popIfBound var = do
    maybeVal <- popFromStack
    case maybeVal of
        Just val -> getBindings >>= modifyRef (varInsert var val)
        Nothing  -> return ()

bindM :: (Var, LispVal) -> EvalM ()
bindM (var, val) = do
    valRef <- newRef val
    pushIfBound var
    getBindings >>= modifyRef (varInsert var valRef)

unbindM :: Var -> EvalM ()
unbindM var = do
    getBindings >>= modifyRef (varRemove var)
    popIfBound var

-- Operations on environment implementation (i.e. underlying map)

varNamespace = "v"
macNamespace = "m"

envIsBound :: Namespace -> Var -> EnvType -> Bool
envIsBound namespace var env = Map.member (namespace, var) env

envVarIsBound = envIsBound varNamespace
envMacIsBound = envIsBound macNamespace

envLookup :: Namespace -> Var -> EnvType -> Maybe (IORef LispVal)
envLookup namespace var env = Map.lookup (namespace, var) env

varLookup = envLookup varNamespace
macroLookup = envLookup macNamespace 

envInsert :: Namespace -> Var -> IORef LispVal -> EnvType -> EnvType
envInsert namespace var val env = Map.insert (namespace, var) val env

varInsert = envInsert varNamespace
macroInsert = envInsert macNamespace

envRemove :: Namespace -> Var -> EnvType -> EnvType
envRemove namespace var env = Map.delete (namespace, var) env

varRemove = envRemove varNamespace
macRemove = envRemove macNamespace

-- Operations on environments

isBound :: Namespace -> Var -> Env -> IO Bool
isBound namespace var env = readIORef (bindings env) >>=
    return . envIsBound namespace var

varIsBound = isBound varNamespace
macroIsBound = isBound macNamespace

isRecBound :: Namespace -> Var -> Env -> IO Bool
isRecBound ns var env = isBound ns var env >>=
    \result -> if result
        then return True
        else case parent env of
            Nothing  -> return False
            Just prt -> isRecBound ns var prt

varIsRecBound = isRecBound varNamespace
macIsRecBound = isRecBound macNamespace

localLookup :: Namespace -> Var -> Env -> IO (Maybe (IORef LispVal))
localLookup ns var env = do 
    local <- readIORef (bindings env)
    return $ envLookup ns var local

localVarLookup = localLookup varNamespace
localMacLookup = localLookup macNamespace

recLookup :: Namespace -> Var -> Env -> IO (Maybe (IORef LispVal))
recLookup ns var env = do
    local <- readIORef (bindings env)
    case envLookup ns var local of
        val@(Just _) -> return val
        Nothing      -> case parent env of
            Nothing  -> return Nothing
            Just prt -> recLookup ns var prt

varRecLookup = recLookup varNamespace
macRecLookup = recLookup macNamespace

get :: Namespace -> Var -> EvalM LispVal
get namespace var = do
    env <- getEnv
    binds <- readRef (bindings env)
    maybe (case parent env of
            Nothing  -> throwError $ UnboundVar "Getting an unbound variable" var
            Just prt -> inEnv prt $ get namespace var)
          (liftIO . readIORef)
          (envLookup namespace var binds)

getVar = get varNamespace
getMacro = get macNamespace

set :: Namespace -> String -> LispVal -> EvalM LispVal
set namespace var val = do
    env <- getEnv
    loc <- readRef (bindings env)
    maybe (case parent env of
            Nothing  -> throwError $ UnboundVar "Setting an unbound variable" var
            Just prt -> inEnv prt $ set namespace var val)
          (lift . flip writeAndReturn val)
          (envLookup namespace var loc)
    return val

setVar = set varNamespace
setMacro = set macNamespace

define :: Namespace -> String -> LispVal -> EvalM LispVal
define namespace var value = do
    env <- getEnv
    definedLocally <- liftIO $ isBound namespace var env
    if definedLocally
        then do liftIO $ putStrLn $ "*** redefining " ++ var
                set namespace var value
        else do
            valueRef <- newRef value
            binds    <- readRef (bindings env)
            writeRef (bindings env) $ envInsert namespace var valueRef binds
            return value

defineVar = define varNamespace
defineMacro = define macNamespace

extendEnv :: [ (Var, LispVal) ] -> Env -> IO Env
extendEnv []    env = return env
extendEnv binds env = mapM mkBind binds >>= newIORef . Map.fromList >>=
    \bindings -> do
        nullStack <- newIORef []
        return $ Environment (Just env) nullStack bindings
    where mkBind (var, val) = newIORef val >>=
            ( \ref -> return (("v", var), ref) )

bindVars :: Env -> [(String,LispVal)] -> IO Env
bindVars = flip extendEnv

writeAndReturn :: IORef LispVal -> LispVal -> IOThrowsError LispVal
writeAndReturn varRef val = do
    liftIO $ writeIORef varRef val
    return val

setCar :: String -> LispVal -> EvalM LispVal
setCar var val = do
    env <- getEnv
    loc <- getBindings >>= readRef
    maybe (case parent env of
            Nothing  -> throwError $ UnboundVar "Setting car of an unbound variable" var
            Just prt -> inEnv prt $ setCar var val)
          (\varRef -> do
            oldVal <- readRef varRef
            case oldVal of
                List (_ : cdr)          -> lift $ setListCar varRef val cdr
                DottedList (_ : cdr) tl -> lift $ setDottedListCar varRef val cdr tl
                Vector arr              -> lift $ setVectorCar varRef val arr
                String (_ : cdr)        -> lift $ setStringCar varRef val cdr
                other    -> errTypeMismatch "pair, vector or string" other)
          (varLookup var loc)

setListCar :: IORef LispVal -> LispVal -> [LispVal] -> IOThrowsError LispVal
setListCar varRef val cdr = writeAndReturn varRef $ List (val : cdr)

setDottedListCar :: IORef LispVal -> LispVal -> [LispVal] -> LispVal -> IOThrowsError LispVal
setDottedListCar varRef val cdr tl = writeAndReturn varRef $ DottedList (val : cdr) tl

setVectorCar :: IORef LispVal -> LispVal -> VectorType -> IOThrowsError LispVal
setVectorCar varRef val arr =
    let bds       = bounds arr
        (_ : cdr) = elems arr
    in writeAndReturn varRef $ Vector $ listArray bds (val : cdr) where

setStringCar :: IORef LispVal -> LispVal -> String -> IOThrowsError LispVal
setStringCar varRef val cdr = writeAndReturn varRef $ case val of
    Char c -> String (c : cdr)
    _      -> List (val : map Char cdr)

setCdr :: String -> LispVal -> EvalM LispVal
setCdr var val = do
    env <- getEnv
    loc <- getBindings >>= readRef
    maybe (case parent env of
            Nothing  -> throwError $ UnboundVar "Setting cdr of an unbound variable" var
            Just prt -> inEnv prt $ setCdr var val)
          (\varRef -> do
            oldVal <- liftIO $ readIORef varRef
            case oldVal of
                List (car : _)          -> lift $ setListCdr varRef car val
                DottedList (car : _) _  -> lift $ setListCdr varRef car val
                Vector arr              -> lift $ setVectorCdr varRef car val where car = arr ! 0
                String (car : _)        -> lift $ setStringCdr varRef car val
                notPair -> errTypeMismatch "pair, vector or string" notPair)
          (varLookup var loc)

setListCdr :: IORef LispVal -> LispVal -> LispVal -> IOThrowsError LispVal
setListCdr varRef car val = writeAndReturn varRef (case val of
    List xs          -> List (car : xs)
    DottedList xs tl -> DottedList (car : xs) tl
    _                -> DottedList [car] val)

setVectorCdr :: IORef LispVal -> LispVal -> LispVal -> IOThrowsError LispVal
setVectorCdr varRef car val = writeAndReturn varRef (case val of
    Vector arr'      -> Vector $ listArray (0,n+1) (car : cdr) where
        (_, n) = bounds arr'
        cdr    = elems arr'
    List xs          -> List (car : xs)
    DottedList xs tl -> DottedList (car : xs) tl
    _                -> DottedList [car] val)

setStringCdr :: IORef LispVal -> Char -> LispVal -> IOThrowsError LispVal
setStringCdr varRef car val = writeAndReturn varRef (case val of
    String xs        -> String (car : xs)
    List xs          -> List (Char car : xs)
    DottedList xs tl -> DottedList (Char car : xs) tl
    _                -> DottedList [Char car] val)

setIndex :: String -> LispVal -> LispVal -> EvalM LispVal
setIndex var index val = do
    env <- getEnv
    loc <- getBindings >>= readRef
    maybe (case parent env of
            Nothing  -> throwError $ UnboundVar "Setting index of an unbound variable" var
            Just prt -> inEnv prt $ setIndex var index val)
          (\varRef -> do
            oldVal <- liftIO $ readIORef varRef
            case oldVal of
                String str -> lift $ setStringIndex varRef str index val
                Vector arr -> lift $ setVectorIndex varRef arr index val
                Hash hash  -> lift $ setHashKey varRef hash index val
                other -> errTypeMismatch "string, vector or hash" other)
          (varLookup var loc)

setStringIndex :: IORef LispVal -> String -> LispVal -> LispVal -> IOThrowsError LispVal
setStringIndex varRef str index val = case index of
    Number n -> case val of
        Char c  -> writeAndReturn varRef (String $ replaceAt n str c)
        notChar -> errTypeMismatch "char" notChar
    notInt   -> errTypeMismatch "integer" notInt

replaceAt :: Integer -> [a] -> a -> [a]
replaceAt n lst val = xs ++ val:ys where
    (xs, _:ys) = splitAt (fromInteger n) lst

setVectorIndex :: IORef LispVal -> VectorType -> LispVal -> LispVal -> IOThrowsError LispVal
setVectorIndex varRef arr index val = case index of
    Number n -> writeAndReturn varRef (Vector $ arr // [(fromInteger n, val)])
    notInt   -> errTypeMismatch "integer" notInt

setHashKey :: IORef LispVal -> HashType -> LispVal -> LispVal -> IOThrowsError LispVal
setHashKey varRef hash key val = writeAndReturn varRef (Hash $ Map.insert key val hash)