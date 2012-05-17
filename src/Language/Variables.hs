module Language.Variables where

import Control.Monad.Error
--import qualified Control.Monad.Reader as R
--import qualified Control.Monad.State as S

import Data.IORef
import Data.Array
import Data.Maybe (isJust)
import qualified Data.Map as Map

import Language.Types

-- Functions that work within the EvalM monad

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

-- Functions to manipulate environments (bind/unbind variables etc)

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

get :: Namespace -> Env -> Var -> IOThrowsError LispVal
get namespace env var = do
    binds <- liftIO $ readIORef (bindings env)
    maybe (case parent env of
            Nothing  -> throwError $ UnboundVar "Getting an unbound variable" var
            Just prt -> get namespace prt var)
          (liftIO . readIORef)
          (envLookup namespace var binds)

getVar = get varNamespace
getMacro = get macNamespace

set :: Namespace -> Env -> String -> LispVal -> IOThrowsError LispVal
set namespace env var val = do
    binds <- liftIO $ readIORef (bindings env)
    maybe (case parent env of
            Nothing  -> throwError $ UnboundVar "Setting an unbound variable" var
            Just prt -> set namespace prt var val)
          (flip writeAndReturn val)
          (envLookup namespace var binds)
    return val

setVar = set varNamespace
setMacro = set macNamespace

define :: Namespace -> Env -> String -> LispVal -> IOThrowsError LispVal
define namespace env var value = do
    definedLocally <- liftIO $ isBound namespace var env
    if definedLocally
        then do
            liftIO $ putStrLn $ "*** redefining " ++ var
            set namespace env var value
        else liftIO $ do
            valueRef <- newIORef value
            binds    <- readIORef (bindings env)
            writeIORef (bindings env) $ envInsert namespace var valueRef binds
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

setCar :: Env -> String -> LispVal -> IOThrowsError LispVal
setCar env var val = do
    local <- liftIO $ readIORef (bindings env)
    maybe (throwError $ UnboundVar "Setting car of an unbound variable" var)
          (\varRef -> do
            oldVal <- liftIO $ readIORef varRef
            case oldVal of
                List (_ : cdr)          -> setListCar varRef val cdr
                DottedList (_ : cdr) tl -> setDottedListCar varRef val cdr tl
                Vector arr              -> setVectorCar varRef val arr
                String (_ : cdr)        -> setStringCar varRef val cdr
                other    -> throwError $ TypeMismatch "pair, vector or string" other)
          (varLookup var local)

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

setCdr :: Env -> String -> LispVal -> IOThrowsError LispVal
setCdr env var val = do
    local <- liftIO $ readIORef (bindings env)
    maybe (throwError $ UnboundVar "Setting cdr of an unbound variable" var)
          (\varRef -> do
            oldVal <- liftIO $ readIORef varRef
            case oldVal of
                List (car : _)          -> setListCdr varRef car val
                DottedList (car : _) _  -> setListCdr varRef car val
                Vector arr              -> setVectorCdr varRef car val where car = arr ! 0
                String (car : _)        -> setStringCdr varRef car val
                notPair -> throwError $ TypeMismatch "pair, vector or string" notPair)
          (varLookup var local)

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

setIndex :: Env -> String -> LispVal -> LispVal -> IOThrowsError LispVal
setIndex env var index val = do
    local <- liftIO $ readIORef (bindings env)
    maybe (throwError $ UnboundVar "Setting index of an unbound variable" var)
          (\varRef -> do
            oldVal <- liftIO $ readIORef varRef
            case oldVal of
                String str -> setStringIndex varRef str index val
                Vector arr -> setVectorIndex varRef arr index val
                Hash hash  -> setHashKey varRef hash index val
                other -> throwError $ TypeMismatch "string, vector or hash" other)
          (varLookup var local)

setStringIndex :: IORef LispVal -> String -> LispVal -> LispVal -> IOThrowsError LispVal
setStringIndex varRef str index val = case index of
    Number n -> case val of
        Char c  -> writeAndReturn varRef (String $ replaceAt n str c)
        notChar -> throwError $ TypeMismatch "char" notChar
    notInt   -> throwError $ TypeMismatch "integer" notInt

replaceAt :: Integer -> [a] -> a -> [a]
replaceAt n lst val = xs ++ val:ys where
    (xs, _:ys) = splitAt (fromInteger n) lst

setVectorIndex :: IORef LispVal -> VectorType -> LispVal -> LispVal -> IOThrowsError LispVal
setVectorIndex varRef arr index val = case index of
    Number n -> writeAndReturn varRef (Vector $ arr // [(fromInteger n, val)])
    notInt   -> throwError $ TypeMismatch "integer" notInt

setHashKey :: IORef LispVal -> HashType -> LispVal -> LispVal -> IOThrowsError LispVal
setHashKey varRef hash key val = writeAndReturn varRef (Hash $ Map.insert key val hash)

-- Debugging

showNamespace :: Env -> LispVal -> IOThrowsError LispVal
showNamespace env (String name) = do
    local <- liftIO $ readIORef (bindings env)
    let namespace = filter (\((n,_),_) -> n == name) (Map.toList local)
    forM_ namespace printVar
    return $ List []
    where
        printVar ((_,var),valRef) = do
            val <- liftIO $ readIORef valRef
            liftIO $ putStrLn (var ++ " : " ++ show val)
showNamespace _ other = throwError $ TypeMismatch "string" other