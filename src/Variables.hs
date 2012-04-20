module Variables where

import Data.IORef
import Data.Array
import Control.Monad.Error

import LispVal
import LispError

nullEnv :: IO Env
nullEnv = newIORef []

isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>= return . maybe False (const True) . lookup var

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do
    env <- liftIO $ readIORef envRef
    maybe (throwError $ UnboundVar "Getting an unbound variable" var)
          (liftIO . readIORef)
          (lookup var env)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do
    env <- liftIO $ readIORef envRef
    maybe (throwError $ UnboundVar "Setting an unbound variable" var)
          (liftIO . (flip writeIORef value))
          (lookup var env)
    return value

setCar :: Env -> String -> LispVal -> IOThrowsError LispVal
setCar envRef var val = do
    env <- liftIO $ readIORef envRef
    maybe (throwError $ UnboundVar "Setting car of an unbound variable" var)
          (\varRef -> do
            oldVal <- liftIO $ readIORef varRef
            case oldVal of
                List (_ : cdr)          -> liftIO $ writeIORef varRef (List (val : cdr))
                DottedList (_ : cdr) tl -> liftIO $ writeIORef varRef (DottedList (val : cdr) tl)
                Vector arr              -> liftIO $ writeIORef varRef (Vector $ listArray bds (val : cdr)) where
                  bds       = bounds arr
                  (_ : cdr) = elems arr
                String (_ : cdr) -> liftIO $ writeIORef varRef (case val of
                  Char c -> String (c : cdr)
                  _      -> List (val : map Char cdr))
                other    -> throwError $ TypeMismatch "pair, vector" other)
          (lookup var env)
    return val

setCdr :: Env -> String -> LispVal -> IOThrowsError LispVal
setCdr envRef var val = do
    env <- liftIO $ readIORef envRef
    maybe (throwError $ UnboundVar "Setting cdr of an unbound variable" var)
          (\varRef -> do
            oldVal <- liftIO $ readIORef varRef
            case oldVal of
                List (car : _) -> case val of
                    List xs          -> liftIO $ writeIORef varRef (List (car : xs))
                    DottedList xs tl -> liftIO $ writeIORef varRef (DottedList (car : xs) tl)
                    _                -> liftIO $ writeIORef varRef (DottedList [car] val)
                DottedList (car : _) _ -> case val of
                    List xs          -> liftIO $ writeIORef varRef (List (car : xs))
                    DottedList xs tl -> liftIO $ writeIORef varRef (DottedList (car : xs) tl)
                    _                -> liftIO $ writeIORef varRef (DottedList [car] val)
                notPair -> throwError $ TypeMismatch "pair" notPair)
          (lookup var env)
    return val

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
    alreadyDefined <- liftIO $ isBound envRef var
    if alreadyDefined
        then setVar envRef var value
        else liftIO $ do
            valueRef <- newIORef value
            env <- readIORef envRef
            writeIORef envRef ((var, valueRef) : env)
            return value

bindVars :: Env -> [(String,LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
    where extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
          addBinding (var, value) = do ref <- newIORef value
                                       return (var, ref)
