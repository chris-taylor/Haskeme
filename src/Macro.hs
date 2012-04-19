module Macro where

import Data.IORef
import Control.Monad.Error

import LispVal
import LispError
import EvalApply
import Variables

evalMacro :: (Env, Env) -> LispVal -> IOThrowsError LispVal
evalMacro (e1, e2) (List (Atom "defmacro" : List (Atom var : params) : body)) = 
    makeNormalMacro e1 params body >>= defineVar e2 var >> return (List [])
evalMacro (e1, e2) (List (Atom "defmacro" : DottedList (Atom var : params) varargs : body)) =
    makeVarArgs varargs e1 params body >>= defineVar e2 var >> return (List [])
evalMacro (_ , e2) val@(List (Atom name : args)) = do
    env <- liftIO $ readIORef e2
    maybe (return val)
          (\macroRef -> do
            macro <- liftIO $ readIORef macroRef
            applyMacro macro args)
          (lookup name env)
evalMacro (_, _) val = return val

applyMacro :: LispVal -> [LispVal] -> IOThrowsError LispVal
applyMacro (Macro params varargs body closure) args = 
    if num params /= num args && varargs == Nothing
        then throwError $ NumArgs (num params) args
        else (liftIO $ bindVars closure $ zip params args) >>= bindVarArgs varargs >>= evalBody
    where
        remainingArgs = drop (length params) args
        num = toInteger . length
        evalBody env = liftM last $ mapM (eval env) body
        bindVarArgs arg env = case arg of
            Just argName -> liftIO $ bindVars env [(argName, List $ remainingArgs)]
            Nothing -> return env

makeMacro :: (Monad m) => Maybe String -> Env -> [LispVal] -> [LispVal] -> m LispVal
makeMacro varargs env params body = return $ Macro (map showVal params) varargs body env

makeNormalMacro :: (Monad m) => Env -> [LispVal] -> [LispVal] -> m LispVal
makeNormalMacro = makeMacro Nothing

makeVarArgs :: (Monad m) => LispVal -> Env -> [LispVal] -> [LispVal] -> m LispVal
makeVarArgs = makeMacro . Just . showVal