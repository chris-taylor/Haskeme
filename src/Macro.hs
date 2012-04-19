module Macro where

import Data.IORef
import Control.Monad.Error

import LispVal
import LispError
import EvalApply
import Variables

evalMacro :: (Env, Env) -> LispVal -> IOThrowsError LispVal
{-  Macro evaluation takes two environments: the normal environment containing
    all primitives and user definitions, and a special macro environment that
    contains only macro forms. Evaluation works as follows:
    - If the form is a list, and the first element is 'defmacro', then create a
      new macro and store it in the macro environment.
    - If the form is a list with an atom as the first element, then check if
      that atom has a binding in the macro environment. If it does, then apply
      the macro to the remaining elements of the list **without evaluating them
      first**, otherwise return the entire form.
    - If the form is anything other than a list, then return it. -}
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
{-  The code here is identical to application of a user-defined function, except
    that it accepts a value of type Macro instead of Func. Is it possible to
    represent Macros as Funcs, but store them in the macro environment instead
    of in the normal environment? -}
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

{-  These functions are nearly identical to makeFun, makeNormalFun etc in
    EvalApply.hs except that they return Macro instead of Func. Can they be
    unified? -}

makeMacro :: (Monad m) => Maybe String -> Env -> [LispVal] -> [LispVal] -> m LispVal
makeMacro varargs env params body = return $ Macro (map showVal params) varargs body env

makeNormalMacro :: (Monad m) => Env -> [LispVal] -> [LispVal] -> m LispVal
makeNormalMacro = makeMacro Nothing

makeVarArgs :: (Monad m) => LispVal -> Env -> [LispVal] -> [LispVal] -> m LispVal
makeVarArgs = makeMacro . Just . showVal