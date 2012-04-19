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
    - If the form is a list, and the first element is they symbol 'macro, then
      create a new macro and store it in the macro environment.
    - If the form is a list with an atom as the first element, then check if
      that atom has a binding in the macro environment. If it does, then apply
      the macro to the remaining elements of the list **without evaluating them
      first**, otherwise return the entire form.
    - If the form is an atom, check if it has a binding in the macro
      environment. If so, then quote and return the associated macro, otherwise
      return the form.
    - If the form is anything other than a list, then return it. -}
evalMacro (env, macroEnv) (List (Atom "macro" : List (Atom var : params) : body)) = 
    makeNormalMacro env params body >>= defineVar macroEnv var >> return (List [])
evalMacro (env, macroEnv) (List (Atom "macro" : DottedList (Atom var : params) varargs : body)) =
    makeVarArgsMacro varargs env params body >>= defineVar macroEnv var >> return (List [])
evalMacro (_ , macroEnv) val@(List (Atom name : args)) = do
    env <- liftIO $ readIORef macroEnv
    maybe (return val)
          (\macroRef -> do
            macro <- liftIO $ readIORef macroRef
            apply macro args)
          (lookup name env)
evalMacro (_, macroEnv) val@(Atom name) = do
    env <- liftIO $ readIORef macroEnv
    maybe (return val)
          (\macroRef -> do
            macro <- liftIO $ readIORef macroRef
            return $ List [Atom "quote", macro])
          (lookup name env)
evalMacro (_, _) val = return val