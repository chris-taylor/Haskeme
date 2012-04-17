module EvalApply (eval, apply, load) where

import Control.Monad.Error

import LispVal
import LispError
import LispParser
import Variables

-- Evaluation

eval :: Env -> LispVal -> IOThrowsError LispVal
eval env val@(String _)  = return val
eval env val@(Char _)    = return val
eval env val@(Number _)  = return val
eval env val@(Ratio _)   = return val
eval env val@(Float _)   = return val
eval env val@(Complex _) = return val
eval env val@(Bool _)    = return val
eval env (Atom name)     = getVar env name
eval env (List [Atom "quote", val]) = return val
eval env (List [Atom "quasiquote", val]) =
    evalQuasiquote env val
eval env (List [Atom "if", predicate, conseq, alt]) =
    evalIf env predicate conseq alt
eval env (List (Atom "cond" : clauses)) = 
    evalCond env clauses
eval env (List [Atom "set!", Atom var, form]) = 
    eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) = 
    eval env form >>= defineVar env var
eval env (List (Atom "define" : List (Atom var : params) : body)) = 
    makeNormalFunc env params body >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : params) varargs : body)) = 
    makeVarArgs varargs env params body >>= defineVar env var
eval env (List (Atom "lambda" : List params : body)) = 
    makeNormalFunc env params body
eval env (List (Atom "lambda" : DottedList params varargs : body)) = 
    makeVarArgs varargs env params body
eval env (List (Atom "lambda" : varargs@(Atom _) : body)) = 
    makeVarArgs varargs env [] body
eval env (List [Atom "load", String filename]) = 
    load filename >>= liftM last . mapM (eval env)
eval env (List (function : args)) = do
    func <- eval env function
    argVals <- mapM (eval env) args
    apply func argVals
eval env badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

-- Application

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (IOFunc func) args = func args
apply (Func params varargs body closure) args = 
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

-- Helper functions

evalIf :: Env -> LispVal -> LispVal -> LispVal -> IOThrowsError LispVal
evalIf env predicate conseq alt = do
    result <- eval env predicate
    case result of
        Bool True  -> eval env conseq
        Bool False -> eval env alt
        notBool    -> throwError $ TypeMismatch "boolean" notBool

evalCond :: Env -> [LispVal] -> IOThrowsError LispVal
evalCond env [] = throwError $ NumArgs 1 [List []]
evalCond env [List [Atom "else", expr]] = eval env expr
evalCond env (List [predicate, expr] : rest) = do
    result <- eval env predicate
    case result of
        Bool True  -> eval env expr
        Bool False -> evalCond env rest
        notBool    -> throwError $ TypeMismatch "boolean" notBool

evalQuasiquote :: Env -> LispVal -> IOThrowsError LispVal
evalQuasiquote env (List [Atom "unquote", val]) = eval env val
evalQuasiquote env (List vals) = liftM List $ mapM (evalQuasiquote env) vals
evalQuasiquote env val = return val
{-  This doesn't work at the moment:
    * Doesn't deal with unquote-splicing
    * Doesn't deal with nested levels of quote/quasiquote -}

makeFunc :: (Monad m) => Maybe String -> Env -> [LispVal] -> [LispVal] -> m LispVal
makeFunc varargs env params body = return $ Func (map showVal params) varargs body env

makeNormalFunc :: (Monad m) => Env -> [LispVal] -> [LispVal] -> m LispVal
makeNormalFunc = makeFunc Nothing

makeVarArgs :: (Monad m) => LispVal -> Env -> [LispVal] -> [LispVal] -> m LispVal
makeVarArgs = makeFunc . Just . showVal

load :: String -> IOThrowsError [LispVal]
load filename = (liftIO $ readFile filename) >>= liftThrows . readExprList
