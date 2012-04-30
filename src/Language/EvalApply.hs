module Language.EvalApply (eval, apply, meval, load) where

import Control.Monad.Error
import Data.Array
import Data.IORef
import qualified Data.Map as Map

import Language.LispVal
import Language.LispParser
import Language.Variables

-- Evaluation

eval :: Env -> LispVal -> IOThrowsError LispVal
eval env val@(String _)  = return val
eval env val@(Char _)    = return val
eval env val@(Number _)  = return val
eval env val@(Ratio _)   = return val
eval env val@(Float _)   = return val
eval env val@(Complex _) = return val
eval env val@(Bool _)    = return val
eval env val@(Vector _)  = return val
eval env val@(Hash _)    = return val
eval env (Atom name)     = getVar env name
eval env (List [Atom "quote", val]) = return val
eval env (List [Atom "quasiquote", val]) = evalQuasiquote 1 env val
eval env (List (Atom "if" : exprs)) = evalIf env exprs
eval env (List (Atom "=" : args)) = evalSet env args
eval env (List (Atom "def" : var : rest)) = evalDefine env var rest
eval env (List (Atom "macro" : params : body)) = evalDefineMacro env params body
eval env (List (Atom "fn" : params : body)) = evalLambda env params body
eval env (List (Atom "load" : params)) = evalLoad env params
eval env (List (Atom "uniq" : rest)) = genUniqueSym env rest
-- Here for debugging
eval env (List [Atom "expand", code]) = expandThenEval env code >>= meval env
eval env (List [Atom "show-env", namespace]) = showNamespace env namespace
-- End debug section
eval env (List (function : args)) = evalApplication env function args
eval env badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

evalApplication :: Env -> LispVal -> [LispVal] -> IOThrowsError LispVal
evalApplication env function args = do
    func <- expandThenEval env function
    mapM (expandThenEval env) args >>= apply func

evalDefine :: Env -> LispVal -> [LispVal] -> IOThrowsError LispVal
evalDefine env (Atom var) [form] =
    expandThenEval env form >>= defineVar env var
evalDefine env (List (Atom var : params)) body = 
    makeNormalFunc env params body >>= defineVar env var
evalDefine env (DottedList (Atom var : params) varargs) body =
    makeVarArgs varargs env params body >>= defineVar env var

evalDefineMacro :: Env -> LispVal -> [LispVal] -> IOThrowsError LispVal
evalDefineMacro env (List (Atom var : params)) body = 
    makeNormalMacro env params body >>= defineMacro env var
evalDefineMacro env (DottedList (Atom var : params) varargs) body =
    makeVarArgsMacro varargs env params body >>= defineMacro env var

evalLambda :: Env -> LispVal -> [LispVal] -> IOThrowsError LispVal
evalLambda env (List params) body = makeNormalFunc env params body
evalLambda env (DottedList params varargs) body = makeVarArgs varargs env params body
evalLambda env varargs@(Atom _) body = makeVarArgs varargs env [] body

evalIf :: Env -> [LispVal] -> IOThrowsError LispVal
evalIf env args@(test : rest) = do
    it <- expandThenEval env test
    newEnv <- bindOne env ("it", it)
    evalIf' newEnv (truthVal it) rest

evalIf' :: Env -> Bool -> [LispVal] -> IOThrowsError LispVal
evalIf' env truth [conseq]        = evalIf' env truth [conseq, Bool False]
evalIf' env truth [conseq, alt]   = if truth
    then expandThenEval env conseq
    else expandThenEval env alt
evalIf' env truth (conseq : rest) = if truth
    then expandThenEval env conseq
    else evalIf env rest

evalSet :: Env -> [LispVal] -> IOThrowsError LispVal
evalSet env [Atom var, form] = expandThenEval env form >>= setVar env var
evalSet env [List [Atom "car", Atom var], form] = expandThenEval env form >>= setCar env var
evalSet env [List [Atom "cdr", Atom var], form] = expandThenEval env form >>= setCdr env var
evalSet env [List [Atom var, form1], form2] = do
    key <- expandThenEval env form1
    val <- expandThenEval env form2
    setIndex env var key val
evalSet env [other, _] = throwError $ TypeMismatch "atom or list" other
evalSet env badArgs = throwError $ NumArgs 2 badArgs

evalQuasiquote :: Int -> Env -> LispVal -> IOThrowsError LispVal
evalQuasiquote 0 env val = expandThenEval env val
evalQuasiquote n env (List [Atom "quasiquote", val]) = evalQuasiquote (n+1) env val
evalQuasiquote n env (List [Atom "unquote", val])    = evalQuasiquote (n-1) env val
evalQuasiquote n env (List vals) = liftM (List . concat) $ mapM (eqqList n env) vals
evalQuasiquote n env val = return val

eqqList :: Int -> Env -> LispVal -> IOThrowsError [LispVal]
eqqList 0 env val = liftM return $ expandThenEval env val
eqqList n env (List [Atom "quasiquote", val]) = liftM return $ evalQuasiquote (n+1) env val
eqqList n env (List [Atom "unquote", val])    = liftM return $ evalQuasiquote (n-1) env val
eqqList n env (List [Atom "unquotesplicing", val]) = do
    result <- evalQuasiquote (n-1) env val
    case result of
        (List xs) -> return xs
        notList   -> throwError $ TypeMismatch "list" notList
eqqList n env (List vals) = liftM (return . List . concat) $ mapM (eqqList n env) vals
eqqList n env val = return $ return val

evalLoad :: Env -> [LispVal] -> IOThrowsError LispVal
evalLoad env [arg] = do
    result <- eval env arg
    case result of
        (String filename) -> load filename >>= liftM last . mapM (expandThenEval env)
        other             -> throwError $ TypeMismatch "string" other
evalLoad env args = throwError $ NumArgs 1 args

-- Helper functions to create user functions and macros

type ProcConstructor = [String] -> Maybe String -> [LispVal] -> Env -> LispVal

makeProc :: ProcConstructor -> Maybe String -> Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeProc constructor varargs env params body = do
    body' <- mapM (meval env) body
    return $ constructor (map showVal params) varargs body' env

makeNormalFunc :: Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeNormalFunc = makeProc Func Nothing

makeVarArgs :: LispVal -> Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeVarArgs varargs = makeProc Func (Just $ showVal varargs)

makeNormalMacro :: Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeNormalMacro = makeProc Macro Nothing

makeVarArgsMacro :: LispVal -> Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeVarArgsMacro varargs = makeProc Macro (Just $ showVal varargs)

-- Macro expansion

meval :: Env -> LispVal -> IOThrowsError LispVal
{-  Macro evaluation takes two environments: the normal environment containing
    all primitives and user definitions, and a special macro environment that
    contains only macro forms. Evaluation works as follows:
    - If the form is a list with an atom as the first element, then check if
      that atom has a binding in the macro environment. If it does, then apply
      the macro to the remaining elements of the list **after macro expanding,
      but not evaluating, them**
    - If the form is an atom, check if it has a binding in the macro
      environment. If so, then quote and return the associated macro, otherwise
      return the form.
    - If the form is anything other than a list or an atom, then return it. -}
meval env val@(List (name : args)) = expandApplication env val
meval env val@(Atom name) = expandAtom env val
meval env val = return val

expandApplication :: Env -> LispVal -> IOThrowsError LispVal
expandApplication envRef val@(List (Atom name : args)) = do
    env <- liftIO $ readIORef envRef
    maybe (return val)
          (\macroRef -> do
            macro <- liftIO $ readIORef macroRef
            apply macro args >>= meval envRef)
          (macroLookup name env)
expandApplication envRef val = return val

expandAtom :: Env -> LispVal -> IOThrowsError LispVal
expandAtom envRef val@(Atom name) = do
    env <- liftIO $ readIORef envRef
    maybe (return val)
          (\macroRef -> do
            macro <- liftIO $ readIORef macroRef
            returnQuoted macro)
          (macroLookup name env)

-- Combined eval and expand

expandThenEval :: Env -> LispVal -> IOThrowsError LispVal
expandThenEval env val = meval env val >>= eval env

-- Application

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc _ func) args = liftThrows $ func args
apply (IOFunc _ func) args        = func args
apply (Func  params varargs body closure) args = applyFunc params varargs body closure args
apply (Macro params varargs body closure) args = applyFunc params varargs body closure args
apply (String str) args = applyString str args
apply (Vector arr) args = applyVector arr args
apply (Hash hash) args  = applyHash hash args
apply other _ = throwError $ TypeMismatch "procedure, macro, string, vector or hash" other

applyFunc :: [String] -> Maybe String -> [LispVal] -> Env -> [LispVal] -> IOThrowsError LispVal
applyFunc params varargs body closure args = 
    if num params /= num args && varargs == Nothing
        then throwError $ NumArgs (num params) args
        else (liftIO $ bindVars closure $ zip params args) >>= bindVarArgs varargs >>= evalBody
    where
        remainingArgs = drop (length params) args
        num = toInteger . length
        evalBody env = liftM last $ mapM (expandThenEval env) body
        bindVarArgs arg env = case arg of
            Just argName -> liftIO $ bindVars env [(argName, List $ remainingArgs)]
            Nothing -> return env

applyString :: String -> [LispVal] -> IOThrowsError LispVal
applyString str [Number n] = let index = fromInteger n in
    if index < length str
        then return $ Char $ str !! index
        else throwError $ OutOfRange index (0, length str - 1) (String str)
applyString str [arg]      = throwError $ TypeMismatch "integer" arg
applyString str args       = throwError $ NumArgs 1 args

applyVector :: VectorType -> [LispVal] -> IOThrowsError LispVal
applyVector arr [Number n] = let index = fromInteger n in
    if index < (snd $ bounds arr) + 1
        then return $ arr ! index
        else throwError $ OutOfRange index (bounds arr) (Vector arr)
applyVector arr [arg]      = throwError $ TypeMismatch "integer" arg
applyVector arr args       = throwError $ NumArgs 1 args

applyHash :: HashType -> [LispVal] -> IOThrowsError LispVal
applyHash hash [key] = maybe (throwError $ KeyNotFound key $ Hash hash)
                             (\value -> return value)
                             (Map.lookup key hash)
applyHash hash args  = throwError $ NumArgs 1 args

-- Helper functions

returnQuoted :: LispVal -> IOThrowsError LispVal
returnQuoted val = return $ List [Atom "quote", val]

bindOne :: Env -> (String, LispVal) -> IOThrowsError Env
bindOne env (var, val) = liftIO $ bindVars env [(var, val)]

load :: String -> IOThrowsError [LispVal]
load filename = (liftIO $ readFile filename) >>= liftThrows . readExprList
