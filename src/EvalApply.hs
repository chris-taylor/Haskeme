module EvalApply (eval, apply, load) where

import Control.Monad.Error
import Data.Array
import qualified Data.Map as Map

import LispVal
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
eval env val@(Vector _)  = return val
eval env val@(Hash _)    = return val
eval env (Atom name)     = getVar env name
eval env (List [Atom "quote", val]) = return val
eval env (List [Atom "quasiquote", val]) = evalQuasiquote 1 env val
eval env (List (Atom "do" : exprs)) = evalDo env exprs
eval env (List (Atom "if" : exprs)) = evalIf env exprs
eval env (List (Atom "case" : key : clauses)) = evalCase env key clauses
eval env (List (Atom "=" : args)) = evalSet env args
eval env (List (Atom "load" : params)) = evalLoad env params
eval env (List (Atom "def" : var : rest)) = evalDefine env var rest
eval env (List (Atom "macro" : mac : rest)) = evalDefineMacro env mac rest
eval env (List (Atom "fn" : params : body)) = evalLambda env params body
eval env (List (function : args)) = evalApplication env function args
eval env badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

-- Application

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (IOFunc func) args = func args
apply (Func  params varargs body closure) args = applyFunc params varargs body closure args
apply (Macro params varargs body closure) args = applyFunc params varargs body closure args
apply (String str) args = applyString str args
apply (Vector arr) args = applyVector arr args
apply (Hash hash) args  = applyHash hash args

applyFunc :: [String] -> Maybe String -> [LispVal] -> Env -> [LispVal] -> IOThrowsError LispVal
applyFunc params varargs body closure args = 
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

applyString :: String -> [LispVal] -> IOThrowsError LispVal
applyString str [Number n] = let index = fromInteger n in
    if index < length str
        then return $ Char $ str !! index
        else throwError $ OutOfRange index (0, length str - 1) (String str)
applyString str [arg]      = throwError $ TypeMismatch "integer" arg
applyString str args       = throwError $ NumArgs 1 args

applyVector :: (Array Int LispVal) -> [LispVal] -> IOThrowsError LispVal
applyVector arr [Number n] = let index = fromInteger n in
    if index < (snd $ bounds arr) + 1
        then return $ arr ! index
        else throwError $ OutOfRange index (bounds arr) (Vector arr)
applyVector arr [arg]      = throwError $ TypeMismatch "integer" arg
applyVector arr args       = throwError $ NumArgs 1 args

applyHash :: (Map.Map LispVal LispVal) -> [LispVal] -> IOThrowsError LispVal
applyHash hash [key] = maybe (throwError $ KeyNotFound key $ Hash hash)
                             (\value -> return value)
                             (Map.lookup key hash)
applyHash hash args  = throwError $ NumArgs 1 args

-- Evaluation of special forms

evalApplication :: Env -> LispVal -> [LispVal] -> IOThrowsError LispVal
evalApplication env function args = do
    func <- eval env function
    case func of 
        (Macro {}) -> apply func args >>= eval env
        _          -> mapM (eval env) args >>= apply func

evalDefine :: Env -> LispVal -> [LispVal] -> IOThrowsError LispVal
evalDefine env (Atom var) [form] =
    eval env form >>= defineVar env var
evalDefine env (List (Atom var : params)) body = 
    makeNormalFunc env params body >>= defineVar env var
evalDefine env (DottedList (Atom var : params) varargs) body =
    makeVarArgs varargs env params body >>= defineVar env var

evalDefineMacro :: Env -> LispVal -> [LispVal] -> IOThrowsError LispVal
evalDefineMacro env (List (Atom var : params)) body = 
    makeNormalMacro env params body >>= defineVar env var
evalDefineMacro env (DottedList (Atom var : params) varargs) body =
    makeVarArgsMacro varargs env params body >>= defineVar env var

evalLambda :: Env -> LispVal -> [LispVal] -> IOThrowsError LispVal
evalLambda env (List params) body = makeNormalFunc env params body
evalLambda env (DottedList params varargs) body = makeVarArgs varargs env params body
evalLambda env varargs@(Atom _) body = makeVarArgs varargs env [] body

evalDo :: Env -> [LispVal] -> IOThrowsError LispVal
evalDo env [] = throwError $ NumArgs 1 [List []]
evalDo env [expr] = eval env expr
evalDo env (expr : rest) = eval env expr >> evalDo env rest

evalIf :: Env -> [LispVal] -> IOThrowsError LispVal
evalIf env args@(test : rest) = do
    it <- eval env test
    newEnv <- bindLocals env ("it", it)
    evalIf' newEnv (truthVal it) rest

evalIf' :: Env -> Bool -> [LispVal] -> IOThrowsError LispVal
evalIf' env truth [conseq]        = evalIf' env truth [conseq, Bool False]
evalIf' env truth [conseq, alt]   = if truth then eval env conseq else eval env alt
evalIf' env truth (conseq : rest) = if truth then eval env conseq else evalIf env rest

bindOne :: Env -> (String, LispVal) -> IOThrowsError Env
bindOne env (var, val) = liftIO $ bindVars env [(var, val)]

evalCase :: Env -> LispVal -> [LispVal] -> IOThrowsError LispVal
evalCase env key clauses = do
    result <- eval env key
    evalCase' env result clauses

evalCase' :: Env -> LispVal -> [LispVal] -> IOThrowsError LispVal
evalCase' env key [] = throwError $ NumArgs 1 [List []]
evalCase' env key [List [Atom "else", expr]] = eval env expr
evalCase' env key (List [obj, expr] : rest) = do
    result <- eval env obj
    if eqv key result
        then eval env expr
        else evalCase' env key rest

evalSet :: Env -> [LispVal] -> IOThrowsError LispVal
evalSet env [Atom var, form] = eval env form >>= setVar env var
evalSet env [List [Atom "car", Atom var], form] = eval env form >>= setCar env var
evalSet env [List [Atom "cdr", Atom var], form] = eval env form >>= setCdr env var
evalSet env [List [Atom var, form1], form2] = do
    key <- eval env form1
    val <- eval env form2
    setIndex env var key val
evalSet env [other, _] = throwError $ TypeMismatch "atom, list" other
evalSet env badArgs = throwError $ NumArgs 2 badArgs

evalQuasiquote :: Int -> Env -> LispVal -> IOThrowsError LispVal
evalQuasiquote 0 env val = eval env val
evalQuasiquote n env (List [Atom "quasiquote", val]) = evalQuasiquote (n+1) env val
evalQuasiquote n env (List [Atom "unquote", val])    = evalQuasiquote (n-1) env val
evalQuasiquote n env (List vals) = liftM (List . concat) $ mapM (eqqList n env) vals
evalQuasiquote n env val = return val

eqqList :: Int -> Env -> LispVal -> IOThrowsError [LispVal]
eqqList 0 env val = liftM return $ eval env val
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
        (String filename) -> load filename >>= liftM last . mapM (eval env)
        other             -> throwError $ TypeMismatch "string" other
evalLoad env args = throwError $ NumArgs 1 args

-- Helper functions

truthVal :: LispVal -> Bool
truthVal (Bool False) = False
truthVal (Number 0)   = False
truthVal (Ratio 0)    = False
truthVal (Float 0)    = False
truthVal (Complex 0)  = False
truthVal (String "")  = False
truthVal (List [])    = False
truthVal (Vector arr) = let (_, n) = bounds arr in n > 0
truthVal _            = True

load :: String -> IOThrowsError [LispVal]
load filename = (liftIO $ readFile filename) >>= liftThrows . readExprList
