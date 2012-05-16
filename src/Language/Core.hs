module Language.Core (eval, evalM, mevalM, macroExpand, apply, load) where

import Control.Monad.Error
import Control.Monad.State as S
import System.Directory (doesFileExist)
import Data.Array
import Data.IORef
import qualified Data.Map as Map

import Language.Types
import Language.Parser
import Language.Variables

-- Monadic functions

newRef :: a -> EvalM (IORef a)
newRef = lift . lift . newIORef

readRef :: IORef a -> EvalM a
readRef = lift . lift . readIORef

writeRef :: IORef a -> a -> EvalM ()
writeRef ref = lift . lift . writeIORef ref

-- 

bindM :: (Var, LispVal) -> EvalM ()
bindM (var, val) = do
    env <- getEnv
    ref <- newRef val
    bds <- readRef (bindings env)
    writeRef (bindings env) $ varInsert var ref bds

getEnv :: EvalM Env
getEnv = S.get

-- Monadic evaluation -- EXPERIMENTAL

mevalM :: LispVal -> EvalM LispVal
mevalM form = do
    env <- getEnv
    (lift $ macroExpand env form) >>= evalM

evalM :: LispVal -> EvalM LispVal
evalM val@(String _)  = return val
evalM val@(Number _)  = return val
evalM val@(Char _)    = return val
evalM val@(Ratio _)   = return val
evalM val@(Float _)   = return val
evalM val@(Complex _) = return val
evalM val@(Bool _)    = return val
evalM val@(Vector _)  = return val
evalM val@(Hash _)    = return val
evalM val@(Atom var)  = getEnv >>= lift . flip getVar var
evalM (List (Atom "quote" : args))       = evalMQuote args
evalM (List [Atom "quasiquote",  args])  = evalMQQ 1 args
evalM (List (Atom "if" : args))          = evalMIf args
evalM (List (Atom "fn" : params : body)) = evalMFn params body
evalM (List (Atom "def" : var : rest))   = evalMDefine defineVar var rest
evalM (List (Atom "macro" : var : rest)) = evalMDefine defineMacro var rest
evalM (List (Atom "=" : args))           = evalMSet args
evalM (List (Atom "try" : args))         = evalMTry args
evalM (List (Atom "load" : params))      = evalMLoad params
-- Here for debugging
evalM (List [Atom "expand", code])  = mevalM code >>= (\v -> do
    env <- getEnv
    lift $ expandAll env v)
evalM (List [Atom "expand1", code]) = mevalM code >>= (\v -> do
    env <- getEnv
    lift $ expandOne env v)
evalM (List [Atom "show-env", namespace]) = do
    env <- getEnv
    lift $ showNamespace env namespace
-- End debug section
evalM (List (function : args))           = evalMApplication function args
evalM badForm = lift $ throwError $ BadSpecialForm "Unrecognized special form" badForm

evalMApplication :: LispVal -> [LispVal] -> EvalM LispVal
evalMApplication function args = do
    func <- mevalM function
    mapM mevalM args >>= (\v -> lift $ apply func v)

evalMQuote :: [LispVal] -> EvalM LispVal
evalMQuote [val]   = return val
evalMQuote badArgs = lift $ errNumArgs 1 badArgs

evalMQQ :: Int -> LispVal -> EvalM LispVal
evalMQQ 0 val = mevalM val
evalMQQ n (List [Atom "quasiquote", val]) = evalMQQ (n+1) val
evalMQQ n (List [Atom "unquote", val])    = evalMQQ (n-1) val
evalMQQ n (List vals) = liftM (List . concat) $ mapM (evalMQQList n) vals
evalMQQ n val = return val

evalMQQList :: Int -> LispVal -> EvalM [LispVal]
--evalMQQList 0 val = liftM return $ mevalM env val
evalMQQList n (List [Atom "quasiquote", val]) = liftM return $ evalMQQ (n+1) val
evalMQQList n (List [Atom "unquote", val])    = liftM return $ evalMQQ (n-1) val
evalMQQList n (List [Atom "unquotesplicing", val]) = do
    result <- evalMQQ (n-1) val
    case result of
        (List xs) -> return xs
        notList   -> throwError $ TypeMismatch "list" notList
evalMQQList n (List vals) = liftM (return . List . concat) $ mapM (evalMQQList n) vals
evalMQQList n val = return $ return val

evalMIf :: [LispVal] -> EvalM LispVal
evalMIf args@(test : rest) = do
    it <- mevalM test
    bindM ("it", it)
    evalMIf' (truthVal it) rest

evalMIf' :: Bool -> [LispVal] -> EvalM LispVal
evalMIf' truth [conseq]        = evalMIf' truth [conseq, lispFalse]
evalMIf' truth [conseq, alt]   = if truth then mevalM conseq else mevalM alt
evalMIf' truth (conseq : rest) = if truth then mevalM conseq else evalMIf rest

evalMFn :: LispVal -> [LispVal] -> EvalM LispVal
evalMFn (List params) body = do
    env <- getEnv
    lift $ makeNormalFunc env params body
evalMFn (DottedList params varargs) body = do
    env <- getEnv
    lift $ makeVarArgs varargs env params body
evalMFn varargs@(Atom _) body = do
    env <- getEnv
    lift $ makeVarArgs varargs env [] body

evalMDefine :: (Env -> String -> LispVal -> IOThrowsError LispVal)
    -> LispVal -> [LispVal] -> EvalM LispVal
evalMDefine definer (Atom var) [form] = do
    env <- getEnv
    lift $ meval env form >>= definer env var
evalMDefine definer (Atom var) (List params : body) = do
    env <- getEnv
    lift $ makeNormalFunc env params body >>= definer env var
evalMDefine definer (Atom var) (DottedList params varargs : body) = do
    env <- getEnv
    lift $ makeVarArgs varargs env params body >>= definer env var
evalMDefine definer (Atom var) (varargs@(Atom _) : body) = do
    env <- getEnv
    lift $ makeVarArgs varargs env [] body >>= definer env var

evalMSet :: [LispVal] -> EvalM LispVal
evalMSet [Atom var, form] = do
    env <- getEnv
    lift $ meval env form >>= setVar env var
evalMSet [List [Atom "car", Atom var], form] = do
    env <- getEnv
    lift $ meval env form >>= setCar env var
evalMSet [List [Atom "cdr", Atom var], form] = do
    env <- getEnv
    lift $ meval env form >>= setCdr env var
evalMSet [List [Atom var, form1], form2] = do
    key <- mevalM form1
    val <- mevalM form2
    env <- getEnv
    lift $ setIndex env var key val
evalMSet [other, _] = lift $ errTypeMismatch "atom or list" other
evalMSet badArgs    = lift $ errNumArgs 2 badArgs

evalMTry :: [LispVal] -> EvalM LispVal
evalMTry [expr, handler] = do
    env <- getEnv
    lift $ meval env expr `catchError` \err ->
        meval env handler >>= flip apply [Exception err]

evalMLoad :: [LispVal] -> EvalM LispVal
evalMLoad [arg] = do
    result <- evalM arg
    case result of
        (String filename) -> (lift $ load filename) >>= liftM last . mapM mevalM
        other             -> lift $ errTypeMismatch "string" other
evalMLoad args  = lift $ errNumArgs 1 args

--evalLoad :: Env -> [LispVal] -> IOThrowsError LispVal
--evalLoad env [arg] = do
--    result <- eval env arg
--    case result of
--        (String filename) -> load filename >>= liftM last . mapM (meval env)
--        other             -> throwError $ TypeMismatch "string" other
--evalLoad env args = throwError $ NumArgs 1 args

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
eval env (List [Atom "quote", val])             = return val
eval env (List [Atom "quasiquote", val])        = evalQuasiquote 1 env val
eval env (List (Atom "if" : exprs))             = evalIf env exprs
eval env (List (Atom "=" : args))               = evalSet env args
eval env (List (Atom "def" : var : rest))       = evalDefine defineVar env var rest
eval env (List (Atom "macro" : var : rest))     = evalDefine defineMacro env var rest
eval env (List (Atom "fn" : params : body))     = evalLambda env params body
eval env (List (Atom "try" : args))             = evalTry env args
eval env (List (Atom "load" : params))          = evalLoad env params
-- Here for debugging
eval env (List [Atom "expand", code])  = meval env code >>= expandAll env
eval env (List [Atom "expand1", code]) = meval env code >>= expandOne env
eval env (List [Atom "show-env", namespace]) = showNamespace env namespace
-- End debug section
eval env (List (function : args)) = evalApplication env function args
eval env badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

evalApplication :: Env -> LispVal -> [LispVal] -> IOThrowsError LispVal
evalApplication env function args = do
    func <- meval env function
    mapM (meval env) args >>= apply func

evalDefine :: (Env -> String -> LispVal -> IOThrowsError LispVal)
    -> Env -> LispVal -> [LispVal] -> IOThrowsError LispVal
evalDefine definer env (Atom var) [form] =
    meval env form >>= definer env var
evalDefine definer env (Atom var) (List params : body) = 
    makeNormalFunc env params body >>= definer env var
evalDefine definer env (Atom var) (DottedList params varargs : body) = 
    makeVarArgs varargs env params body >>= definer env var
evalDefine definer env (Atom var) (varargs@(Atom _) : body) = 
    makeVarArgs varargs env [] body >>= definer env var

evalLambda :: Env -> LispVal -> [LispVal] -> IOThrowsError LispVal
evalLambda env (List params) body = makeNormalFunc env params body
evalLambda env (DottedList params varargs) body = makeVarArgs varargs env params body
evalLambda env varargs@(Atom _) body = makeVarArgs varargs env [] body

evalIf :: Env -> [LispVal] -> IOThrowsError LispVal
evalIf env args@(test : rest) = do
    it <- meval env test
    newEnv <- bindOne env ("it", it)
    evalIf' newEnv (truthVal it) rest

evalIf' :: Env -> Bool -> [LispVal] -> IOThrowsError LispVal
evalIf' env truth [conseq]        = evalIf' env truth [conseq, lispFalse]
evalIf' env truth [conseq, alt]   = if truth
    then meval env conseq
    else meval env alt
evalIf' env truth (conseq : rest) = if truth
    then meval env conseq
    else evalIf env rest

evalSet :: Env -> [LispVal] -> IOThrowsError LispVal
evalSet env [Atom var, form] = meval env form >>= setVar env var
evalSet env [List [Atom "car", Atom var], form] = meval env form >>= setCar env var
evalSet env [List [Atom "cdr", Atom var], form] = meval env form >>= setCdr env var
evalSet env [List [Atom var, form1], form2] = do
    key <- meval env form1
    val <- meval env form2
    setIndex env var key val
evalSet env [other, _] = throwError $ TypeMismatch "atom or list" other
evalSet env badArgs = throwError $ NumArgs 2 badArgs

evalQuasiquote :: Int -> Env -> LispVal -> IOThrowsError LispVal
evalQuasiquote 0 env val = meval env val
evalQuasiquote n env (List [Atom "quasiquote", val]) = evalQuasiquote (n+1) env val
evalQuasiquote n env (List [Atom "unquote", val])    = evalQuasiquote (n-1) env val
evalQuasiquote n env (List vals) = liftM (List . concat) $ mapM (eqqList n env) vals
evalQuasiquote n env val = return val

eqqList :: Int -> Env -> LispVal -> IOThrowsError [LispVal]
eqqList 0 env val = liftM return $ meval env val
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
        (String filename) -> load filename >>= liftM last . mapM (meval env)
        other             -> throwError $ TypeMismatch "string" other
evalLoad env args = throwError $ NumArgs 1 args

-- Code for user errors

evalTry :: Env -> [LispVal] -> IOThrowsError LispVal
evalTry env [expr, handler] = meval env expr
    `catchError` \err -> meval env handler >>= flip apply [Exception err]
evalTry env badArgs = throwError $ NumArgs 2 badArgs

-- Helper functions to create user procedures

makeFunc :: Maybe String -> Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeFunc varargs env params body = do
    body' <- mapM (macroExpand env) body
    return $ Func (map showVal params) varargs body' env

makeNormalFunc :: Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeNormalFunc = makeFunc Nothing

makeVarArgs :: LispVal -> Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeVarArgs varargs = makeFunc (Just $ showVal varargs)

-- Macro expansion

macroExpand :: Env -> LispVal -> IOThrowsError LispVal
{-  Macro expansion works as follows:
    - If the form is a list with an atom as the first element, then check if
      that atom has a binding in the macro namespace. If it does, then apply
      the macro to the remaining elements of the list **after macro expanding,
      but not evaluating, them**
    - If the form is anything other than a list, then return it unchanged. -}
macroExpand env val@(List _) = expandAll env val
--macroExpand env val@(Atom name) = expandAtom env val
macroExpand env val = return val

genericExpand :: (Env -> LispVal -> IOThrowsError LispVal) -> Env -> LispVal -> IOThrowsError LispVal
genericExpand continue env val@(List (Atom name : args)) = do
    result <- liftIO $ macRecLookup name env
    maybe (return val)
          (\macroRef -> do
            macro <- liftIO $ readIORef macroRef
            apply macro args >>= continue env)
          (result)
genericExpand _ _ val = return val

expandOne :: Env -> LispVal -> IOThrowsError LispVal
expandOne = genericExpand (\_ -> return)

expandAll :: Env -> LispVal -> IOThrowsError LispVal
expandAll = genericExpand macroExpand

expandAtom :: Env -> LispVal -> IOThrowsError LispVal
expandAtom env val@(Atom name) = do
    result <- liftIO $ macRecLookup name env
    maybe (return val)
          (\macroRef -> do
            macro <- liftIO $ readIORef macroRef
            returnQuoted macro)
          (result)

-- Combined eval and expand

meval :: Env -> LispVal -> IOThrowsError LispVal
meval env val = macroExpand env val >>= eval env

-- Application

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc _ func) args = liftThrows $ func args
apply (IOFunc _ func) args        = func args
apply (Func  params varargs body closure) args = applyFunc params varargs body closure args
apply (HFunc params varargs body closure) args = applyHFunc params varargs body closure args
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
        evalBody env = liftM last $ mapM (meval env) body
        bindVarArgs arg env = case arg of
            Just argName -> liftIO $ bindVars env [ (argName, List $ remainingArgs) ]
            Nothing      -> return env

applyHFunc :: [String] -> Maybe String -> (Env -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal) -> Env -> [LispVal] -> IOThrowsError LispVal
applyHFunc params varargs body closure args = 
    if num params /= num args && varargs == Nothing
        then throwError $ NumArgs (num params) args
        else (liftIO $ bindVars closure $ zip params args) >>= bindVarArgs varargs >>= (evalBody body)
    where
        remainingArgs = drop (length params) args
        num = toInteger . length
        evalBody body env = body env Nil Nothing
        bindVarArgs arg env = case arg of
            Just argName -> liftIO $ bindVars env [ (argName, List $ remainingArgs) ]
            Nothing      -> return env

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
load filename = do
    result <- liftIO $ doesFileExist filename
    if result
      then (liftIO $ readFile filename) >>= liftThrows . readExprList
      else throwError $ FileNotFound filename
