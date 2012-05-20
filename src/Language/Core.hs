module Language.Core (meval, apply, expandOne, expandAll) where

import Control.Monad.Error
import System.Directory (doesFileExist)
import System.IO.Unsafe (unsafePerformIO)
import Data.Array
import Data.IORef
import qualified Data.Map as Map

import Language.Types
import Language.Parser
import Language.Variables

-- Combined expand and eval

meval :: LispVal -> EvalM LispVal
meval form = expandAll form >>= eval 

-- Evaluator

eval :: LispVal -> EvalM LispVal
eval val@(String _)  = return val
eval val@(Number _)  = return val
eval val@(Char _)    = return val
eval val@(Ratio _)   = return val
eval val@(Float _)   = return val
eval val@(Complex _) = return val
eval val@(Bool _)    = return val
eval val@(Vector _)  = return val
eval val@(Hash _)    = return val
eval val@(Atom var)  = getVar var
eval (List (Atom "quote" : args))       = evalQuote args
eval (List [Atom "quasiquote",  args])  = evalQQ 1 args
eval (List (Atom "if" : args))          = evalIf args
eval (List (Atom "fn" : params : body)) = evalFn params body
eval (List (Atom "def" : var : rest))   = evalDefine defineVar var rest
eval (List (Atom "macro" : var : rest)) = evalDefine defineMacro var rest
eval (List (Atom "=" : args))           = evalSet args
eval (List (Atom "try" : args))         = evalTry args
eval (List (function : args))           = evalApplication function args
eval badForm = lift $ throwError $ BadSpecialForm "Unrecognized special form" badForm

evalApplication :: LispVal -> [LispVal] -> EvalM LispVal
evalApplication function args = do
    func <- meval function
    mapM meval args >>= apply func

evalQuote :: [LispVal] -> EvalM LispVal
evalQuote [val]   = return val
evalQuote badArgs = lift $ errNumArgs 1 badArgs

evalQQ :: Int -> LispVal -> EvalM LispVal
evalQQ 0 val = meval val
evalQQ n (List [Atom "quasiquote", val]) = evalQQ (n+1) val
evalQQ n (List [Atom "unquote", val])    = evalQQ (n-1) val
evalQQ n (List vals) = liftM (List . concat) $ mapM (evalQQList n) vals
evalQQ n val = return val

evalQQList :: Int -> LispVal -> EvalM [LispVal]
evalQQList n (List [Atom "quasiquote", val]) = liftM return $ evalQQ (n+1) val
evalQQList n (List [Atom "unquote", val])    = liftM return $ evalQQ (n-1) val
evalQQList n (List [Atom "unquotesplicing", val]) = do
    result <- evalQQ (n-1) val
    case result of
        (List xs) -> return xs
        notList   -> throwError $ TypeMismatch "list" notList
evalQQList n (List vals) = liftM (return . List . concat) $ mapM (evalQQList n) vals
evalQQList n val = return $ return val

evalIf :: [LispVal] -> EvalM LispVal
evalIf args@(test : rest) = do it <- meval test
                               bindM ("it", it)
                               evalIf' (truthVal it) rest

evalIf' :: Bool -> [LispVal] -> EvalM LispVal
evalIf' truth [conseq]        = evalIf' truth [conseq, lispFalse]
evalIf' truth [conseq, alt]   = if truth then evalU conseq else evalU alt
evalIf' truth (conseq : rest) = if truth then evalU conseq else unbindM "it" >> evalIf rest

evalU :: LispVal -> EvalM LispVal
evalU expr = do
    result <- meval expr
    unbindM "it"
    return result

evalFn :: LispVal -> [LispVal] -> EvalM LispVal
evalFn (List params) body = makeNormalFunc params body
evalFn (DottedList params varargs) body = makeVarArgs varargs params body
evalFn varargs@(Atom _) body = makeVarArgs varargs [] body

evalDefine :: (String -> LispVal -> EvalM LispVal) -> LispVal -> [LispVal] -> EvalM LispVal
evalDefine definer (Atom var) [form] = meval form >>= definer var
evalDefine definer (Atom var) (List params : body) = 
    makeNormalFunc params body >>= definer var
evalDefine definer (Atom var) (DottedList params varargs : body) = 
    makeVarArgs varargs params body >>= definer var
evalDefine definer (Atom var) (varargs@(Atom _) : body) = 
    makeVarArgs varargs [] body >>= definer var

evalSet :: [LispVal] -> EvalM LispVal
evalSet [Atom var, form] = meval form >>= setVar var
evalSet [List [Atom "car", Atom var], form] = meval form >>= setCar var
evalSet [List [Atom "cdr", Atom var], form] = meval form >>= setCdr var
evalSet [List [Atom var, form1], form2]     = do key <- meval form1
                                                 val <- meval form2
                                                 setIndex var key val
evalSet [other, _] = lift $ errTypeMismatch "atom or list" other
evalSet badArgs    = lift $ errNumArgs 2 badArgs

evalTry :: [LispVal] -> EvalM LispVal
evalTry [expr, handler] = meval expr `catchError` \err ->
    meval handler >>= flip apply [Exception err]

-- Macro expansion

genericExpand :: (LispVal -> EvalM LispVal) -> LispVal -> EvalM LispVal
genericExpand continue val@(List (Atom name : args)) = do
    env <- getEnv
    result <- liftIO $ macRecLookup name env
    maybe (return val)
          (\macroRef -> do
            macro <- liftIO $ readIORef macroRef
            apply macro args >>= continue)
          (result)
genericExpand _ val = return val

expandOne :: LispVal -> EvalM LispVal
expandOne = genericExpand return

expandAll :: LispVal -> EvalM LispVal
expandAll = genericExpand expandAll

-- Helper functions to create user procedures

makeFunc :: Maybe String -> [LispVal] -> [LispVal] -> EvalM LispVal
makeFunc varargs params body = do
    expandedBody <- mapM expandAll body
    getEnv >>= return . Func (map showVal params) varargs expandedBody

makeNormalFunc :: [LispVal] -> [LispVal] -> EvalM LispVal
makeNormalFunc = makeFunc Nothing

makeVarArgs :: LispVal -> [LispVal] -> [LispVal] -> EvalM LispVal
makeVarArgs varargs = makeFunc (Just $ showVal varargs)

-- Application

apply :: LispVal -> [LispVal] -> EvalM LispVal
apply (PrimitiveFunc _ func) args = lift $ liftThrows $ func args
apply (IOFunc _ func) args        = lift $ func args
apply (EvalFunc _ func) args      = func args
apply (Func  params varargs body closure) args = applyFunc params varargs body closure args
apply (String str) args = lift $ applyString str args
apply (Vector arr) args = lift $ applyVector arr args
apply (Hash hash) args  = lift $ applyHash hash args
apply other _ = lift $ errTypeMismatch "procedure, macro, string, vector or hash" other

applyFunc :: [String] -> Maybe String -> [LispVal] -> Env -> [LispVal] -> EvalM LispVal
applyFunc params varargs body closure args = inEnv closure' $
    if num params /= num args && varargs == Nothing
        then lift $ errNumArgs (num params) args
        else liftM last (mapM meval body)
    where closure' = bindAll params varargs args closure
          num = toInteger . length

bindAll :: [Var] -> Maybe Var -> [LispVal] -> Env -> Env
bindAll params varargs args closure = unsafePerformIO $ extendEnv bindings closure where
    bindings = zip params args ++ case varargs of
        Just argName -> [(argName, List $ remainingArgs)]
        Nothing      -> []
    remainingArgs = drop (length params) args

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
