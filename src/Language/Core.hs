module Language.Core (mevalM, mapply, expandMOne, expandMAll) where

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

mevalM :: LispVal -> EvalM LispVal
mevalM form = macroExpandM form >>= evalM 

-- Evaluator

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
evalM val@(Atom var)  = getVar var
evalM (List (Atom "quote" : args))       = evalMQuote args
evalM (List [Atom "quasiquote",  args])  = evalMQQ 1 args
evalM (List (Atom "if" : args))          = evalMIf args
evalM (List (Atom "fn" : params : body)) = evalMFn params body
evalM (List (Atom "def" : var : rest))   = evalMDefine defineVar var rest
evalM (List (Atom "macro" : var : rest)) = evalMDefine defineMacro var rest
evalM (List (Atom "=" : args))           = evalMSet args
evalM (List (Atom "try" : args))         = evalMTry args
evalM (List (function : args))           = evalMApplication function args
evalM badForm = lift $ throwError $ BadSpecialForm "Unrecognized special form" badForm

evalMApplication :: LispVal -> [LispVal] -> EvalM LispVal
evalMApplication function args = do
    func <- mevalM function
    mapM mevalM args >>= mapply func

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
evalMIf args@(test : rest) = do it <- mevalM test
                                bindM ("it", it)
                                evalMIf' (truthVal it) rest

evalMIf' :: Bool -> [LispVal] -> EvalM LispVal
evalMIf' truth [conseq]        = evalMIf' truth [conseq, lispFalse]
evalMIf' truth [conseq, alt]   = if truth then evalU conseq else evalU alt
evalMIf' truth (conseq : rest) = if truth then evalU conseq else unbindM "it" >> evalMIf rest

evalU :: LispVal -> EvalM LispVal
evalU expr = do
    result <- mevalM expr
    unbindM "it"
    return result

evalMFn :: LispVal -> [LispVal] -> EvalM LispVal
evalMFn (List params) body = makeNormalFuncM params body
evalMFn (DottedList params varargs) body = makeVarArgsM varargs params body
evalMFn varargs@(Atom _) body = makeVarArgsM varargs [] body

evalMDefine :: (String -> LispVal -> EvalM LispVal) -> LispVal -> [LispVal] -> EvalM LispVal
evalMDefine definer (Atom var) [form] = mevalM form >>= definer var
evalMDefine definer (Atom var) (List params : body) = 
    makeNormalFuncM params body >>= definer var
evalMDefine definer (Atom var) (DottedList params varargs : body) = 
    makeVarArgsM varargs params body >>= definer var
evalMDefine definer (Atom var) (varargs@(Atom _) : body) = 
    makeVarArgsM varargs [] body >>= definer var

evalMSet :: [LispVal] -> EvalM LispVal
evalMSet [Atom var, form] = mevalM form >>= setVar var
evalMSet [List [Atom "car", Atom var], form] = mevalM form >>= setCar var
evalMSet [List [Atom "cdr", Atom var], form] = mevalM form >>= setCdr var
evalMSet [List [Atom var, form1], form2]     = do key <- mevalM form1
                                                  val <- mevalM form2
                                                  setIndex var key val
evalMSet [other, _] = lift $ errTypeMismatch "atom or list" other
evalMSet badArgs    = lift $ errNumArgs 2 badArgs

evalMTry :: [LispVal] -> EvalM LispVal
evalMTry [expr, handler] = mevalM expr `catchError` \err ->
    mevalM handler >>= flip mapply [Exception err]

-- Macro expansion

macroExpandM :: LispVal -> EvalM LispVal
macroExpandM val@(List _) = expandMAll val
macroExpandM val          = return val

genericExpandM :: (LispVal -> EvalM LispVal) -> LispVal -> EvalM LispVal
genericExpandM continue val@(List (Atom name : args)) = do
    env <- getEnv
    result <- liftIO $ macRecLookup name env
    maybe (return val)
          (\macroRef -> do
            macro <- liftIO $ readIORef macroRef
            mapply macro args >>= continue)
          (result)
genericExpandM _ val = return val

expandMOne :: LispVal -> EvalM LispVal
expandMOne = genericExpandM return

expandMAll :: LispVal -> EvalM LispVal
expandMAll = genericExpandM macroExpandM

-- Helper functions to create user procedures

makeFuncM :: Maybe String -> [LispVal] -> [LispVal] -> EvalM LispVal
makeFuncM varargs params body = do
    expandedBody <- mapM macroExpandM body
    getEnv >>= return . Func (map showVal params) varargs expandedBody

makeNormalFuncM :: [LispVal] -> [LispVal] -> EvalM LispVal
makeNormalFuncM = makeFuncM Nothing

makeVarArgsM :: LispVal -> [LispVal] -> [LispVal] -> EvalM LispVal
makeVarArgsM varargs = makeFuncM (Just $ showVal varargs)

-- Application

mapply :: LispVal -> [LispVal] -> EvalM LispVal
mapply (PrimitiveFunc _ func) args = lift $ liftThrows $ func args
mapply (IOFunc _ func) args        = lift $ func args
mapply (EvalFunc _ func) args      = func args
mapply (Func  params varargs body closure) args = applyFuncM params varargs body closure args
mapply (String str) args = lift $ applyString str args
mapply (Vector arr) args = lift $ applyVector arr args
mapply (Hash hash) args  = lift $ applyHash hash args
mapply other _ = lift $ errTypeMismatch "procedure, macro, string, vector or hash" other

applyFuncM :: [String] -> Maybe String -> [LispVal] -> Env -> [LispVal] -> EvalM LispVal
applyFuncM params varargs body closure args = inEnv closure' $
    if num params /= num args && varargs == Nothing
        then lift $ errNumArgs (num params) args
        else liftM last (mapM mevalM body)
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
