module Main where

import System.Environment
import Control.Monad.Error
import IO hiding (try)

import LispVal
import LispError
import LispParser
import Primitives
import IOPrimitives
import Variables
import EvalApply
import Macro

-- Main

main :: IO ()
main = do
    args <- getArgs
    if null args then runRepl else runOne $ args

runOne :: [String] -> IO ()
runOne args = do
    env <- primitiveBindings >>= flip bindVars [("args", List $ map String $ drop 1 args)]
    (runIOThrows $ liftM show $ eval env (List [Atom "load", String (args !! 0)]))
        >>= hPutStrLn stderr

runRepl :: IO ()
runRepl = do 
    evalEnv  <- primitiveBindings
    macroEnv <- nullEnv
    untilM_ (== "quit") (readPrompt "haskeme> ") (evalExpandAndPrint (evalEnv, macroEnv))

-- IO Functions

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

-- Eval

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ liftM show $ (liftThrows $ readExpr expr) >>= eval env

evalExpr :: (Env, Env) -> String -> IO String
evalExpr (env, macroEnv) expr = runIOThrows $ liftM show $ macroExpand (env, macroEnv) expr >>= eval env

evalExpandAndPrint :: (Env, Env) -> String -> IO ()
evalExpandAndPrint envs expr = if null expr
    then return ()
    else evalExpr envs expr >>= putStrLn

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = if null expr
    then return ()
    else evalString env expr >>= putStrLn

-- Macros

macroExpand :: (Env, Env) -> String -> IOThrowsError LispVal
macroExpand (env, macroEnv) expr = (liftThrows $ readExpr expr) >>= evalMacro (env, macroEnv)

-- Environments

primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= (flip bindVars $ map (makeFunc IOFunc) ioPrimitives
                                              ++ map (makeFunc PrimitiveFunc) primitives)
    where makeFunc constructor (var, func) = (var, constructor func)

-- Monadic looping

untilM_ :: (Monad m) => (a -> Bool) -> m a -> (a -> m ()) -> m ()
untilM_ predicate prompt action = do
    result <- prompt
    if predicate result
        then return ()
        else action result >> untilM_ predicate prompt action
