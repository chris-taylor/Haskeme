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

-- Main

main :: IO ()
main = do
    args <- getArgs
    if null args then runRepl else runOne $ args

-- IO Functions

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ liftM show $ (liftThrows $ readExpr expr) >>= eval env

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = if null expr
    then return ()
    else evalString env expr >>= putStrLn

primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= (flip bindVars $ map (makeFunc IOFunc) ioPrimitives
                                              ++ map (makeFunc PrimitiveFunc) primitives)
    where makeFunc constructor (var, func) = (var, constructor func)

runOne :: [String] -> IO ()
runOne args = do
    env <- primitiveBindings >>= flip bindVars [("args", List $ map String $ drop 1 args)]
    (runIOThrows $ liftM show $ eval env (List [Atom "load", String (args !! 0)]))
        >>= hPutStrLn stderr

runRepl :: IO ()
runRepl = primitiveBindings >>= untilM_ (== "quit") (readPrompt "haskeme> ") . evalAndPrint

-- Monadic looping

untilM_ :: (Monad m) => (a -> Bool) -> m a -> (a -> m ()) -> m ()
untilM_ predicate prompt action = do
    result <- prompt
    if predicate result
        then return ()
        else action result >> untilM_ predicate prompt action
