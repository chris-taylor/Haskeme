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

runOne :: [String] -> IO ()
runOne args = do
    env <- primitiveBindings >>= flip bindVars [("args", List $ map String $ drop 1 args)]
    (runIOThrows $ liftM show $ eval env (List [Atom "load", String (args !! 0)]))
        >>= hPutStrLn stderr

runRepl :: IO ()
runRepl = showHeader >> primitiveBindings >>= untilM_ (== "quit") (readPrompt "haskeme> ") . evalAndPrint

-- IO Functions

showHeader :: IO ()
showHeader = do
    putStrLn " _               _"
    putStrLn "| |_   ___  ___ | |__ ___  _____  ___"
    putStrLn "|    \\/ _ \\/ __/| | // _ \\|     \\/ _ \\"
    putStrLn "| || | |_| \\__ \\|   \\  __/| | | |  __/"
    putStrLn "|_||_|\\__/\\|___/|_|\\_\\___/|_|_|_|\\___/"

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

-- Macro expansion and evaluation

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = if null expr
    then return ()
    else evalExpr env expr >>= putStrLn

evalExpr :: Env -> String -> IO String
evalExpr env expr = runIOThrows $ liftM show $ (liftThrows $ readExpr expr) >>= eval env

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
