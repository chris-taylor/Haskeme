module Main where

import System.Environment
import Control.Monad.Error
import IO hiding (try)

import Language.LispVal
import Language.LispParser
import Language.Primitives
import Language.IOPrimitives
import Language.Variables
import Language.EvalApply

import Paths_haskeme

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
    showHeader
    env <- primitiveBindings
    loadLibraries env
    untilM_ (== "quit") (readPrompt "haskeme> ") (evalAndPrint env)

loadLibraries :: Env -> IO ()
loadLibraries env = do
    stdlib <- getDataFileName "lib/stdlib.scm"
    putStrLn $ "Loading library from " ++ stdlib
    evalString env $ "(load  \"" ++ (escapeBackslashes stdlib) ++ "\")"

-- IO Functions

versionNum :: String
versionNum = "0.1"

showHeader :: IO ()
showHeader = do
    putStrLn $ " _               _"
    putStrLn $ "| |_   ___  ___ | |__ ___  _____  ___"
    putStrLn $ "|    \\/ _ \\/ __/| | // _ \\|     \\/ _ \\"
    putStrLn $ "| || | |_| \\__ \\|   \\  __/| | | |  __/"
    putStrLn $ "|_||_|\\__/\\|___/|_|\\_\\___/|_|_|_|\\___/"
    putStrLn $ "   Author: github.com/chris-taylor"
    putStrLn $ "  Version: " ++ versionNum

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

-- Macro expansion and evaluation

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = if null expr
    then return ()
    else evalExpr env expr >>= putStrLn

evalString :: Env -> String -> IO ()
evalString env expr = evalExpr env expr >> return ()

evalExpr :: Env -> String -> IO String
evalExpr env expr = runIOThrows $ liftM show $ (liftThrows $ readExpr expr) >>=
    macroExpand env >>= eval env

-- Environments

primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= (flip bindVars $ map (makeFunc IOFunc) ioPrimitives
                                              ++ map (makeFunc PrimitiveFunc) primitives)
    where makeFunc constructor (var, func) = (var, constructor var func)

-- Monadic looping

untilM_ :: (Monad m) => (a -> Bool) -> m a -> (a -> m ()) -> m ()
untilM_ predicate prompt action = do
    result <- prompt
    if predicate result
        then return ()
        else action result >> untilM_ predicate prompt action

escapeBackslashes :: String -> String
escapeBackslashes str = replace str "\\" "\\\\"

replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace [] _ _ = []
replace s find repl =
    if take (length find) s == find
        then repl ++ (replace (drop (length find) s) find repl)
        else [head s] ++ (replace (tail s) find repl)