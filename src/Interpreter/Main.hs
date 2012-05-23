module Main where

import System.Environment
import Control.Monad.Error
--import Control.Monad.Reader
--import Control.Monad.State
import System.IO
import System.Info
import Data.IORef

import Language.Core
import Language.Types
import Language.Parser
import Language.Primitives
import Language.Variables

import Paths_haskeme
import Data.Version (showVersion)

-- Config

data ConfigImpl = ConfigImpl { prompt :: String }
type Config = IORef ConfigImpl

defaultConfig :: IO Config
defaultConfig = newIORef $ ConfigImpl { prompt = defaultPrompt }

showHeader :: IO ()
showHeader = do
    putStrLn $ " _               _"
    putStrLn $ "| |_   ___  ___ | |__ ___  _____  ___"
    putStrLn $ "|    \\/ _ \\/ __/| | // _ \\|     \\/ _ \\"
    putStrLn $ "| || | |_| \\__ \\|   \\  __/| | | |  __/"
    putStrLn $ "|_||_|\\__/\\|___/|_|\\_\\___/|_|_|_|\\___/"
    putStrLn $ "   Author: github.com/chris-taylor"
    putStrLn $ "  Version: " ++ showVersion version
    putStrLn $ ""

showMsg :: IO ()
showMsg = putStrLn $ "Welcome to Haskeme. Enter :quit to quit.\n"

defaultPrompt :: String
defaultPrompt = "haski> "

quitCmds :: [String]
quitCmds = [":q",":quit"]

-- Main

main :: IO ()
main = do
    args <- getArgs
    if null args then runReplM else runOneM $ args

-- Monadic interpreter

runOneM :: [String] -> IO ()
runOneM args = do
    env <- primitiveBindings
    loadLibraries env
    extendEnv [("args", List $ map String $ drop 1 args)] env
    let thunk = meval (List [Atom "load", String (args !! 0)])
    let evaledExpr = run thunk env
    result <- runIOThrows $ liftM show $ evaledExpr
    hPutStrLn stderr result

runReplM :: IO ()
runReplM = do
    showHeader
    cfg <- defaultConfig
    env <- primitiveBindings
    loadLibraries env
    showMsg
    replLoop cfg env

replLoop :: Config -> Env -> IO ()
replLoop cfg env = untilM_ (`elem` quitCmds) (readPrompt cfg) (handle cfg env)

handle :: Config -> Env -> String -> IO ()
handle cfg env expr = case expr of
    ""        -> return ()
    ':' : cmd -> handleCommand cfg cmd
    _         -> evalPrint env expr

loadLibraries :: Env -> IO ()
loadLibraries env = do
    stdlib <- getDataFileName $ if os == "mingw32"
        then "lib\\stdlib.scm"
        else "lib/stdlib.scm"
    putStrLn $ "Loading stdlib from " ++ stdlib ++ "\n"
    evalString env $ "(load  \"" ++ (escapeBackslashes stdlib) ++ "\")"

-- IO Functions

handleCommand :: Config -> String -> IO ()
handleCommand cfgRef cmd = case cmd of
    'p' : args -> writeIORef cfgRef $ ConfigImpl { prompt = strip args ++ " " }
    cmd : args -> putStrLn $ "Unknown command " ++ [cmd]

readPrompt :: Config -> IO String
readPrompt cfgRef = do
    cfg <- liftIO $ readIORef cfgRef
    flushStr (prompt cfg) >> getLine

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

-- Macro expansion and evaluation

evalPrint :: Env -> String -> IO ()
evalPrint env expr = evalExpr env expr >>= putStrLn

evalString :: Env -> String -> IO ()
evalString env expr = evalExpr env expr >> return ()

evalExpr :: Env -> String -> IO String
evalExpr env expr = do
    let parsedExpr = lift . liftThrows $ readExpr expr
    let evaledExpr = parsedExpr >>= meval
    --runIOThrows $ liftM show $ run evaledExpr env

    p <- runIOThrows $ liftM show $ run parsedExpr env
    putStrLn $ "***Reading: " ++ p
    e <- runIOThrows $ liftM show $ run evaledExpr env
    putStrLn $ "***Result:  " ++ e
    
    return e

-- Utility functions

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

lstrip :: String -> String
lstrip = dropWhile (`elem` " \t")

rstrip :: String -> String
rstrip = reverse . lstrip . reverse

strip :: String -> String
strip = lstrip . rstrip