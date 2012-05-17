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

--showHeader :: IO ()
--showHeader = do
--    putStrLn $ " _               _"
--    putStrLn $ "| |_   ___  ___ | |__ ___  _____  ___"
--    putStrLn $ "|    \\/ _ \\/ __/| | // _ \\|     \\/ _ \\"
--    putStrLn $ "| || | |_| \\__ \\|   \\  __/| | | |  __/"
--    putStrLn $ "|_||_|\\__/\\|___/|_|\\_\\___/|_|_|_|\\___/"
--    putStrLn $ "   Author: github.com/chris-taylor"
--    putStrLn $ "  Version: " ++ showVersion version
--    putStrLn $ ""

--showMsg :: IO ()
--showMsg = putStrLn $ "Welcome to Haskeme! Enter " ++ quitCmd ++ " to quit.\n"

defaultPrompt :: String
defaultPrompt = "haskm> "

quitCmd :: String
quitCmd = ":q"

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
    bindVars env [("args", List $ map String $ drop 1 args)]
    let thunk = mevalM (List [Atom "load", String (args !! 0)])
    let evaledExpr = run thunk env
    result <- runIOThrows $ liftM show $ evaledExpr
    hPutStrLn stderr result

runReplM :: IO ()
runReplM = do
    cfg <- defaultConfig
    env <- primitiveBindings
    loadLibraries env
    replM cfg env

replM :: Config -> Env -> IO ()
replM cfg env = untilM_ (== quitCmd) (readPrompt cfg) (handleM cfg env)

handleM :: Config -> Env -> String -> IO ()
handleM cfg env expr = case expr of
    ""        -> return ()
    ':' : cmd -> handleCommand cfg cmd
    _         -> evalMPrint env expr

loadLibraries :: Env -> IO ()
loadLibraries env = do
    stdlib <- getDataFileName $ if os == "mingw32"
        then "lib\\stdlib.scm"
        else "lib/stdlib.scm"
    putStrLn $ "Loading stdlib from " ++ stdlib ++ "\n"
    evalMString env $ "(load  \"" ++ (escapeBackslashes stdlib) ++ "\")"

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

evalMPrint :: Env -> String -> IO ()
evalMPrint env expr = evalMExpr env expr >>= putStrLn

evalMString :: Env -> String -> IO ()
evalMString env expr = evalMExpr env expr >> return ()

evalMExpr :: Env -> String -> IO String
evalMExpr env expr = do
    let parsedExpr = lift . liftThrows $ readExpr expr
    let evaledExpr = parsedExpr >>= mevalM
    runIOThrows $ liftM show $ run evaledExpr env

    --e <- runIOThrows $ liftM show $ run evaledExpr env
    --p <- runIOThrows $ liftM show $ run parsedExpr env
    --putStrLn $ "Reading: " ++ p
    --putStrLn $ "Result:  " ++ e
    --return e

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