module Main where

import System
import System.IO
import System.Info
import System.Console.GetOpt
import Control.Monad.Error

import Language.Compiler
import Language.Types
-- import Language.Variables
-- import qualified Language.Core as Core

import Paths_haskeme

-- Main

main :: IO ()
main = do
    showWarning
    -- Read cmd line args and process options
    args <- getArgs
    --let (flags, nonOpts, msgs) = getOpt RequireOrder options args
    let inFile = args !! 0
    let outFile = args !! 1
    process inFile outFile

--options :: [OptDescr Flag]
--options =
--    [ Option ['v'] ["version"] (NoArg Version) "show version number" ]

showWarning :: IO ()
showWarning = do
    putStrLn ""
    putStrLn "Haskeme compiler -- experimental!"
    putStrLn ""

process :: FilePath -> FilePath -> IO ()
process inFile outFile = do
    env <- liftIO nullEnv
    lib <- getLibPath
    result <- runIOThrowsCompile $ liftM show $ compileSchemeFile env lib inFile
    case result of
        Just errMsg -> putStrLn errMsg
        _ -> compileHaskellFile outFile

getLibPath :: IO String
getLibPath = getDataFileName $ if os == "mingw32"
    then "lib\\stdlib.scm"
    else "lib/stdlib.scm"

compileSchemeFile :: Env -> FilePath -> FilePath -> IOThrowsError LispVal
compileSchemeFile env stdlibFile inFile = do
    -- libC <- compileLisp env stdlibFile "run" (Just "exec")
    exec <- compileLisp env inFile "exec" Nothing
    outH <- liftIO $ openFile "_tmp.hs" WriteMode
    _ <- liftIO $ writeList outH header
    -- _ <- liftIO $ writeList outH $ map show libC
    _ <- liftIO $ writeList outH $ map show exec
    _ <- liftIO $ hClose outH
    if not (null exec)
        then return $ List []
        else throwError $ Default "Empty file"

compileHaskellFile :: FilePath -> IO ()
compileHaskellFile filename = do
    let ghc = "ghc"
    compileStatus <- system $ ghc ++ " -cpp --make -package ghc -fglasgow-exts -o " ++ filename ++ " _tmp.hs"
    -- to do: delete intermediate files if req'd
    case compileStatus of
        ExitFailure _code -> exitWith compileStatus
        ExitSuccess       -> return ()

writeList :: Handle -> [String] -> IO ()
writeList outH []     = hPutStr outH ""
writeList outH (l:ls) = hPutStrLn outH l >> writeList outH ls



