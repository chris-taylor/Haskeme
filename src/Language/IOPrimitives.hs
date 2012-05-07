module Language.IOPrimitives (ioPrimitives, load, gensym_) where

import System.IO
import System.Directory
import Data.Unique
import Control.Monad.Error
import Control.Exception
import System.Random

import Language.Types
import Language.Core
import Language.Parser

ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives = [ ("apply", applyProc)
               , ("open-input-file", makePort ReadMode)
               , ("open-output-file", makePort WriteMode)
               , ("close-input-port", closePort)
               , ("close-output-port", closePort)
               , ("read", readProc)
               , ("write", writeProc)
               , ("pr", printProc putStr)
               , ("prn", printProc putStrLn)
               , ("read-contents", readContents)
               , ("read-all", readAll)
               , ("file-exists", fileExists)
               , ("delete-file", deleteFile)
               , ("random", rand)
               , ("uniq", gensym) ]

applyProc :: [LispVal] -> IOThrowsError LispVal
applyProc [func, List args] = apply func args
applyProc (func : args)     = apply func args

makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [String filename] = asIO Port $ openFile filename mode
makePort mode args = throwError $ NumArgs 1 args

closePort :: [LispVal] -> IOThrowsError LispVal
closePort [Port port] = liftIO $ hClose port >> (return $ Bool True)
closePort _           = return $ Bool False

readProc :: [LispVal] -> IOThrowsError LispVal
readProc []          = readProc [Port stdin]
readProc [Port port] = (liftIO $ hGetLine port) >>= liftThrows . readExpr
readProc [notPort]   = throwError $ TypeMismatch "port" notPort
readProc badArgs = throwError $ NumArgs 1 badArgs

writeProc :: [LispVal] -> IOThrowsError LispVal
writeProc [obj]            = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port obj >> return nil
writeProc [obj, notPort]   = throwError $ TypeMismatch "port" notPort
writeProb badArgs = throwError $ NumArgs 1 badArgs

printProc :: (String -> IO ()) -> [LispVal] -> IOThrowsError LispVal
printProc printer []     = liftIO $ printer "" >> return nil
printProc printer (x:xs) = liftIO (putStr $ pr x) >> printProc printer xs
    where pr (String s) = s
          pr (Char c)   = [c]
          pr other      = showVal other

fileExists :: [LispVal] -> IOThrowsError LispVal
fileExists [String filename] = liftIO (doesFileExist filename) >>= return . Bool
fileExists [notString]       = throwError $ TypeMismatch "string" notString
fileExists badArgs           = throwError $ NumArgs 1 badArgs

deleteFile :: [LispVal] -> IOThrowsError LispVal
deleteFile [String filename] = do
    exists <- liftIO $ doesFileExist filename
    if exists
        then do
            liftIO $ removeFile filename
            return $ Bool True
        else return $ Bool False
deleteFile [notString] = throwError $ TypeMismatch "string" notString
deleteFile badArgs     = throwError $ NumArgs 1 badArgs

readContents :: [LispVal] -> IOThrowsError LispVal
readContents [String filename] = asIO String $ readFile filename
readContents [notString]       = throwError $ TypeMismatch "string" notString
readContents badArgs           = throwError $ NumArgs 1 badArgs

readAll :: [LispVal] -> IOThrowsError LispVal
readAll [String filename] = liftM List $ load filename
readAll [notString]       = throwError $ TypeMismatch "string" notString
readAll badArgs           = throwError $ NumArgs 1 badArgs

-- Unique symbols

gensym :: [LispVal] -> IOThrowsError LispVal
gensym []              = gensym_ "#g"
gensym [String prefix] = gensym_ prefix
gensym [notString]     = throwError $ TypeMismatch "string" notString
gensym badArgs         = throwError $ NumArgs 1 badArgs

-- Random numbers

rand :: [LispVal] -> IOThrowsError LispVal
rand [modulus] = case modulus of
    Number n -> asIO Number $ randomRIO (0, n - 1)
    Float n  -> asIO Float $ randomRIO (0, n)
    Ratio n  -> asIO (Ratio . toRational) $ randomRIO (0 , fromRational n :: Double)
rand badArgs = throwError $ NumArgs 1 badArgs

-- Helper functions

gensym_ :: String -> IOThrowsError LispVal
gensym_ prefix = do
    u <- liftIO $ newUnique
    return $ Atom $ prefix ++ (show $ toInteger $ hashUnique u)

{- liftIO :: MonadIO m => IO a -> m a
        Lifts IO values into an IO-compatible monad
    liftM :: Monad m => (a -> b) -> (m a -> m b)
        Converts arbitrary functions into monadic functions
    asIO :: MonadIO m => (a -> b) -> (IO a -> m b)
        Converts arbitrary functions into functions that map IO values to
        arbitrary IO-compatible monads -}

asIO :: (MonadIO m) => (a -> b) -> (IO a -> m b)
asIO func = liftM func . liftIO