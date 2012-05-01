module Language.IOPrimitives (ioPrimitives) where

import System.IO
import Data.Unique
import Control.Monad.Error
import System.Random

import Language.Types
import Language.Parser
import Language.Core

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
               , ("random", rand)
               , ("uniq", gensym) ]

applyProc :: [LispVal] -> IOThrowsError LispVal
applyProc [func, List args] = apply func args
applyProc (func : args)     = apply func args

makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [String filename] = asIO Port $ openFile filename mode

closePort :: [LispVal] -> IOThrowsError LispVal
closePort [Port port] = liftIO $ hClose port >> (return $ Bool True)
closePort _           = return $ Bool False

readProc :: [LispVal] -> IOThrowsError LispVal
readProc [] = readProc [Port stdin]
readProc [Port port] = (liftIO $ hGetLine port) >>= liftThrows . readExpr

writeProc :: [LispVal] -> IOThrowsError LispVal
writeProc [obj] = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port obj >> (return $ Bool True)

printProc :: (String -> IO ()) -> [LispVal] -> IOThrowsError LispVal
printProc printer []     = liftIO $ printer "" >> return nil
printProc printer (x:xs) = liftIO (putStr $ pr x) >> printProc printer xs
    where pr (String s) = s
          pr (Char c)   = [c]
          pr other      = showVal other

readContents :: [LispVal] -> IOThrowsError LispVal
readContents [String filename] = asIO String $ readFile filename

readAll :: [LispVal] -> IOThrowsError LispVal
readAll [String filename] = liftM List $ load filename

-- Unique symbols

_gensym :: String -> IOThrowsError LispVal
_gensym prefix = do
    u <- liftIO $ newUnique
    return $ Atom $ prefix ++ (show $ toInteger $ hashUnique u)

gensym :: [LispVal] -> IOThrowsError LispVal
gensym []              = _gensym "#g"
gensym [String prefix] = _gensym prefix
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

{- liftIO :: MonadIO m => IO a -> m a
        Lifts IO values into an IO-compatible monad
    liftM :: Monad m => (a -> b) -> (m a -> m b)
        Converts arbitrary functions into monadic functions
    asIO :: MonadIO m => (a -> b) -> (IO a -> m b)
        Converts arbitrary functions into functions that map IO values to
        arbitrary IO-compatible monads -}

asIO :: (MonadIO m) => (a -> b) -> (IO a -> m b)
asIO func = liftM func . liftIO