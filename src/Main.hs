module Main where

import System.Environment
import Control.Monad.Error
import IO hiding (try)

import LispVal
import LispError
import LispParser
import Primitives

-- Main

main :: IO ()
main = do
    args <- getArgs
    case length args of
        0 -> runRepl
        1 -> evalAndPrint $ args !! 0
        otherwise -> putStrLn "Program takes 0 or 1 command line argument"

-- IO Functions

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: String -> IO String
evalString expr = return $ extractValue $ trapError (liftM show $ readExpr expr >>= eval)

evalAndPrint :: String -> IO ()
evalAndPrint expr = if null expr
    then return ()
    else evalString expr >>= putStrLn

runRepl :: IO ()
runRepl = untilM_ (== "quit") (readPrompt "haskeme> ") evalAndPrint

-- Monadic looping

untilM_ :: (Monad m) => (a -> Bool) -> m a -> (a -> m ()) -> m ()
untilM_ predicate prompt action = do
    result <- prompt
    if predicate result
        then return ()
        else action result >> untilM_ predicate prompt action

-- Evaluation

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Char _)   = return val
eval val@(Number _) = return val
eval val@(Bool _)   = return val
eval (List [Atom "quote", val]) = return val
eval (List [Atom "if", predicate, conseq, alt]) = do
    result <- eval predicate
    case result of
        Bool True  -> eval conseq
        Bool False -> eval alt
        notBool    -> throwError $ TypeMismatch "boolean" notBool
eval (List (Atom func : args))  = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func)
                        ($ args)
                        (lookup func primitives)
