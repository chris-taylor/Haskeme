module Main where

import System.Environment
import Control.Monad.Error

import LispVal
import LispError
import LispParser
import Primitives

-- Main

main :: IO ()
main = getArgs >>= readEvalPrint . head

readEvalPrint :: String -> IO ()
readEvalPrint arg = do
    evaled <- return $ liftM show $ readExpr arg >>= eval
    putStrLn $ extractValue $ trapError evaled

repl :: IO ReplResult
repl = iterateUntil (== Quit) $ do
    putStr prompt
    line <- getLine
    case line of
        "quit" -> return Quit
        ""     -> return Continue
        _      -> readEvalPrint line >> return Continue

prompt :: String
prompt = "haskeme> "

data ReplResult = Continue | Quit deriving Eq

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

-- Monad loops

iterateUntil :: (Monad m) => (a -> Bool) -> m a -> m a
iterateUntil p x = do
    y <- x
    if p y
        then return y
        else iterateUntil p x
