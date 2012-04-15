module LispError ( trapError
                 , extractValue
                 , liftThrows
                 , runIOThrows
                 ) where

import Data.IORef
import Control.Monad.Error

import LispVal

instance Show LispError where
    show = showError

instance Error LispError where
    noMsg  = Default "An error has occured"
    strMsg = Default

showError :: LispError -> String
showError (NumArgs expected found) = "Expected " ++ show expected
    ++ " args; found values " ++ unwordsList found
showError (Parser parseErr) = "Parse error at " ++ show parseErr
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
    ++ ", found " ++ show found
showError (UnboundVar message varname) = message ++ ": " ++ varname

trapError :: (MonadError e m, Show e) => m String -> m String
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err)  = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runErrorT (trapError action) >>= return . extractValue
