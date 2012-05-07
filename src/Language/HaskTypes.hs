module Language.HaskTypes where

import Language.Types

-- Make Haskell functions

makeHFunc :: (Monad m) => Maybe String -> Env -> [String]
    -> (Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal)
    -> m LispVal
makeHFunc varargs env fparams fbody = return $ HFunc fparams varargs fbody env

makeHNormalFunc :: (Monad m) => Env -> [String]
    -> (Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal)
    -> m LispVal
makeHNormalFunc = makeHFunc Nothing

makeHVarArgs :: (Monad m) => LispVal -> Env -> [String]
    -> (Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal)
    -> m LispVal
makeHVarArgs varargs = makeHFunc (Just $ showVal varargs)




