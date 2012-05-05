module Language.Compiler where

import qualified Data.List as L

import Language.Types
import Language.HaskTypes

-- Misc functions

header :: [String]
header =
    [ "module Main where "
    , "import Language.Core "
    , "import Language.Numeric "
    , "import Language.Primitives "
    , "import Language.IOPrimitives "
    , "import Language.Types "
    , "import Language.Variables "
    , "import Control.Monad.Error "
    , " "
    , "main :: IO () "
    , "main = do "
    , "  env <- primitiveBindings "
    , "  result <- runIOThrows $ liftM show $ run env .... "
    , "  case result of "
    , "    Just errMsg -> putStrLn errMsg "
    , "    _ -> return () "
    , " "]

--compileLisp :: Env -> String -> String -> Maybe String -> IOThrowsError [HaskAST]
--compileLisp env filename entry exit = load filename >>=
--    compileBlock entry exit env []

compileLisp :: Env -> String -> String -> Maybe String -> IOThrowsError [HaskAST]
compileLisp env filename entry exit = return []

compileBlock :: String -> Maybe String -> Env -> [HaskAST] -> [LispVal] -. IOThrowsError [HaskAST]
compileBlock thisFunc lastFunc env result code@[c] = do
    compiled <- compile env c $ CompileOptions thisFunc False False lastFunc
    return $ result ++ compiled
compileBlock thisFunc lastFunc env result code@(c:cs) = do
    Atom nextFunc <- _gensym "f"
    compiled <- compile env c $ CompileOptions thisFunc False False (Just nextFunc)
    compileBlock nextFunc lastFunc env (result ++ compiled) cs
compileBlock _ _ _ result [] = return result

compileScalar :: String -> CompOpts -> IOThrowsError [HaskAST]
compileScalar val copts = do
    f <- return $ AstAssignM "xl" $ AstValue val
    c <- return $ 