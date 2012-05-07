module Language.Compiler where

import Complex
import Ratio
import qualified Data.List as L

import Language.Types
import Language.HaskTypes
import Language.Core
import Language.IOPrimitives

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
    , "  result <- runIOThrowsCompile $ liftM show $ run env .... "
    , "  case result of "
    , "    Just errMsg -> putStrLn errMsg "
    , "    _ -> return () "
    , " "]

--data CompileOptions = CompileOptions String (Maybe String)

compileLisp :: Env -> FilePath -> String -> Maybe String -> IOThrowsError [HaskAST]
compileLisp _ _ _ _ = return []

--compileLisp :: Env -> FilePath -> String -> Maybe String -> IOThrowsError [HaskAST]
--compileLisp env filename entry exit = load filename >>=
--    compileBlock entry exit env []

--compileBlock :: String -> Maybe String -> Env -> [HaskAST] -> [LispVal] -> IOThrowsError [HaskAST]
--compileBlock thisFunc lastFunc env result code@[c] = do
--    compiled <- compile env c $ CompileOptions thisFunc False False lastFunc
--    return $ result ++ compiled
--compileBlock thisFunc lastFunc env result code@(c:cs) = do
--    Atom nextFunc <- _gensym "f"
--    compiled <- compile env c $ CompileOptions thisFunc False False (Just nextFunc)
--    compileBlock nextFunc lastFunc env (result ++ compiled) cs
--compileBlock _ _ _ result [] = return result

--compile :: Env -> LispVal -> CompOpts -> IOThrowsError [HaskAST]
--compile _ (String s) copts  = compileScalar ("  return $ String " ++ show s) copts
--compile _ (Char c) copts    = compileScalar ("  return $ Char " ++ show c) copts
--compile _ (Complex c) copts = compileScalar ("  return $ Complex $ (" ++
--    show (realPart c) ++ ") :+ (" ++ show (imagPart c) ++ ")") copts
--compile _ (Float f) copts   = compileScalar ("  return $ Float (" ++ show f ++ ")") copts
--compile _ (Ratio r) copts   = compileScalar ("  return $ Ratio $ (" ++
--    show (numerator r) ++ ") % (" ++ show (denominator r) ++ ")") copts
--compile _ (Number n) copts  = compileScalar ("  return $ Number (" ++ show n ++ ")") copts
--compile _ (Bool b) copts    = compileScalar ("  return $ Bool " ++ show b) copts


--compileScalar :: String -> CompOpts -> IOThrowsError [HaskAST]
--compileScalar val copts = do
--    f <- return $ AstAssignM "x1" $ AstValue val
--    return [createAstFunc copts [f]]

