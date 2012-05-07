module Language.Compiler where

import Complex
import Ratio
import qualified Data.List as L
import qualified Data.Array as A

import Language.Types
import Language.HaskTypes
import Language.Core
import Language.Variables
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
    , ""
    , "main :: IO () "
    , "main = do "
    , "  env <- primitiveBindings "
    , "  result <- runIOThrowsCompile $ liftM show $ exec env Nil Nothing"
    , "  case result of "
    , "    Just errMsg -> putStrLn errMsg "
    , "    _ -> return () "
    , ""]

-- Compilation options

data CompOpts = CompileOptions {
    coptsThisFunc :: String,
    coptsThisFuncUseValue :: Bool,
    coptsThisFuncUseArgs :: Bool,
    coptsNextFunc :: Maybe String
}

defaultCompileOptions :: String -> CompOpts
defaultCompileOptions thisFunc = CompileOptions thisFunc False False Nothing

-- AST functions

data HaskAST = AstAssignM String HaskAST
             | AstFunction { astfName :: String
                           , astfArgs :: String
                           , astfCode :: [HaskAST] }
             | AstValue String
             | AstContinuation { astcNext :: String
                               , astcArgs :: String }

instance Show HaskAST where
    show = showValAst

showValAst :: HaskAST -> String
showValAst (AstAssignM var val) = "  " ++ var ++ " <- " ++ show val
showValAst (AstFunction name args code) = do
    let header = "\n" ++ name ++ args ++ "= do "
    let body = unwords . map (\x -> "\n" ++ x) $ map showValAst code
    header ++ body
showValAst (AstValue v) = v

createAstFunc :: CompOpts -> [HaskAST] -> HaskAST
createAstFunc (CompileOptions thisFunc useVal useArgs _) body =
    let val = if useVal then "value" else "_"
        args = if useArgs then "(Just args)" else "_"
    in AstFunction thisFunc (" env " ++ val ++ " " ++ args ++ " ") body

createAstCont :: CompOpts -> String -> String -> HaskAST
createAstCont (CompileOptions _ _ _ (Just nextFunc)) var indent =
    AstValue $ indent ++ "  " ++ nextFunc ++ " env " ++ var ++ " Nothing"
createAstCont (CompileOptions _ _ _ Nothing) var indent =
    AstValue $ indent ++ "  return " ++ var

-- Turn Lisp types into Haskell strings

toHaskString :: LispVal -> String
toHaskString (String s) = "String " ++ show s
toHaskString (Char c) = "Char " ++ show c
toHaskString (Atom a) = "Atom " ++ show a
toHaskString (Number n) = "Number (" ++ show n ++ ")"
toHaskString (Complex c) = "Complex $ (" ++ show (realPart c) ++ ") :+ (" ++
    show (imagPart c) ++ ")"
toHaskString (Ratio r) = "Rational $ (" ++ show (numerator r) ++ ") % (" ++
    show (denominator r) ++ ")"
toHaskString (Float f) = "Float (" ++ show f ++ ")"
toHaskString (Bool True) = "Bool True"
toHaskString (Bool False) = "Bool False"
toHaskString (Vector v) = do
    let ls = A.elems v
        size = (length ls) - 1
    "Vector (listArray (0, " ++ show size ++ ")" ++ "[" ++
        joinL (map toHaskString ls) "," ++ "])"
toHaskString (List ls) = "List [" ++ joinL (map toHaskString ls) "," ++ "]"
toHaskString (DottedList ls tl) =
    "DottedList [" ++ joinL (map toHaskString ls) "," ++ "] $" ++ toHaskString tl

joinL :: [[a]] -> [a] -> [a]
joinL ls sep = concat $ L.intersperse sep ls

-- Main

--compileLisp :: Env -> FilePath -> String -> Maybe String -> IOThrowsError [HaskAST]
--compileLisp _ _ _ _ = return []

compileLisp :: Env -> FilePath -> String -> Maybe String -> IOThrowsError [HaskAST]
compileLisp env filename entry exit = load filename >>=
    compileBlock entry exit env []

compileBlock :: String -> Maybe String -> Env -> [HaskAST] -> [LispVal] -> IOThrowsError [HaskAST]
compileBlock thisFunc lastFunc env result code@[c] = do
    compiled <- mcompile env c $ CompileOptions thisFunc False False lastFunc
    return $ result ++ compiled
compileBlock thisFunc lastFunc env result code@(c:cs) = do
    Atom nextFunc <- gensym_ "f"
    compiled <- mcompile env c $ CompileOptions thisFunc False False (Just nextFunc)
    compileBlock nextFunc lastFunc env (result ++ compiled) cs
compileBlock _ _ _ result [] = return result

mcompile :: Env -> LispVal -> CompOpts -> IOThrowsError [HaskAST]
mcompile env lisp copts = mfunc compile env lisp copts

mfunc :: (Env -> LispVal -> CompOpts -> IOThrowsError [HaskAST]) -> 
    Env -> LispVal -> CompOpts -> IOThrowsError [HaskAST]
mfunc func env lisp copts = do
    transformed <- meval env lisp
    func env transformed copts

compile :: Env -> LispVal -> CompOpts -> IOThrowsError [HaskAST]
compile _ (String s) copts  = compileScalar ("return $ String " ++ show s) copts
compile _ (Char c) copts    = compileScalar ("return $ Char " ++ show c) copts
compile _ (Complex c) copts = compileScalar ("return $ Complex $ (" ++
    show (realPart c) ++ ") :+ (" ++ show (imagPart c) ++ ")") copts
compile _ (Float f) copts   = compileScalar ("return $ Float (" ++ show f ++ ")") copts
compile _ (Ratio r) copts   = compileScalar ("return $ Ratio $ (" ++
    show (numerator r) ++ ") % (" ++ show (denominator r) ++ ")") copts
compile _ (Number n) copts  = compileScalar ("return $ Number (" ++ show n ++ ")") copts
compile _ (Bool b) copts    = compileScalar ("return $ Bool " ++ show b) copts
compile _ (Atom a) copts    = compileScalar ("getVar env \"" ++ a ++ "\"") copts
compile _ (List [Atom "quote", val]) copts = compileScalar ("return $ " ++ toHaskString val) copts
compile env (List (Atom "def" : var : rest)) copts = compileDefine env copts var rest

compileScalar :: String -> CompOpts -> IOThrowsError [HaskAST]
compileScalar val copts = do
    f <- return $ AstAssignM "x" $ AstValue val
    c <- return $ createAstCont copts "x" ""
    return [createAstFunc copts [f, c]]

compileLambdaList :: [LispVal] -> IOThrowsError String
compileLambdaList lst = do
    serialized <- mapM serialize lst
    return $ "[" ++ joinL serialized "," ++ "]"
    where
        serialize (Atom a) = return $ show a

compileDefine :: Env -> CompOpts -> LispVal -> [LispVal] -> IOThrowsError [HaskAST]
compileDefine env copts (Atom var) [form] = compileDefineVar env copts var form
compileDefine env copts (Atom var) (List params : body) = compileDefineFunc env copts var params body

compileDefineVar :: Env -> CompOpts -> String -> LispVal -> IOThrowsError [HaskAST]
compileDefineVar env copts var form = do
    Atom symDefine <- gensym_ "defineFuncDefine"
    Atom symMakeDefine <- gensym_ "defineFuncMakeDef"
    -- Store in local environment for macro processing
    _ <- defineVar env var form
    -- Entry point
    f <- return $ [AstValue $ "  " ++ symDefine ++ " env Nil Nothing"]
    -- Auxiliary functions
    compDefine <- compileExpr env form symDefine $ Just symMakeDefine
    compMakeDefine <- return $ AstFunction symMakeDefine " env result _ " [
        AstValue $ "  _ <- defineVar env \"" ++ var ++ "\" result",
        createAstCont copts "result" ""]
    return $ [createAstFunc copts f] ++ compDefine ++ [compMakeDefine]

compileDefineFunc :: Env -> CompOpts -> String -> [LispVal] -> [LispVal] -> IOThrowsError [HaskAST]
compileDefineFunc env copts@(CompileOptions thisFunc _ _ nextFunc) var params body = do
    Atom symCallFunc <- gensym_ "defineFuncEntryPoint"
    compiledParams   <- compileLambdaList params
    compiledBody     <- compileBlock symCallFunc Nothing env [] body
    -- TODO: store var in local environment for macro processing
    -- _ <- defineVar env var form
    --Entry point
    f <- return $ [AstValue $ "  result <- makeHNormalFunc env (" ++ compiledParams ++ ") " ++ symCallFunc,
                   AstValue $ "  _ <- defineVar env \"" ++ var ++ "\" result",
                   createAstCont copts "result" "" ]
    return $ [createAstFunc copts f] ++ compiledBody

compileExpr :: Env -> LispVal -> String -> Maybe String -> IOThrowsError [HaskAST]
compileExpr env expr thisFunc nextExpr =
    mcompile env expr (CompileOptions thisFunc False False nextExpr)



