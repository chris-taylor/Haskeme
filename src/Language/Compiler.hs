module Language.Compiler where

import Complex
import Ratio
import qualified Data.List as L
import qualified Data.Array as A
import qualified Data.Map as M

import Language.Types
import Language.HaskTypes
import Language.Core
import Language.Variables
import Language.Primitives

-- Misc functions

header :: [String]
header =
    [ "module Main where "
    , "import Control.Monad.Error "
    , "import qualified Data.Array as A"
    , "import qualified Data.Map as M"
    , "import Language.Core "
    , "import Language.Numeric "
    , "import Language.Primitives "
    , "import Language.Types "
    , "import Language.HaskTypes "
    , "import Language.Variables "
    , ""
    , "main :: IO () "
    , "main = do "
    , "  env <- primitiveBindings "
    , "  result <- runIOThrowsCompile $ liftM show $ exec env Nil Nothing"
    , "  case result of "
    , "    Just errMsg -> putStrLn errMsg "
    , "    _ -> return () " ]

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
    let val  = if useVal then "value" else "_"
        args = if useArgs then "(Just args)" else "_"
    in AstFunction thisFunc (" env " ++ val ++ " " ++ args ++ " ") body

createAstCont :: CompOpts -> String -> String -> HaskAST
createAstCont (CompileOptions _ _ _ (Just nextFunc)) var indent =
    AstValue $ indent ++ "  " ++ nextFunc ++ " env " ++ var ++ " Nothing"
createAstCont (CompileOptions _ _ _ Nothing) var indent =
    AstValue $ indent ++ "  return " ++ var

-- Turn Lisp types into Haskell strings

toHaskStr :: LispVal -> String
toHaskStr (String s) = "String " ++ show s
toHaskStr (Char c) = "Char " ++ show c
toHaskStr (Atom a) = "Atom " ++ show a
toHaskStr (Number n) = "Number " ++ show n
toHaskStr (Complex c) = "Complex $ " ++ show (realPart c) ++ " :+ " ++ show (imagPart c)
toHaskStr (Ratio r) = "Rational $ " ++ show (numerator r) ++ " % " ++ show (denominator r)
toHaskStr (Float f) = "Float " ++ show f
toHaskStr (Bool b) = "Bool " ++ show b
toHaskStr (Vector v) = do
    let ls = A.elems v
        size = (length ls) - 1
    "Vector $ A.listArray (0, " ++ show size ++ ") " ++ "[" ++
        joinL (map toHaskStr ls) "," ++ "]"
toHaskStr (Hash h) = do
    let keys = M.keys h
        vals = M.elems h
        toHaskStrPair (a,b) = "(" ++ toHaskStr a ++ "," ++ toHaskStr b ++ ")"
    "Hash $ M.fromList [" ++ joinL (map toHaskStrPair $ zip keys vals) "," ++ "]"
toHaskStr (List ls) = "List [" ++ joinL (map toHaskStr ls) "," ++ "]"
toHaskStr (DottedList ls tl) =
    "DottedList [" ++ joinL (map toHaskStr ls) "," ++ "] $" ++ toHaskStr tl

joinL :: [[a]] -> [a] -> [a]
joinL ls sep = concat $ L.intersperse sep ls

-- Compilation helper functions

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

mfunc :: (Env -> LispVal -> CompOpts -> IOThrowsError [HaskAST]) -> Env -> LispVal -> CompOpts -> IOThrowsError [HaskAST]
mfunc func env lisp copts = do
    transformed <- macroExpand env lisp
    func env transformed copts

compileScalar :: String -> CompOpts -> IOThrowsError [HaskAST]
compileScalar val copts = do
    f <- return $ AstAssignM "x" $ AstValue val
    c <- return $ createAstCont copts "x" ""
    return [createAstFunc copts [f, c]]

compileArgList :: [LispVal] -> IOThrowsError String
compileArgList lst = do
    serialized <- mapM serialize lst
    return $ "[" ++ joinL serialized "," ++ "]"
    where
        serialize (Atom a) = return $ show a

compileExpr :: Env -> LispVal -> String -> Maybe String -> IOThrowsError [HaskAST]
compileExpr env expr thisFunc nextExpr =
    mcompile env expr (CompileOptions thisFunc False False nextExpr)

-- Main compilation routines

compile :: Env -> LispVal -> CompOpts -> IOThrowsError [HaskAST]
compile env v@(String _) copts  = compileScalar ("return $ " ++ toHaskStr v) copts
compile env v@(Char _) copts    = compileScalar ("return $ " ++ toHaskStr v) copts
compile env v@(Complex _) copts = compileScalar ("return $ " ++ toHaskStr v) copts
compile env v@(Float _) copts   = compileScalar ("return $ " ++ toHaskStr v) copts
compile env v@(Ratio _) copts   = compileScalar ("return $ " ++ toHaskStr v) copts
compile env v@(Number _) copts  = compileScalar ("return $ " ++ toHaskStr v) copts
compile env v@(Bool _) copts    = compileScalar ("return $ " ++ toHaskStr v) copts
compile env v@(Vector _) copts  = compileScalar ("return $ " ++ toHaskStr v) copts
compile env v@(Hash _) copts    = compileScalar ("return $ " ++ toHaskStr v) copts
compile env (Atom a) copts      = compileScalar ("getVar env \"" ++ a ++ "\"") copts
compile env (List [Atom "quote", val]) copts = compileScalar ("return $ " ++ toHaskStr val) copts
compile env (List (Atom "if" : exprs)) copts = compileIf env copts exprs
compile env (List (Atom "=" : args)) copts = compileSet env copts args
compile env (List (Atom "def" : var : rest)) copts = compileDefine env copts var rest
compile env (List (Atom "fn" : params : body)) copts = compileLambda env copts params body
compile env app@(List _) copts = mfunc compileApply env app copts

compileIf :: Env -> CompOpts -> [LispVal] -> IOThrowsError [HaskAST]
compileIf env copts@(CompileOptions thisFunc _ _ nextFunc) [test, conseq, alt] = do
    Atom symPredicate   <- gensym_ "compiledPredicate"
    Atom symConsequent  <- gensym_ "compiledConsequent"
    Atom symAlternate   <- gensym_ "compiledAlternate"
    -- Entry point
    f <- return $ [ AstValue $ "  test  <- " ++ symPredicate ++ " env Nil Nothing"
                  , AstValue $ "  if (truthVal test)"
                  , AstValue $ "    then " ++ symConsequent ++ " env Nil Nothing"
                  , AstValue $ "    else " ++ symAlternate ++ " env Nil Nothing"]
    -- Compiled subexpressions
    compPredicate  <- compileExpr env test symPredicate Nothing
    compConsequent <- compileExpr env conseq symConsequent nextFunc
    compAlternate  <- compileExpr env alt symAlternate nextFunc
    -- Join compiled code together
    return $ [createAstFunc copts f] ++ compPredicate ++ compConsequent ++ compAlternate

compileSet :: Env -> CompOpts -> [LispVal] -> IOThrowsError [HaskAST]
compileSet env copts [Atom var, form] = compileSetVar env var form copts

compileSetVar :: Env -> String -> LispVal -> CompOpts -> IOThrowsError [HaskAST]
compileSetVar env var form copts@(CompileOptions thisFunc _ _ nextFunc) = do
    Atom symDefine <- gensym_ "setVarDefine"
    Atom symMakeDefine <- gensym_ "setVarMakeDefine"
    -- Store var in local environment for macro processing
    _ <- setVar env var form
    -- Entry point
    f <- return $ [AstValue $ "  " ++ symDefine ++ " env Nil Nothing"]
    -- Auxiliary functions
    compDefine <- compileExpr env form symDefine $ Just symMakeDefine
    compMakeDefine <- return $ AstFunction symMakeDefine " env result _ " [
        AstValue $ "  _ <- setVar env \"" ++ var ++ "\" result",
        createAstCont copts "result" ""]
    return $ [createAstFunc copts f] ++ compDefine ++ [compMakeDefine]

compileDefine :: Env -> CompOpts -> LispVal -> [LispVal] -> IOThrowsError [HaskAST]
compileDefine env copts (Atom var) [form] = compileDefineVar env copts var form
compileDefine env copts (Atom var) (List params : body) = compileDefineFunc env copts var params body

compileDefineVar :: Env -> CompOpts -> String -> LispVal -> IOThrowsError [HaskAST]
compileDefineVar env copts var form = do
    Atom symDefine <- gensym_ "defineVarDefine"
    Atom symMakeDefine <- gensym_ "defineVarMakeDef"
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
    Atom symCallFunc <- gensym_ "defineFuncEntry"
    compiledParams   <- compileArgList params
    compiledBody     <- compileBlock symCallFunc Nothing env [] body
    -- TODO: store var in local environment for macro processing
    -- _ <- defineVar env var form
    --Entry point
    f <- return $ [AstValue $ "  result <- makeHNormalFunc env (" ++ compiledParams ++ ") " ++ symCallFunc,
                   AstValue $ "  _ <- defineVar env \"" ++ var ++ "\" result",
                   createAstCont copts "result" "" ]
    -- Join code
    return $ [createAstFunc copts f] ++ compiledBody

compileLambda :: Env -> CompOpts -> LispVal -> [LispVal] -> IOThrowsError [HaskAST]
compileLambda env copts (List params) body = compileNormalLambda env copts params body
compileLambda env copts (DottedList params varargs) body = compileVarArgsLambda env copts params varargs body
compileLambda env copts varargs@(Atom _) body = compileVarArgsLambda env copts [] varargs body

compileNormalLambda :: Env -> CompOpts -> [LispVal] -> [LispVal] -> IOThrowsError [HaskAST]
compileNormalLambda env copts@(CompileOptions thisFunc _ _ nextFunc) params body = do
    Atom symCallFunc <- gensym_ "lambdaEntry"
    compiledParams   <- compileArgList params
    compiledBody     <- compileBlock symCallFunc Nothing env [] body
    -- TODO: extend env when compiling body?
    f <- return $ [ AstValue $ "  result <- makeHNormalFunc env (" ++
        compiledParams ++") " ++ symCallFunc
                  , createAstCont copts "result" "" ]
    -- Join code together
    return $ [createAstFunc copts f] ++ compiledBody

compileVarArgsLambda :: Env -> CompOpts -> [LispVal] -> LispVal -> [LispVal] -> IOThrowsError [HaskAST]
compileVarArgsLambda env copts@(CompileOptions thisFunc _ _ nextFunc) params varargs body = do
    Atom symCallFunc <- gensym_ "lambdaEntry"
    compiledParams   <- compileArgList params
    compiledBody     <- compileBlock symCallFunc Nothing env [] body
    -- TODO: extend env when compiling body?
    f <- return $ [ AstValue $ "  result <- makeHVarArgs (" ++ toHaskStr varargs ++
        ") env (" ++ compiledParams ++ ") " ++ symCallFunc
                  , createAstCont copts "result" "" ]
    -- Join code together
    return $ [createAstFunc copts f] ++ compiledBody

compileApply :: Env -> LispVal -> CompOpts -> IOThrowsError [HaskAST]
compileApply env (List (func : params)) copts@(CompileOptions thisFunc _ _ nextFunc) = do
    Atom symFunc <- gensym_ "compiledFunc"
    Atom symArgs <- gensym_ "compiledArgs"
    -- Entry point
    f <- return $ [ AstValue $ "  func <- " ++ symFunc ++ " env Nil Nothing"
                  , AstValue $ "  args <- " ++ symArgs ++ " env Nil Nothing"
                  , AstValue $ "  result <- apply func args"
                  , createAstCont copts "result" ""]
    -- Compiled code for the function and arguments
    compFunc <- mcompile env func $ CompileOptions symFunc False False Nothing
    compArgs <- compileArgs env params $ CompileOptions symArgs False False Nothing
    -- Join code together
    return $ [createAstFunc copts f] ++
        compFunc ++ compArgs

compileArgs :: Env -> [LispVal] -> CompOpts -> IOThrowsError [HaskAST]
compileArgs env []     copts@(CompileOptions thisFunc _ _ _) =
    return $ [AstFunction thisFunc " env _ _ " [AstValue "  return []"]]
compileArgs env (a:as) copts@(CompileOptions thisFunc _ _ _) = do
    Atom symThis <- gensym_ "getArg"
    Atom symRest <- gensym_ "getOtherArgs"
    -- Entry point
    f <- return $ AstFunction thisFunc " env _ _ " [
                        AstValue $ "  this <- " ++ symThis ++ " env Nil Nothing",
                        AstValue $ "  rest <- " ++ symRest ++ " env Nil Nothing",
                        createAstCont copts "(this:rest)" "" ]
    -- Compile other arguments
    compThis  <- mcompile env a $ CompileOptions symThis False False Nothing
    compRest  <- compileArgs env as $ CompileOptions symRest False False Nothing
    -- Join code together
    return $ [f] ++ compThis ++ compRest