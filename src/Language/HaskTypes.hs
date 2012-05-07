module Language.HaskTypes where

import Ratio
import Complex
import qualified Data.Array
import qualified Data.Map
import qualified Data.List

import Language.Types

-- Compilation options

data CompOpts = CompileOptions {
    coptsThisFunc :: String,
    coptsThisFuncUseValue :: Bool,
    coptsThisFuncUseArgs :: Bool,
    coptsNextFunc :: Maybe String
}

defaultCompileOptions :: String -> CompOpts
defaultCompileOptions thisFunc = CompileOptions thisFunc False False Nothing

-- Haskell AST

data HaskAST = AstAssignM String HaskAST
             | AstFunction { astfName :: String
                           , astfArgs :: String
                           , astfCode :: [HaskAST] }
             | AstValue String

instance Show HaskAST where
    show = showValAst

createAstFunc :: CompOpts -> [HaskAST] -> HaskAST
createAstFunc (CompileOptions thisFunc useVal useArgs _) body =
    let val = if useVal then "value" else "_"
        args = if useArgs then "(Just args)" else "_"
    in AstFunction thisFunc (" env " ++ val ++ " " ++ args ++ " ") body

showValAst :: HaskAST -> String
showValAst (AstAssignM var val) = "  " ++ var ++ " <- " ++ show val
showValAst (AstFunction name args code) = do
    let header = "\n" ++ name ++ args ++ "= do "
    let body = unwords . map (\x -> "\n" ++ x) $ map showValAst code
    header ++ body
showValAst (AstValue v) = v

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
    let ls = Data.Array.elems v
        size = (length ls) - 1
    "Vector (listArray (0, " ++ show size ++ ")" ++ "[" ++
        joinL (map toHaskString ls) "," ++ "])"
toHaskString (List ls) = "List [" ++ joinL (map toHaskString ls) "," ++ "]"
toHaskString (DottedList ls tl) =
    "DottedList [" ++ joinL (map toHaskString ls) "," ++ "] $" ++ toHaskString tl

joinL :: [[a]] -> [a] -> [a]
joinL ls sep = concat $ Data.List.intersperse sep ls

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




