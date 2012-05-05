module Language.HaskTypes where

-- Haskell AST

data HaskAST = AstAssignM String HaskAST
             | AstFunction { astfName :: String
                           , astfArgs :: String
                           , astfCode :: [HaskAST] }
             | AstValue String

instance Show HaskAST where
    show = showValAST

showValAst :: HaskAST -> String
showValAst (AstAssignM var val) = "  " ++ var ++ " <- " ++ show val
showValAst (AstFunction name args code) = do
    let header = "\n" ++ name ++ args ++ "= do "
    let body = unwords . map (\x -> "\n" ++ x) $ map showValAST code
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
toHaskString (Rational r) = "Rational $ (" ++ show (numerator r) ++ ") % (" ++
    show (denominator r) ++ ")"
toHaskString (Float f) = "Float (" ++ show f ++ ")"
toHaskString (Bool True) = "Bool True"
toHaskString (Bool False) = "Bool False"

-- Make Haskell functions

makeHFunc :: (Monad m) => Maybe String -> Env -> [String]
    -> (Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal)
    -> m LispVal
makeHFunc varargs env fparams fbody = return $ HFunc fparams varargs fbody env

makeHNormalFunc = makeHFunc Nothing
makeHVarArgs = makeHFunc . Just . showVal