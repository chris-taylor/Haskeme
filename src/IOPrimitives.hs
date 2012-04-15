module IOPrimitives (ioPrimitives) where

import LispVal
import LispError
import Eval

ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives = []
--ioPrimitives = [ ("apply", applyProc)
               --, ("open-input-file", makePort ReadMode)
               --, ("open-output-file", makePort WriteMode)
               --, ("close-input-port", closePort)
               --, ("close-output-port", closePort)
               --, ("read", readProc)
               --, ("write", writeProc)
               --, ("read-contents", readContents)
               --, ("read-all", readAll) ]

applyProc :: [LispVal] -> IOThrowsError LispVal
applyProc [func, List args] = apply func args
applyProc (func : args)     = apply func args