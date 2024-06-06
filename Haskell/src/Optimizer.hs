module Optimizer (runTailcallPass) where

import Ast
import Data.List (unsnoc)

runTailcallPass :: Ast -> Ast
runTailcallPass = map tailcallFunction

tailcallFunction :: Function -> Function
tailcallFunction f@(Function name retType params block loc)
    | hasTailcall f = Function name retType params [loop] loc
    | otherwise = f
    where
        block' = init block
        params' = case last block of
            ReturnStmt (PrimaryExpr (CallExpr _ ps _)) _ ->
                zip ps $ map (\(Parameter name _ _) -> name) params
            _ -> undefined

        body = block' <> map (\(expr, name) -> AssignmentStmt name expr (-1, -1)) params'
        loop = WhileStmt (PrimaryExpr $ BoolLit True) body (-1, -1)

hasTailcall :: Function -> Bool
hasTailcall (Function name _ _ block _) = case snd <$> unsnoc block of
    Just (ReturnStmt (PrimaryExpr (CallExpr name' _ _)) _) -> name' == name
    _ -> False
