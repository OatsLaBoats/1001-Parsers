module Analyzer.Typechecker () where

import Analyzer.Internal
import Ast
import Error
import VarTable (VarTable)
import qualified VarTable as VT

type TcState = (FunctionMap, VarTable)

typecheck :: AnalyzerState -> AnalyzerState
typecheck s = foldr pred s (getTable s)
    where
        pred f acc =
            let errors = checkFunction f (getTable acc)
            in  appendErrors acc errors

checkFunction :: Function -> FunctionMap -> [Error]
checkFunction (Function _ _ params block _) functions =
    snd $ foldr pred (initialTable, []) block
    where
        pred stmt (table, errors) =
            let (table', errors') = checkStmt stmt (functions, table)
            in  (table', errors ++ errors')

        initialTable = foldr 
            (\(Parameter name t _) acc -> VT.defineVar name t acc)
            VT.empty 
            params

checkStmt :: Stmt -> TcState -> (VarTable, [Error])
checkStmt stmt state@(funcs, table) = case stmt of
    (VariableStmt _ _ _ _) -> checkVariableStmt stmt state
    _ -> undefined

checkVariableStmt :: Stmt -> TcState -> (VarTable, [Error])
checkVariableStmt stmt state@(funcs, table) = case stmt of
    (VariableStmt name varType expr loc) ->
        let (exprType, exprErrors) = checkExpr expr state
        in if compareType varType exprType
            then ( VT.defineVar name varType table, exprErrors )
            else ( VT.defineVar name varType table
                 , exprErrors ++ [("Varible '" ++ name ++ "' doesn't match the assignment expression type", loc)]
                 )

    _ -> undefined

checkExpr :: Expr -> TcState -> (Maybe Type, [Error])
checkExpr expr state = case expr of
    (BinaryExpr _ _ _ _) -> undefined
    (UnaryExpr _ _ _) -> undefined
    (PrimaryExpr pexpr) -> checkPrimaryExpr pexpr state

checkPrimaryExpr :: PrimaryExpr -> TcState -> (Maybe Type, [Error])
checkPrimaryExpr expr (funcs, table) = case expr of
    (IntLit _) -> (Just $ BaseType "Int", [])
    (BoolLit _) -> (Just $ BaseType "Bool", [])
    (FloatLit _) -> (Just $ BaseType "Float", [])
    (StringLit _) -> (Just $ BaseType "String", [])
    (AccessExpr varName loc)
        | VT.isVarDefined varName table -> (VT.getVar varName table, [])
        | otherwise -> (Nothing, [("Variable '" ++ varName ++ "' doesn't exist", loc)])

compareType :: Type -> Maybe Type -> Bool
compareType t1 m = case m of
    Just t2 -> t1 == t2
    Nothing -> False
