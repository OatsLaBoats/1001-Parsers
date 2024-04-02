module Analyzer.Typechecker (typecheck) where

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
checkStmt stmt state = case stmt of
    (VariableStmt _ _ _ _) -> checkVariableStmt stmt state
    _ -> undefined

checkVariableStmt :: Stmt -> TcState -> (VarTable, [Error])
checkVariableStmt stmt state@(_, table) = case stmt of
    (VariableStmt name varType expr loc) ->
        let (exprType, exprErrors) = checkExpr expr state
        in  if compareType1 varType exprType
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
checkPrimaryExpr expr state@(funcs, table) = case expr of
    (IntLit _) -> (Just $ BaseType "Int", [])
    (BoolLit _) -> (Just $ BaseType "Bool", [])
    (FloatLit _) -> (Just $ BaseType "Float", [])
    (StringLit _) -> (Just $ BaseType "String", [])
    (ArrayLit values loc) -> checkArrayLit values loc state
    (ParenExpr expr) -> checkExpr expr state
    (CallExpr name params loc) -> undefined
    (AccessExpr varName loc)
        | VT.isVarDefined varName table -> (VT.getVar varName table, [])
        | otherwise -> (Nothing, [("Variable '" ++ varName ++ "' doesn't exist", loc)])

-- This one is a bit crazy. Unlike the odin implementation this one checks all the
-- expressions inside of the array, the odin version just returns on the first type error.
checkArrayLit :: [Expr] -> Location -> TcState -> (Maybe Type, [Error])
checkArrayLit [] _ _ = (Nothing, [])
checkArrayLit (expr:exprs) loc state =
    let result@(t, es) = check exprs []
    in case t of
        Nothing -> (t, errorMessage : es)
        _ -> result
    where
        errorMessage = ("Multiple different types in array literal", loc)

        (firstType, errors) = checkExpr expr state

        -- Recursively goes through the expressions inside the literal and accumulates the errors.
        -- If an errors occurred or the errs accumulator already had errors in it then we will assume failure.
        -- May be possible to implement using fold.
        check [] errs = (if null errs then firstType else Nothing, errors ++ errs)
        check (x:xs) errs =
            let (t, errors') = checkExpr x state
                errs' = if compareType2 firstType t && null errs
                        then [] 
                        else errs ++ errors'
            in check xs errs'

compareType1 :: Type -> Maybe Type -> Bool
compareType1 t1 m = case m of
    Just t2 -> t1 == t2
    Nothing -> False

compareType2 :: Maybe Type -> Maybe Type -> Bool
compareType2 m1 m2 = case m1 of
    Nothing -> False
    Just t1 -> case m2 of
        Nothing -> False
        Just t2 -> t1 == t2
