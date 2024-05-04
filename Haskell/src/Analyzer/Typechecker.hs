module Analyzer.Typechecker (typecheck) where

import Analyzer.Internal
import Ast
import Error
import VarTable (VarTable)
import qualified VarTable as VT
import qualified Data.Map as M
import Data.Maybe

type TcState = (FunctionMap, VarTable)

typecheck :: AnalyzerState -> AnalyzerState
typecheck s = foldr pred s (getTable s)
    where
        pred f acc =
            let errors = checkFunction f (getTable acc)
            in  appendErrors acc errors

checkFunction :: Function -> FunctionMap -> [Error]
checkFunction (Function name _ params block _) functions =
    checkBlock block (functions, initialTable) name
    where
        initialTable = foldr 
            (\(Parameter name t _) acc -> VT.defineVar name t acc)
            VT.empty 
            params

checkBlock :: Block -> TcState -> String -> [Error]
checkBlock block (functions, vars) name =
    snd $ foldr pred (vars, []) block
    where
        pred stmt (table, errors) =
            let (table', errors') = checkStmt stmt (functions, table) name
            in (table', errors ++ errors')

checkStmt :: Stmt -> TcState -> String -> (VarTable, [Error])
checkStmt stmt state@(_, vars) funcName = case stmt of
    (VariableStmt _ _ _ _) -> checkVariableStmt stmt state
    (ReturnStmt _ _) -> (vars, checkReturnStmt stmt state funcName)
    (PrintStmt expr) -> (vars, checkPrintStmt expr state)
    (WhileStmt _ _ _) -> (vars, checkWhileStmt stmt state funcName)
    (IfStmt _ _ _ _) -> (vars, checkIfStmt stmt state funcName "'if'")
    _ -> undefined

type Keyword = String
checkIfStmt :: Stmt -> TcState -> String -> Keyword -> [Error]
checkIfStmt stmt state name keyword = case stmt of
    (IfStmt expr block elseBranch loc) -> exprErrors ++ blockErrors ++ ifErrors ++ elseErrors
        where
            (etype, exprErrors) = checkExpr expr state
            blockErrors = checkBlock block state name
            ifErrors
                | compareType1 (BaseType "Bool") etype = []
                | otherwise = [(keyword ++ " conditional must be 'Bool'", loc)]
            elseErrors = case elseBranch of
                Nothing -> []
                Just elseStmt -> case elseStmt of
                    (ElifStmt expr' block' elseBranch' loc') -> 
                        checkIfStmt (IfStmt expr' block' elseBranch' loc') state name "'elif'"
                    (ElseStmt block') -> checkBlock block' state name
                    _ -> undefined
    _ -> undefined

checkWhileStmt :: Stmt -> TcState -> String -> [Error]
checkWhileStmt stmt state name = case stmt of
    (WhileStmt expr block loc) -> exprErrors ++ blockErrors ++ whileErrors
        where
            (etype, exprErrors) = checkExpr expr state
            blockErrors = checkBlock block state name
            whileErrors
                | compareType1 (BaseType "Bool") etype = []
                | otherwise = [("'while' conditional must be 'Bool'", loc)]
    _ -> undefined

checkPrintStmt :: Expr -> TcState -> [Error]
checkPrintStmt expr state = 
    let (_, errors) = checkExpr expr state in errors

checkReturnStmt :: Stmt -> TcState -> String -> [Error]
checkReturnStmt stmt state@(funcs, _) funcName = case stmt of
    (ReturnStmt expr loc) -> case retType of
        Nothing -> [("Function '" ++ name ++ "' shouldn't have a 'return' statement", loc)]
        Just rtype
            | compareType1 rtype etype -> []
            | otherwise -> 
                exprErrors ++ [("'return' doesn't match the function '" ++ name ++ "' return type", loc)]
        where
            (Function name retType _ _ _) = fromJust $ M.lookup funcName funcs
            (etype, exprErrors) = checkExpr expr state
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
    (BinaryExpr _ _ _ _) -> checkBinaryExpr expr state
    (UnaryExpr _ _ _) -> checkUnaryExpr expr state
    (PrimaryExpr pexpr) -> checkPrimaryExpr pexpr state

checkBinaryExpr :: Expr -> TcState -> (Maybe Type, [Error])
checkBinaryExpr expr state = case expr of
    (BinaryExpr op lexpr rexpr loc)
        | not . null $ errors -> (Nothing, errors)
        | otherwise -> case op of
            OpOr -> orAnd
            OpAnd -> orAnd
            OpEq -> eqNeq
            OpNeq -> eqNeq
            OpGt -> gtLt
            OpLt -> gtLt
            OpGtEq -> gtLt
            OpLtEq -> gtLt
            OpSub -> subMulDiv
            OpMul -> subMulDiv
            OpDiv -> subMulDiv
            OpMod -> opMod
            OpAdd -> opAdd
            OpIndex -> opIndex
        where
            (ltype, lErrors) = checkExpr lexpr state
            (rtype, rErrors) = checkExpr rexpr state
            errors = lErrors ++ rErrors

            orAnd
                | isBoolL && isBoolR = (Just tBool, [])
                | otherwise = (Nothing, [("'" ++ opName ++ "' requires 'Bool' operands", loc)])

            eqNeq
                | (isNumberL && isNumberR) || compareType2 ltype rtype = (Just tBool, [])
                | otherwise = 
                    (Nothing, [("'" ++ opName ++ "' requires operands of the same type", loc)])

            gtLt
                | isNumberL && isNumberR = (Just tBool, [])
                | otherwise = (Nothing, [("'" ++ opName ++ "' requires numeric operands", loc)])

            subMulDiv
                | isNumberL && isNumberR = (Just (if isFloatL || isFloatR then tFloat else tInt), [])
                | otherwise = (Nothing, [("'" ++ opName ++ "' requires numeric operands", loc)])

            opMod
                | isIntL && isIntR = (Just tInt, [])
                | otherwise = (Nothing, [("'%' requires 'Int' operands", loc)])

            opAdd
                | isNumberL && isNumberR = (Just (if isFloatL || isFloatR then tFloat else tInt), [])
                | isStringL && isStringR = (Just tString, [])
                | compareType2 ltype rtype = (ltype, []) -- checks for compatible array types
                | isArrayL && isArrayR = (Nothing, [("Arrays contain different types", loc)])
                | otherwise = (Nothing, [("'+' requires 'String', '[T]', Int or Float operands", loc)])

            opIndex
                | isArrayL && isIntR = (ltype, [])
                | isArrayL && not isIntR = (Nothing, [("Index must be of type 'Int'", loc)])
                | otherwise = (Nothing, [("Can't index data that's not an array", loc)])

            tBool = BaseType "Bool"
            isBoolL = compareType1 tBool ltype
            isBoolR = compareType1 tBool rtype

            tFloat = BaseType "Float"
            isFloatL = compareType1 tFloat ltype
            isFloatR = compareType1 tFloat rtype

            tInt = BaseType "Int"
            isIntL = compareType1 tInt ltype
            isIntR = compareType1 tInt rtype

            tString = BaseType "String"
            isStringL = compareType1 tString ltype
            isStringR = compareType1 tString rtype

            isArray t = case t of
                Just (ArrayType _) -> True
                _ -> False

            isArrayL = isArray ltype
            isArrayR = isArray rtype

            isNumberL = isFloatL || isIntL
            isNumberR = isFloatR || isIntR

            opName = case op of
                OpOr -> "or"
                OpAnd -> "and"
                OpEq -> "=="
                OpNeq -> "!="
                OpGt -> ">"
                OpLt -> "<"
                OpGtEq -> ">="
                OpLtEq -> "<="
                OpSub -> "-"
                OpMul -> "*"
                OpDiv -> "/"
                OpMod -> "%"
                OpAdd -> "+"
                OpIndex -> "[]"
    _ -> undefined

checkUnaryExpr :: Expr -> TcState -> (Maybe Type, [Error])
checkUnaryExpr expr state = case expr of
    (UnaryExpr op expr' loc) ->
        let (etype, errors) = checkExpr expr' state
        in case op of
            OpNeg
                | compareType1 (BaseType "Int") etype || compareType1 (BaseType "Float") etype -> (etype, [])
                | otherwise -> (Nothing, errors ++ [("'-' requires 'Float' or 'Int' operands", loc)])
            OpNot
                | compareType1 (BaseType "Bool") etype -> (etype, [])
                | otherwise -> (Nothing, errors ++ [("'not' requires 'Bool' operands", loc)])
    _ -> undefined

checkPrimaryExpr :: PrimaryExpr -> TcState -> (Maybe Type, [Error])
checkPrimaryExpr expr state@(_, table) = case expr of
    (IntLit _) -> (Just $ BaseType "Int", [])
    (BoolLit _) -> (Just $ BaseType "Bool", [])
    (FloatLit _) -> (Just $ BaseType "Float", [])
    (StringLit _) -> (Just $ BaseType "String", [])
    (ArrayLit values loc) -> checkArrayLit values loc state
    (ParenExpr expr) -> checkExpr expr state
    (CallExpr name params loc) -> checkCallExpr name params loc state
    (AccessExpr varName loc)
        | VT.isVarDefined varName table -> (VT.getVar varName table, [])
        | otherwise -> (Nothing, [("Variable '" ++ varName ++ "' doesn't exist", loc)])

-- TODO: Handle builtins
checkCallExpr :: String -> [Expr] -> Location -> TcState -> (Maybe Type, [Error])
checkCallExpr name params loc state@(funcs, _)
    | M.member name funcs =
        if lenDif > 0 then
            (Nothing, [("Too many arguments passed into function '" ++ name ++ "'", loc)])
        else if lenDif < 0 then
            (Nothing, [("Too few arguments passed into function '" ++ name ++ "'", loc)])
        else checkParams params params' loc [] 0

    | otherwise = (Nothing, [("Function '" ++ name ++ "' doesn't exist", loc)])
    where
        (Function _ retType params' _ _) = fromJust $ M.lookup name funcs
        
        lenDif = length params - length params'

        -- Hello darkness my old friend...
        checkParams :: [Expr] -> [Parameter] -> Location -> [Error] -> Int -> (Maybe Type, [Error])
        checkParams [] [] _ errs _
            | null errs = (retType, errs)
            | otherwise = (Nothing, errs)
        checkParams [] _ _ _ _ = undefined -- Unreachable
        checkParams _ [] _ _ _ = undefined -- Unreachable
        checkParams (x:xs) ((Parameter _ ptype _):ps) l errs i =
            let (etype, errors) = checkExpr x state 
            in if compareType1 ptype etype
            then checkParams xs ps l errs (i+1)
            else checkParams xs ps l 
                (errs ++ errors ++ [("Type mismatch with argument " ++ show i, l)]) (i+1)


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
                errs' = if compareType2 firstType t
                        then errs
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
