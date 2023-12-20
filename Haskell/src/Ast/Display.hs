module Ast.Display (displayAst) where

import Ast

displayAst :: Ast -> String
displayAst = concat . foldr (\f acc -> acc ++ [displayFunction f]) []

displayFunction :: Function -> String
displayFunction (Function name retType params block _) =
    "<Function>\n" ++
    indent 0 ++ "name: " ++ name ++ "\n" ++
    ifType (\t -> indent 0 ++ "returns: " ++ displayType t) retType ++ "\n" ++
    doIf 
        (\_ -> indent 0 ++ "parameters: [" ++ foldr (\p acc -> displayParameter p ++ acc) "" params  ++ "]") 
        (not $ null params) ++ "\n" ++
    displayBlock block 1

displayBlock :: Block -> Int -> String
displayBlock b i =
    indent i ++ "<Block>\n" ++
    foldr (\s acc -> displayStmt s (i+1) ++ acc) "" b

displayStmt :: Stmt -> Int -> String
displayStmt stmt i = case stmt of
    (VariableStmt name t expr _) ->
        indent i ++ "<VariableStmt>\n" ++
        indent (i+1) ++ "name: " ++ name ++ "\n" ++
        indent (i+1) ++ "type: " ++ displayType t  ++ "\n" ++
        indent (i+1) ++ "value:\n" ++ displayExpr expr (i+2)
    (ReturnStmt expr _) ->
        indent i ++ "<ReturnStmt>\n" ++
        indent (i+1) ++ "expr:\n" ++ displayExpr expr (i+2)
    (PrintStmt expr) ->
        indent i ++ "<PrintStmt>\n" ++
        indent (i+1) ++ "expr:\n" ++ displayExpr expr (i+2)
    (WhileStmt cond block _) ->
        indent i ++ "<WhileStmt>\n" ++
        indent (i+1) ++ "condition:\n" ++ displayExpr cond (i+2) ++
        displayBlock block (i+1)
    (IfStmt cond block next _) ->
        indent i ++ "<IfStmt>\n" ++
        indent (i+1) ++ "condition:\n" ++ displayExpr cond (i+2) ++
        displayBlock block (i+1) ++
        ifJust (\s -> displayStmt s i) next ""
    (ElifStmt cond block next _) ->
        indent i ++ "<ElifStmt>\n" ++
        indent (i+1) ++ "condition:\n" ++ displayExpr cond (i+2) ++
        displayBlock block (i+1) ++
        ifJust (\s -> displayStmt s i) next ""
    (ElseStmt block) ->
        indent i ++ "<ElseStmt>\n" ++
        displayBlock block (i+1)
    (AssignmentStmt name expr _) ->
        indent i ++ "<AssignmentStmt>\n" ++
        indent (i+1) ++ "name: " ++ name ++ "\n" ++
        indent (i+1) ++ "expr:\n" ++ displayExpr expr (i+2)
    (IndexAssignmentStmt name idx expr _) ->
        indent i ++ "<IndexAssignmentStmt>\n" ++
        indent (i+1) ++ "name: " ++ name ++ "\n" ++
        indent (i+1) ++ "index:\n" ++ displayExpr idx (i+2) ++
        indent (i+1) ++ "expr:\n" ++ displayExpr expr (i+2)
    (RawExprStmt expr) ->
        indent i ++ "<RawExprStmt>\n" ++
        indent (i+1) ++ "expr:\n" ++ displayExpr expr (i+2)

displayExpr :: Expr -> Int -> String
displayExpr expr i = case expr of
    (PrimaryExpr e) -> displayPrimaryExpr e i
    (BinaryExpr op lhs rhs _) ->
        indent i ++ "<BinaryExpr>\n" ++
        indent (i+1) ++ "operator: " ++ displayBinaryOperator op ++ "\n" ++
        indent (i+1) ++ "lhs:\n" ++ displayExpr lhs (i+2) ++
        indent (i+1) ++ "rhs:\n" ++ displayExpr rhs (i+2)
    (UnaryExpr op e _) ->
        indent i ++ "<UnaryExpr>\n" ++
        indent (i+1) ++ "operator: " ++ displayUnaryOperator op ++ "\n" ++
        indent (i+1) ++ "expr:\n" ++ displayExpr e (i+2)

displayUnaryOperator :: UnaryOperator -> String
displayUnaryOperator op = case op of
    OpNeg -> "-"
    OpNot -> "not"

displayBinaryOperator :: BinaryOperator -> String
displayBinaryOperator op = case op of
    OpOr    -> "or"
    OpAnd   -> "and"
    OpEq    -> "=="
    OpNeq   -> "!="
    OpGt    -> ">"
    OpLt    -> "<"
    OpGtEq  -> ">="
    OpLtEq  -> "<="
    OpAdd   -> "+"
    OpSub   -> "-"
    OpMul   -> "*"
    OpDiv   -> "/"
    OpMod   -> "%"
    OpIndex -> "[]"

displayPrimaryExpr :: PrimaryExpr -> Int -> String
displayPrimaryExpr expr i =
    indent i ++ "<PrimaryExpr>\n" ++
    case expr of
        (IntLit n)       -> indent (i+1) ++ "literal(Int): " ++ show n ++ "\n"
        (FloatLit f)     -> indent (i+1) ++ "literal(Float): " ++ show f ++ "\n"
        (BoolLit b)      -> indent (i+1) ++ "literal(Bool): " ++ show b ++ "\n"
        (StringLit s)    -> indent (i+1) ++ "literal(String): \"" ++ s ++ "\"\n"
        (AccessExpr s _) -> indent (i+1) ++ "identifier(access): " ++ s ++ "\n"
        (CallExpr s xs _) ->
            indent (i+1) ++ "identifier(call): " ++ s ++ "\n" ++
            indent (i+1) ++ "parameters:\n" ++
            foldr (\e acc -> displayExpr e (i+2) ++ acc) "" xs
        (ArrayLit xs _) ->
            indent (i+1) ++ "literal(Array):\n" ++
            foldr (\e acc -> displayExpr e (i+2) ++ acc) "" xs
        (ParenExpr e) -> displayExpr e (i+1)

displayParameter :: Parameter -> String
displayParameter (Parameter name t _) = '(' : name ++ ": " ++ displayType t ++ ")"

displayType :: Type -> String
displayType (BaseType name) = name
displayType (ArrayType t)   = '[' : displayType t ++ "]"

ifJust :: (a -> b) -> Maybe a -> b -> b
ifJust _ Nothing alt = alt
ifJust f (Just x) _  = f x

ifType :: (Type -> String) -> Maybe Type -> String
ifType _ (Nothing) = ""
ifType f (Just t)  = f t

doIf :: (() -> String) -> Bool -> String
doIf _ False = ""
doIf f True  = f ()

indent :: Int -> String
indent i = foldr (\_ acc -> acc ++ "  ") "" [0..i]