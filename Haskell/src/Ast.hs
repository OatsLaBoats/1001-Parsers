module Ast 
    ( Ast
    , Type(..)
    , Function(..)
    , Parameter(..)
    , Block
    , Stmt(..)
    , Expr(..)
    , PrimaryExpr(..)
    , UnaryOperator(..)
    , BinaryOperator(..)
    , getFuncName
    , getFuncLoc
    , getArrayInternal
    ) where

import Error

type Name = String
type Ast = [Function]

data Type
    = BaseType String
    | ArrayType Type
    deriving (Show, Eq)

getArrayInternal :: Type -> Maybe Type
getArrayInternal t = case t of
    BaseType _ -> Nothing
    ArrayType x -> Just x

-- name returnType parameterList block loc
data Function = Function Name (Maybe Type) [Parameter] Block Location deriving Show

getFuncName :: Function -> Name
getFuncName (Function name _ _ _ _) = name

getFuncLoc :: Function -> Location
getFuncLoc (Function _ _ _ _ loc) = loc

data Parameter = Parameter Name Type Location deriving Show
type Block = [Stmt]

data Stmt
    = VariableStmt Name Type Expr Location
    | ReturnStmt Expr Location
    | PrintStmt Expr
    | WhileStmt Expr Block Location
    | IfStmt Expr Block (Maybe Stmt) Location -- cond block elif/else loc
    | ElifStmt Expr Block (Maybe Stmt) Location
    | ElseStmt Block
    | AssignmentStmt Name Expr Location 
    | IndexAssignmentStmt Name Expr Expr Location -- name index expr loc
    | RawExprStmt Expr
    deriving Show

data Expr
    = BinaryExpr BinaryOperator Expr Expr Location
    | UnaryExpr UnaryOperator Expr Location
    | PrimaryExpr PrimaryExpr
    deriving Show

data PrimaryExpr
    = IntLit Int
    | FloatLit Float
    | BoolLit Bool
    | StringLit String
    | ArrayLit [Expr] Location
    | AccessExpr String Location
    | CallExpr String [Expr] Location
    | ParenExpr Expr
    deriving Show

data UnaryOperator = OpNeg | OpNot deriving Show

data BinaryOperator
    = OpOr
    | OpAnd
    | OpEq
    | OpNeq
    | OpGt
    | OpLt
    | OpGtEq
    | OpLtEq
    | OpAdd
    | OpSub
    | OpMul
    | OpDiv
    | OpMod
    | OpIndex
    deriving Show
