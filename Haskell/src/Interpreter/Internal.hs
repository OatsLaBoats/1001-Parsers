module Interpreter.Internal
    ( Scope
    , Environment(..)
    , Value(..)
    , call
    ) where

import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe
import Ast

type Scope = Map String Value

-- TODO: Add builtins
data Environment = Environment
    { getFunctions :: Map String Function
    }

data Value
    = IntValue Int
    | FloatValue Float
    | BoolValue Bool
    | StringValue String
    | ArrayValue [Value]
    | NilValue

call :: String -> Environment -> [Value] -> IO Value
call fname env params
    | isJust function = evalFunction (fromJust function) env params
    | otherwise = undefined
    where
        function = Map.lookup fname $ getFunctions env
        builtin = undefined

evalFunction :: Function -> Environment -> [Value] -> IO Value
evalFunction (Function name _ fparams block _) env params =
    evalBlock block env scope
    where
        scope = foldr pred Map.empty (zip fparams params)
        pred ((Parameter name _ _), value) acc = Map.insert name value acc

evalBlock :: Block -> Environment -> Scope -> IO Value
evalBlock block env scope = undefined
    where
        loop [] env scope = undefined
        loop (s:stmts) env scope = case s of
            VariableStmt _ _ _ _-> do
                scope' <- evalVariableStmt s env scope
                loop stmts env scope'

evalVariableStmt :: Stmt -> Environment -> Scope -> IO Scope
evalVariableStmt stmt env scope = case stmt of
    VariableStmt name _ expr _ -> do
        value <- evalExpr expr env scope
        return $ Map.insert name value scope
    _ -> undefined

evalExpr :: Expr -> Environment -> Scope -> IO Value
evalExpr expr env scope = case expr of
    BinaryExpr _ _ _ _ -> undefined
    UnaryExpr _ _ _ -> undefined
    PrimaryExpr pexpr -> evalPrimaryExpr pexpr env scope

evalPrimaryExpr :: PrimaryExpr -> Environment -> Scope -> IO Value
evalPrimaryExpr expr env scope = case expr of
    IntLit value -> pure $ IntValue value
    FloatLit value -> pure $ FloatValue value
    BoolLit value -> pure $ BoolValue value
    StringLit value -> pure $ StringValue value
    AccessExpr varName _ -> pure $ fromJust $ Map.lookup varName scope
    ParenExpr expr' -> evalExpr expr' env scope
    ArrayLit exprs _ -> mapM (\e -> evalExpr e env scope) exprs >>= pure . ArrayValue
    CallExpr funcName params _ -> mapM (\e -> evalExpr e env scope) params >>= call funcName env
