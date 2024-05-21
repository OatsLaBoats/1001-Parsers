module Interpreter.Internal
    ( Scope
    , Environment(..)
    , Value(..)
    , call
    ) where

import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe
import GHC.Float
import Ast
import Interpreter.Value

type Scope = Map String Value

-- TODO: Add builtins
data Environment = Environment
    { getFunctions :: Map String Function
    }

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
evalBlock block env scope = loop block env scope Nothing
    where
        loop [] _ _ _ = pure NilValue
        loop _ _ _ (Just retVal) = pure retVal
        loop (s:stmts) env scope _ = case s of
            VariableStmt _ _ _ _-> do
                scope' <- evalVariableStmt s env scope
                loop stmts env scope' Nothing

evalVariableStmt :: Stmt -> Environment -> Scope -> IO Scope
evalVariableStmt stmt env scope = case stmt of
    VariableStmt name _ expr _ -> do
        value <- evalExpr expr env scope
        return $ Map.insert name value scope
    _ -> undefined

evalExpr :: Expr -> Environment -> Scope -> IO Value
evalExpr expr env scope = case expr of
    BinaryExpr _ _ _ _ -> evalBinaryExpr expr env scope
    UnaryExpr _ _ _ -> evalUnaryExpr expr env scope
    PrimaryExpr pexpr -> evalPrimaryExpr pexpr env scope

evalBinaryExpr :: Expr -> Environment -> Scope -> IO Value
evalBinaryExpr expr env scope = case op of
    OpOr -> simpleOp BoolValue extractBool (||)
    OpAnd -> simpleOp BoolValue extractBool (&&)
    OpEq -> opEq
    OpNeq -> BoolValue . not . extractBool <$> opEq
    OpGt -> simpleOp BoolValue id gtValues
    OpLt -> simpleOp BoolValue id ltValues
    OpGtEq -> undefined
    OpLtEq -> undefined
    OpAdd -> undefined
    OpSub -> undefined
    OpMul -> undefined
    OpDiv -> undefined
    OpMod -> undefined
    OpIndex -> undefined
    where
        (op, lhs', rhs') = case expr of
            BinaryExpr op lhs rhs _ -> (op, lhs, rhs)
            _ -> undefined
        
        lhs = evalExpr lhs' env scope
        rhs = evalExpr rhs' env scope

        opEq = simpleOp BoolValue id eqValues

        simpleOp constructor extractor operator =
            liftA2 (\v1 v2 -> constructor $ extractor v1 `operator` extractor v2) lhs rhs

        -- NOTE: This is beyong scuffed, I'm near certain that there is an abstraction
        --       which solves this more elegantly but I can't think of it right now.
        ltValues v1 v2 = case v1 of
            IntValue v1' -> case v2 of
                IntValue v2' -> v1' < v2'
                FloatValue v2' -> (int2Float v1') < v2'
                _ -> undefined
            FloatValue v1' -> case v2 of
                FloatValue v2' -> v1' < v2'
                IntValue v2' -> v1' < (int2Float v2')
                _ -> undefined
            _ -> undefined

        gtValues v1 v2 = case v1 of
            IntValue v1' -> case v2 of
                IntValue v2' -> v1' > v2'
                FloatValue v2' -> (int2Float v1') > v2'
                _ -> undefined
            FloatValue v1' -> case v2 of
                FloatValue v2' -> v1' > v2'
                IntValue v2' -> v1' > (int2Float v2')
                _ -> undefined
            _ -> undefined

        eqValues v1 v2 = case v1 of
            IntValue v1' -> case v2 of
                IntValue v2' -> v1' == v2'
                FloatValue v2' -> (int2Float v1') == v2'
                _ -> undefined
            FloatValue v1' -> case v2 of
                IntValue v2' -> v1' == (int2Float v2')
                FloatValue v2' -> v1' == v2'
                _ -> undefined
            BoolValue v1' -> v1' == (extractBool v2)
            StringValue v1' -> v1' == (extractString v2)
            ArrayValue v1' -> v1' == (extractArray v2)
            NilValue -> undefined


evalUnaryExpr :: Expr -> Environment -> Scope -> IO Value
evalUnaryExpr expr env scope = case op of
    OpNeg -> negateValue <$> (evalExpr expr' env scope)
    OpNot -> notValue <$> (evalExpr expr' env scope)
    where
        (op, expr') = case expr of
            UnaryExpr op expr' _ -> (op, expr')
            _ -> undefined

        negateValue value = case value of
            IntValue v -> IntValue $ negate v
            FloatValue v -> FloatValue $ negate v
            _ -> undefined

        notValue value = case value of
            BoolValue v -> BoolValue $ not v
            _ -> undefined


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
