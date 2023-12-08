module Parser.Expr 
    ( parseExpr
    ) where

import Ast
import Parser.Util
import Lexer

parseExpr :: Parser Expr
parseExpr = parseFactorExpr

parseOrExpr :: Parser Expr
parseOrExpr tokens = do
    (rest, expr) <- parseAndExpr tokens
    prat rest expr
    where 
        prat (Token TkOr _ loc : rest) lhs = do
            (rest, rhs) <- parseExpr rest
            prat rest (BinaryExpr OpOr lhs rhs loc)
        prat ts lhs = Right (ts, lhs)

parseAndExpr :: Parser Expr
parseAndExpr tokens = do
    (rest, expr) <- parseEqExpr tokens
    prat rest expr
    where 
        prat (Token TkAnd _ loc : rest) lhs = do
            (rest, rhs) <- parseExpr rest
            prat rest (BinaryExpr OpAnd lhs rhs loc)
        prat ts lhs = Right (ts, lhs)

parseEqExpr :: Parser Expr
parseEqExpr tokens = do
    (rest, expr) <- parseCompExpr tokens
    prat rest expr
    where 
        prat (Token TkEq _ loc : rest) lhs = do
            (rest, rhs) <- parseExpr rest
            prat rest (BinaryExpr OpEq lhs rhs loc)
        prat (Token TkNeq _ loc : rest) lhs = do
            (rest, rhs) <- parseExpr rest
            prat rest (BinaryExpr OpNeq lhs rhs loc)
        prat ts lhs = Right (ts, lhs)


parseCompExpr :: Parser Expr
parseCompExpr tokens = do
    (rest, expr) <- parseTermExpr tokens
    prat rest expr
    where 
        prat (Token TkGt _ loc : rest) lhs = do
            (rest, rhs) <- parseExpr rest
            prat rest (BinaryExpr OpGt lhs rhs loc)
        prat (Token TkLt _ loc : rest) lhs = do
            (rest, rhs) <- parseExpr rest
            prat rest (BinaryExpr OpLt lhs rhs loc)
        prat (Token TkGtEq _ loc : rest) lhs = do
            (rest, rhs) <- parseExpr rest
            prat rest (BinaryExpr OpGtEq lhs rhs loc)
        prat (Token TkLtEq _ loc : rest) lhs = do
            (rest, rhs) <- parseExpr rest
            prat rest (BinaryExpr OpLtEq lhs rhs loc)
        prat ts lhs = Right (ts, lhs)

parseTermExpr :: Parser Expr
parseTermExpr tokens = do
    (rest, expr) <- parseFactorExpr tokens
    prat rest expr
    where 
        prat (Token TkPlus _ loc : rest) lhs = do
            (rest, rhs) <- parseExpr rest
            prat rest (BinaryExpr OpAdd lhs rhs loc)
        prat (Token TkMinus _ loc : rest) lhs = do
            (rest, rhs) <- parseExpr rest
            prat rest (BinaryExpr OpSub lhs rhs loc)
        prat ts lhs = Right (ts, lhs)

parseFactorExpr :: Parser Expr
parseFactorExpr tokens = do
    (rest, expr) <- parseUnaryExpr tokens
    prat rest expr
    where 
        prat (Token TkMul _ loc : rest) lhs = do
            (rest, rhs) <- parseExpr rest
            prat rest (BinaryExpr OpMul lhs rhs loc)
        prat (Token TkDiv _ loc : rest) lhs = do
            (rest, rhs) <- parseExpr rest
            prat rest (BinaryExpr OpDiv lhs rhs loc)
        prat (Token TkMod _ loc : rest) lhs = do
            (rest, rhs) <- parseExpr rest
            prat rest (BinaryExpr OpMod lhs rhs loc)
        prat ts lhs = Right (ts, lhs)

parseUnaryExpr :: Parser Expr
parseUnaryExpr tokens = case tokens of
    (Token TkMinus _ loc : rest) -> do
        (rest, expr) <- parseUnaryExpr rest
        return (rest, UnaryExpr OpNeg expr loc)
    (Token TkNot _ loc : rest) -> do
        (rest, expr) <- parseUnaryExpr rest
        return (rest, UnaryExpr OpNot expr loc)
    _ -> parseIndexExpr tokens

parseIndexExpr :: Parser Expr
parseIndexExpr tokens = do
    (rest, expr) <- parsePrimaryExpr tokens
    prat rest expr
    where 
        prat (Token TkLBracket _ loc : rest) lhs = do
            (rest, idx) <- parseExpr rest
            (rest, _) <- expect TkRBracket rest "Expected ']'"
            prat rest (BinaryExpr OpIndex lhs idx loc)
        prat ts lhs = Right (ts, lhs)

parsePrimaryExpr :: Parser Expr
parsePrimaryExpr tokens = case tokens of
    (Token TkLParen _ _ : rest) -> do
        (rest, e) <- parseExpr rest
        (rest, _) <- expect TkRParen rest "Expected ')'"
        return (rest, PrimaryExpr $ ParenExpr e)
    
    (Token TkNumberLit lex _ : rest)
        | '.' `elem` lex -> Right (rest, PrimaryExpr $ FloatLit (read lex))
        | otherwise      -> Right (rest, PrimaryExpr $ IntLit (read lex))
    
    (Token TkStringLit lex _ : rest) -> Right (rest, PrimaryExpr $ StringLit lex)
    
    (Token TkBoolLit "false" _ : rest) -> Right (rest, PrimaryExpr $ BoolLit False)
    (Token TkBoolLit "true" _ : rest)  -> Right (rest, PrimaryExpr $ BoolLit True)
    
    (Token TkIdentifier lex loc : Token TkLParen _ _ : rest) -> do
        (rest, exprs) <- parseCallParams [] rest
        (rest, _) <- expect TkRParen rest "Expected ')'"
        return (rest, PrimaryExpr $ CallExpr lex exprs loc)
    
    (Token TkIdentifier lex loc : rest) -> Right (rest, PrimaryExpr $ AccessExpr lex loc)
    
    (Token TkLBracket _ loc : rest) -> do
        (rest, exprs) <- parseArrayLit [] rest
        (rest, _) <- expect TkRBracket rest "Expected ']'"
        return (rest, PrimaryExpr $ ArrayLit exprs loc)
    
    (Token _ _ loc : rest) -> Left ("Expected expression", loc)
        
parseArrayLit :: [Expr] -> Parser [Expr]
parseArrayLit acc tokens
    | match TkRBracket tokens = Right (tokens, reverse acc)
    | otherwise = do
        (rest, expr) <- parseExpr tokens
        if match TkRBracket rest
            then return (rest, reverse acc)
            else do
                (rest, _) <- expect TkComma rest "Expected ',' after expression"
                parseArrayLit (expr : acc) rest

parseCallParams :: [Expr] -> Parser [Expr]
parseCallParams acc tokens
    | match TkRParen tokens = Right (tokens, reverse acc)
    | otherwise = do
        (rest, expr) <- parseExpr tokens
        if match TkRParen rest
            then return (rest, reverse acc)
            else do
                (rest, _) <- expect TkComma rest "Expected ',' after expression"
                parseCallParams (expr : acc) rest