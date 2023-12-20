module Parser.Stmt 
    ( parseBlock
    , parseStmt
    ) where

import Lexer hiding (scan)
import Ast
import Parser.Util
import Parser.Expr
import Parser.Type

parseBlock :: Parser Block
parseBlock tokens = do 
    (rest, _) <- expect TkLBrace tokens "Expected '{'"
    (rest, block) <- parseStmts [] rest
    (rest, _) <- expect TkRBrace rest "Expected '}'"
    return (rest, reverse block)
    where
        parseStmts acc tokens = case tokens of
            [] -> Right ([], acc)
            (Token TkLineEnd _ _ : rest) -> parseStmts acc rest
            (Token TkRBrace _ _ : _) -> Right (tokens, acc)
            _ -> do
                (rest, stmt) <- parseStmt tokens
                parseStmts (stmt : acc) rest

parseStmt :: Parser Stmt
parseStmt tokens = case tokens of
    (Token TkVar _ _ : _) -> parseVariableStmt tokens
    (Token TkReturn _ _ : _) -> parseReturnStmt tokens
    (Token TkPrint _ _ : _) -> parsePrintStmt tokens
    (Token TkWhile _ _ : _) -> parseWhileStmt tokens
    (Token TkIf _ _ : _) -> parseIfStmt tokens
    _
        | isAssignment && isIndex -> parseIndexAssignment tokens
        | isAssignment -> parseAssignment tokens
        | otherwise -> parseRawExprStmt tokens -- TODO: There is a bug with raw exprs. We can't just write 1 + 20 raw
    where
        isAssignment = 
            elem TkEqual .
            map (\(Token kind _ _) -> kind) . 
            takeWhile (\(Token kind _ _) -> kind /= TkLineEnd && kind /= TkEOF) $ tokens
        isIndex = case tokens of
            (Token TkIdentifier _ _ : Token TkLBracket _ _ : _) -> True
            _ -> False

parseVariableStmt :: Parser Stmt
parseVariableStmt tokens = do
    (rest, _) <- expect TkVar tokens "Expected 'var'"
    (rest, identifier) <- expect TkIdentifier rest "Expected variable name"
    (rest, _) <- expect TkColon rest "Expected ':'"
    (rest, varType) <- parseTypeAnnotation rest
    (rest, _) <- expect TkEqual rest "Expected '='"
    (rest, expr) <- parseExpr rest

    let (Token _ name loc) = identifier
    return (rest, VariableStmt name varType expr loc)

parseReturnStmt :: Parser Stmt
parseReturnStmt tokens = do
    (rest, keyword) <- expect TkReturn tokens "Expected 'return'"
    (rest, expr) <- parseExpr rest
    return (rest, ReturnStmt expr (getTokenLocation keyword))

parsePrintStmt :: Parser Stmt
parsePrintStmt tokens = do
    (rest, _) <- expect TkPrint tokens "Expected 'print'"
    (rest, expr) <- parseExpr rest
    return (rest, PrintStmt expr)

parseWhileStmt :: Parser Stmt
parseWhileStmt tokens = do
    (rest, keyword) <- expect TkWhile tokens "Expected 'while'"
    (rest, cond) <- parseExpr rest
    (rest, block) <- parseBlock rest
    return (rest, WhileStmt cond block (getTokenLocation keyword))

parseIfStmt :: Parser Stmt
parseIfStmt tokens = do
    (rest, keyword) <- expect TkIf tokens "Expected 'if'"
    (rest, cond) <- parseExpr rest
    (rest, block) <- parseBlock rest
    
    let rest' = dropWhile (\(Token kind _ _) -> kind == TkLineEnd) rest

    (rest, branch) <- parseBranch rest'
    return (rest, IfStmt cond block branch (getTokenLocation keyword))

parseBranch :: Parser (Maybe Stmt)
parseBranch tokens = case tokens of
    (Token TkElif _ _ : _) -> do
        (rest, stmt) <- parseElifStmt tokens
        return (rest, Just stmt)
    (Token TkElse _ _ : _) -> do
        (rest, stmt) <- parseElseStmt tokens
        return (rest, Just stmt)
    _ -> Right (tokens, Nothing)   

parseElifStmt :: Parser Stmt
parseElifStmt tokens = do
    (rest, keyword) <- expect TkElif tokens "Expected 'elif'"
    (rest, cond) <- parseExpr rest
    (rest, block) <- parseBlock rest

    let rest' = dropWhile (\(Token kind _ _) -> kind == TkLineEnd) rest

    (rest, branch) <- parseBranch rest'
    return (rest, ElifStmt cond block branch (getTokenLocation keyword))

parseElseStmt :: Parser Stmt
parseElseStmt tokens = do
    (rest, _) <- expect TkElse tokens "Expected 'else'"
    (rest, block) <- parseBlock rest
    return (rest, ElseStmt block)

parseAssignment :: Parser Stmt
parseAssignment tokens = do
    (rest, identifier) <- expect TkIdentifier tokens "Expected variable identifier"
    (rest, _) <- expect TkEqual rest "Expected '=' after variable name"
    (rest, expr) <- parseExpr rest
    
    let (Token _ name loc) = identifier
    return (rest, AssignmentStmt name expr loc)

parseIndexAssignment :: Parser Stmt
parseIndexAssignment tokens = do
    (rest, identifier) <- expect TkIdentifier tokens "Expected variable identifier"
    (rest, _) <- expect TkLBracket rest "Expected '['"
    (rest, idx) <- parseExpr rest
    (rest, _) <- expect TkRBracket rest "Expected ']'"
    (rest, _) <- expect TkEqual rest "Expected '=' after variable name"
    (rest, expr) <- parseExpr rest
    
    let (Token _ name loc) = identifier
    return (rest, IndexAssignmentStmt name idx expr loc)

parseRawExprStmt :: Parser Stmt
parseRawExprStmt tokens = do
    (rest, expr) <- parseExpr tokens
    return (rest, RawExprStmt expr)