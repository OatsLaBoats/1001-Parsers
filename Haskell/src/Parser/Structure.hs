module Parser.Structure ( parseFunction ) where

import Lexer hiding (scan)
import Ast
import Parser.Util
import Parser.Type
import Parser.Stmt

-- Note: Maybe use combinators
parseFunction :: Parser Function
parseFunction tokens = do
    (rest, _) <- expect TkFun tokens "Expected 'fun' keyword" 
    (rest, identifier) <- expect TkIdentifier rest "Expected identifier after 'fun' keyword"
    (rest, params) <- parseParameters rest 
    (rest, returnType) <- parseReturnType rest
    (rest, block) <- parseBlock rest

    let (Token _ name loc) = identifier 
    return (rest, Function name returnType params block loc)

parseParameters :: Parser [Parameter]
parseParameters tokens = do 
    (rest, _) <- expect TkLParen tokens "Expected '('"
    (rest, params) <- loop [] rest
    (rest, _) <- expect TkRParen rest "Expected ')'"
    return (rest, reverse params)
    where
        loop :: [Parameter] -> Parser [Parameter]
        loop acc tks = case tks of
            (Token TkRParen _ _ : _) -> Right (tks, acc)
            _ -> do
                (rest, param) <- parseParameter tks
                let rest' = if match TkComma rest then drop 1 rest else rest
                loop (param : acc) rest'

parseParameter :: Parser Parameter
parseParameter tokens = do
    (rest, identifier) <- expect TkIdentifier tokens "Expected parameter name after '('"
    (rest, _) <- expect TkColon rest "Expected ':' after parameter name"
    (rest, t) <- parseTypeAnnotation rest
    let (Token _ name loc) = identifier
    return (rest, Parameter name t loc)

parseReturnType :: Parser (Maybe Type)
parseReturnType tokens = case tokens of
    (Token TkColon _ _ : rest) -> do
        (rest, t ) <- parseTypeAnnotation rest
        return (rest, Just t)
    _ -> Right (tokens, Nothing)