module Parser (parse) where

import Error
import Lexer hiding (scan)
import Ast
import Parser.Structure

parse :: [Token] -> Either Error Ast
parse = parse_ []

parse_ :: Ast -> [Token] -> Either Error Ast
parse_ ast tokens = case tokens of
    []                   -> Right ast
    (TkLineEnd _ : rest) -> parse_ ast rest
    _                    -> do
        let (result, rest) = parseFunction tokens
        f <- result
        newAst <- parse_ (f : ast) rest
        return newAst