module Parser (parse) where

import Error
import Lexer hiding (scan)
import Ast
import Parser.Structure

parse :: [Token] -> Either Error Ast
parse = parse_ []

parse_ :: Ast -> [Token] -> Either Error Ast
parse_ ast tokens = case tokens of
    [] -> return ast
    ((Token TkLineEnd _ _) : rest) -> parse_ ast rest
    _ -> do
        (rest, f) <- parseFunction tokens
        newAst <- parse_ (f : ast) rest
        return newAst