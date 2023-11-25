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
    ((Token kind lexeme location) : rest) -> do
        return []