module Parser (parse) where

import Error
import Lexer hiding (scan)
import Ast
import Parser.Structure

parse :: [Token] -> Either Error Ast
parse = parse' []

parse' :: Ast -> [Token] -> Either Error Ast
parse' ast tokens = case tokens of
    [] -> return ast
    ((Token TkLineEnd _ _) : rest) -> parse' ast rest
    _ -> do
        (rest, f) <- parseFunction tokens
        parse' (f : ast) rest
