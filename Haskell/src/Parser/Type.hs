module Parser.Type
    ( parseTypeAnnotation
    ) where

import Ast
import Lexer
import Parser.Util

parseTypeAnnotation :: Parser Type
parseTypeAnnotation tokens = case tokens of
    (Token TkLBracket _ _ : rest) -> do
            (rest, t) <- parseTypeAnnotation rest
            (rest, _) <- expect TkRBracket rest "Expected ']'"
            return (rest, ArrayType t)
    _ -> do
        (rest, identifier) <- expect TkIdentifier tokens "Expected type name"
        return (rest, BaseType $ getTokenLexeme identifier)