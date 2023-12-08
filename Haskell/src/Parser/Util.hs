module Parser.Util 
    ( expect
    , match
    , peek
    , Parser
    ) where

import Error
import Lexer hiding (scan)

type Parser a = [Token] -> Either Error ([Token], a)

expect :: TokenKind -> [Token] -> Message -> Either Error ([Token], Token)
expect _ [] msg = Left (msg, (-1, -1))
expect expected (token : rest) msg
    | kind == expected = return (rest, token)
    | otherwise = Left (msg, location)
    where
        (Token kind _ location) = token

match :: TokenKind -> [Token] -> Bool
match _ [] = False
match expected (token : rest)
    | kind == expected = True
    | otherwise = False
    where (Token kind _ _) = token

peek :: [Token] -> Token
peek [] = Token TkEOF "" (-1, -1)
peek (x : xs) = x