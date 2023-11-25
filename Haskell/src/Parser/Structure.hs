module Parser.Structure 
    ( parseFunction
    ) where

import Error
import Lexer hiding (scan)
import Ast

parseFunction :: [Token] -> Either Error (Function, [Token])
parseFunction tokens = do
    return $ (Function "" Nothing [] [] (-1, -1), [])