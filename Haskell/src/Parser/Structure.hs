module Parser.Structure 
    ( parseFunction
    ) where

import Error
import Lexer hiding (scan)
import Ast

parseFunction :: [Token] -> (Either Error Function, [Token])
parseFunction (x:tokens) = (Right $ Function "" (BaseType "") [] [] (0, 0), tokens)