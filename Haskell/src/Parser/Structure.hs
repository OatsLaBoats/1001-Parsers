module Parser.Structure 
    ( parseFunction
    ) where

import Error
import Lexer hiding (scan)
import Ast
import Parser.Util

-- TODO: Move parse function to a new module called Top which is for top level definitions

-- Maybe create a type like Parser a = [Token] -> Either Error ([Token], a)
-- Note: Maybe use combinators
parseFunction :: Parser Function
parseFunction tokens = do
    (rest, _) <- expect TkFun tokens "Expected 'fun' keyword" 
    (rest, identifier) <- expect TkIdentifier rest "Expected identifier after 'fun' keyword"

    let name = getTokenLexeme identifier 
    return ([], Function name Nothing [] [] (-1, -1))