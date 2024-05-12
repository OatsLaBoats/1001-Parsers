module Interpreter
    ( Environment
    , Value
    , eval
    , setupEnv
    , call
    ) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Ast
import Interpreter.Value

-- TODO: Add builtins
data Environment = Environment
    { getFunctions :: Map String Function
    }

eval :: Ast -> IO Int
eval ast = return 0
    where
        env = setupEnv ast

setupEnv :: Ast -> Environment
setupEnv ast = Environment (foldr pred Map.empty ast)
    where
        pred f@(Function name _ _ _ _) acc = Map.insert name f acc

call :: String -> Environment -> [Value] -> Value
call fname env params
    | isJust function = evalFunction (fromJust function) env params
    where
        function = Map.lookup fname $ getFunctions env
        builtin = undefined

evalFunction :: Function -> Environment -> [Value] -> Value
evalFunction f env params = undefined