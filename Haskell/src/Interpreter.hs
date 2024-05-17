module Interpreter
    ( Environment(..)
    , Value(..)
    , eval
    , setupEnv
    , call
    ) where

import qualified Data.Map as Map
import Ast
import Interpreter.Internal

eval :: Ast -> IO Int
eval ast = return 0
    where
        env = setupEnv ast

setupEnv :: Ast -> Environment
setupEnv ast = Environment (foldr pred Map.empty ast)
    where
        pred f@(Function name _ _ _ _) acc = Map.insert name f acc
