module Interpreter
    ( Environment(..)
    , Value(..)
    , eval
    , setupEnv
    , call
    ) where

import qualified Data.Map as Map
import Data.Functor
import Ast
import Interpreter.Internal
import Interpreter.Value

eval :: Ast -> IO Int
eval ast = call "main" env [] <&> extractInt
    where
        env = setupEnv ast

setupEnv :: Ast -> Environment
setupEnv ast = Environment (foldr pred Map.empty ast)
    where
        pred f@(Function name _ _ _ _) acc = Map.insert name f acc
