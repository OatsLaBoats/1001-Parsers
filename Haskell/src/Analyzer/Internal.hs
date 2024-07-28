module Analyzer.Internal 
    ( AnalyzerState(..)
    , Builtin(..)
    , FunctionMap
    , BuiltinMap
    , isReturnType
    , appendErrors
    ) where

import Data.Map (Map)
import Error
import Ast

data Builtin = Builtin [Type] (Maybe Type)

type FunctionMap = Map String Function
type BuiltinMap = Map String Builtin

data AnalyzerState = AnalyzerState
    { getTable :: FunctionMap -- TODO: Maybe rename?
    , getBuiltins :: BuiltinMap
    , getErrors :: [Error]
    }
    
isReturnType :: Maybe Type -> Type -> Bool
isReturnType Nothing _ = False
isReturnType (Just t1) t2 = t1 == t2

appendErrors :: AnalyzerState -> [Error] -> AnalyzerState
appendErrors s errors = s { getErrors = getErrors s ++ errors }
