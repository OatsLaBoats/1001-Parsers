module Analyzer.Internal 
    ( AnalyzerState(..)
    , FunctionMap
    , isReturnType
    , appendErrors
    ) where

import Data.Map (Map)
import Error
import Ast

type FunctionMap = Map String Function

data AnalyzerState = AnalyzerState
    { getTable :: FunctionMap
    , getErrors :: [Error]
    }
    
isReturnType :: Maybe Type -> Type -> Bool
isReturnType Nothing _ = False
isReturnType (Just t1) t2 = t1 == t2

appendErrors :: AnalyzerState -> [Error] -> AnalyzerState
appendErrors s errors = s { getErrors = getErrors s ++ errors }
