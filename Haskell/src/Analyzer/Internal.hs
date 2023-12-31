module Analyzer.Internal 
    ( AnalyzerState(..)
    , isReturnType
    ) where

import Data.Map (Map)
import Error
import Ast

data AnalyzerState = AnalyzerState
    { getTable :: Map String Function
    , getErrors :: [Error]
    }
    
isReturnType :: Maybe Type -> Type -> Bool
isReturnType Nothing _ = False
isReturnType (Just t1) t2 = t1 == t2