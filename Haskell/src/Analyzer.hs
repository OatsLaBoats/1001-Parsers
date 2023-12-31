module Analyzer (analyze) where

import qualified Data.Map as Map
import Data.Function

import Ast
import Error
import Analyzer.Internal
import Analyzer.Variables

analyze :: Ast -> [Error]
analyze ast = 
    duplicateFunctionCheck ast (AnalyzerState Map.empty []) &
    validMainCheck &
    getErrors

duplicateFunctionCheck :: Ast -> AnalyzerState -> AnalyzerState
duplicateFunctionCheck ast s = foldr predicate s ast
    where
        predicate f acc
            | fid `Map.member` tab = acc { getErrors = (errMsg, floc) : (getErrors s) }
            | otherwise = acc { getTable = Map.insert fid f tab }
            where 
                fid = getFuncName f
                floc = getFuncLoc f
                tab = getTable acc
                errMsg = "Function " ++ fid ++ " is already defined"

validMainCheck :: AnalyzerState -> AnalyzerState
validMainCheck s = case entrypoint of
    Nothing -> s { getErrors = ("Missing 'main' function", (-1, -1)) : (getErrors s) }
    Just (Function _ retType params _ loc) ->
        arityCheck params loc s &
        returnTypeCheck retType loc &
        reverseState
    where
        entrypoint = Map.lookup "main" (getTable s)
        reverseState (AnalyzerState tab errs) = AnalyzerState tab (reverse errs)
        
        arityCheck params loc s
            | null params = s
            | otherwise = s { getErrors = ("'main' should have no parameters", loc) : (getErrors s) }
        
        returnTypeCheck rt loc s
            | rt `isReturnType` (BaseType "Int") = s
            | otherwise = s { getErrors = ("'main' should have a return type of 'Int'", loc) : (getErrors s) }