module Analyzer.MissingReturn (missingReturnCheck) where

import Analyzer.Internal
import Ast

missingReturnCheck :: AnalyzerState -> AnalyzerState
missingReturnCheck s = foldr pred s (getTable s)
    where
        pred (Function name _ _ block loc) acc
            | not $ checkBlock block =
                acc { getErrors = (getErrors acc) ++ [("Function '" ++ name ++ "' is missing a 'return' statement", loc)] }
            | otherwise = acc

-- This is simpler than checking for variable errors because we don't need to update the state
-- Returns true if a return is found
checkBlock :: Block -> Bool
checkBlock block = case block of
    [] -> False
    ((ReturnStmt _ _) : _) -> True
    (stmt@(IfStmt _ _ _ _) : rest) -> 
        let result = checkIfStmt stmt
        in if result then result else checkBlock rest
    (_ : rest) -> checkBlock rest

checkIfStmt :: Stmt -> Bool
checkIfStmt (IfStmt _ block rest _) = case rest of
    (Just stmt@(ElifStmt _ _ _ _)) -> checkElifStmt stmt && blockHasReturn
    (Just (ElseStmt block')) -> checkBlock block' && blockHasReturn
    _ -> False
    where
        blockHasReturn = checkBlock block
checkIfStmt _ = undefined -- unreachable

checkElifStmt :: Stmt -> Bool
checkElifStmt (ElifStmt _ block rest _) = case rest of
    (Just stmt@(ElifStmt _ _ _ _)) -> checkElifStmt stmt && blockHasReturn
    (Just (ElseStmt block')) -> checkBlock block' && blockHasReturn
    _ -> False
    where
        blockHasReturn = checkBlock block
checkElifStmt _ = undefined -- unreachable

