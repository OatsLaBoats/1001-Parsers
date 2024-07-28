module Analyzer.Variables (duplicateVariableCheck) where

import qualified Data.Map as Map

import Analyzer.Internal
import Error
import Ast

import VarTable (VarTable)
import qualified VarTable as VT

duplicateVariableCheck :: AnalyzerState -> AnalyzerState
duplicateVariableCheck s = foldr pred s (getTable s)
    where
        pred f acc =
            let (nf, errs) = checkFunction f
            in  AnalyzerState 
                    (Map.insert (getFuncName nf) nf (getTable acc))
                    (getBuiltins acc)
                    (getErrors acc ++ errs)

checkFunction :: Function -> (Function, [Error])
checkFunction (Function name retType params block loc) =
    let (params', errs, table) = checkParams params VT.empty
        (block', errs') = checkBlock block table
    in  (Function name retType params' block' loc, errs ++ errs')

checkParams :: [Parameter] -> VarTable -> ([Parameter], [Error], VarTable)
checkParams = checkParams_ [] []
    where 
        checkParams_ acc errs params table = case params of
            [] -> (reverse acc, errs, table)
            (param@(Parameter name t loc) : rest)
                | VT.isVarDefined name table ->
                    checkParams_ 
                        acc 
                        (("Parameter '" ++ name ++ "' is already defined", loc) : errs) 
                        rest 
                        table
                | otherwise -> 
                    checkParams_ 
                        (param : acc) 
                        errs 
                        rest 
                        (VT.defineVar name t table)

checkBlock :: Block -> VarTable -> (Block, [Error])
checkBlock block = checkBlock_ [] [] block . VT.new
    where
        checkBlock_ acc errors block table = case block of
            [] -> (reverse acc, errors)
            (stmt : rest) -> case checkStmt stmt table of
                (Nothing, table', errors') ->
                    checkBlock_ 
                        acc 
                        (errors ++ errors') 
                        rest 
                        table'
                (Just stmt', table', errors') -> 
                    checkBlock_
                        (stmt' : acc) 
                        (errors ++ errors') 
                        rest 
                        table'
                    
-- We need to return the var table because not every statement creates a block and this will be called in loop like manner.
checkStmt :: Stmt -> VarTable -> (Maybe Stmt, VarTable, [Error])
checkStmt stmt table = case stmt of
    (VariableStmt name vType _ loc)
        | VT.isVarDefined name table -> 
            ( Nothing
            , table
            , [("Variable '" ++ name ++ "' is already defined", loc)]
            )
        | otherwise -> 
            ( Just stmt
            , VT.defineVar name vType table
            , []
            )
    
    (WhileStmt expr block loc) ->
        let (block', errors) = checkBlock block table
        in case errors of
            [] -> (Just stmt, table, [])
            _  -> 
                ( Just $ WhileStmt expr block' loc
                , table
                , errors
                )

    (IfStmt expr block elifStmt loc) ->
        let (block', errors) = checkBlock block table
            (elifStmt', errors') = checkElifStmt elifStmt table
        in ( Just $ IfStmt expr block' elifStmt' loc
           , table
           , errors ++ errors'
           )
    
    (AssignmentStmt name _ loc)
        | VT.isVarDefined name table -> (Just stmt, table, [])
        | otherwise -> 
            ( Nothing
            , table
            , [("Variable '" ++ name ++ "' is undefined", loc)]
            )
    
    (IndexAssignmentStmt name _ _ loc)
        | VT.isVarDefined name table -> (Just stmt, table, [])
        | otherwise -> 
            ( Nothing
            , table
            , [("Variable '" ++ name ++ "' is undefined", loc)]
            )
    
    _ -> (Just stmt, table, [])

checkElifStmt :: Maybe Stmt -> VarTable -> (Maybe Stmt, [Error])
checkElifStmt stmt table = case stmt of
    Nothing -> (stmt, [])

    (Just (ElifStmt expr block elifStmt loc)) ->
        let (block', errors) = checkBlock block table
            (elifStmt', errors') = checkElifStmt elifStmt table
        in  ( Just $ ElifStmt expr block' elifStmt' loc
            , errors ++ errors'
            )

    (Just (ElseStmt block)) ->
        let (block', errors) = checkBlock block table
        in  (Just $ ElseStmt block', errors)

    _ -> undefined -- Impossible to reach
