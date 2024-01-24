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
                    ((getErrors acc) ++ (reverse errs))

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
        let (newBlock, errors) = checkBlock block table
        in case errors of
            [] -> (Just stmt, table, [])
            _  -> 
                ( Just $ WhileStmt expr newBlock loc
                , table
                , errors
                )

    (IfStmt _ _ _ _) -> undefined
    
    (AssignmentStmt _ _ _) -> undefined
    
    (IndexAssignmentStmt _ _ _ _) -> undefined
    
    _ -> (Just stmt, table, [])