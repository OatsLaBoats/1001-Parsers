module Analyzer.Variables (duplicateVariableCheck) where

import Analyzer.Internal
import Ast

duplicateVariableCheck :: AnalyzerState -> AnalyzerState
duplicateVariableCheck s = foldr checkFunction s (getTable s)

checkFunction :: Function -> AnalyzerState -> AnalyzerState
checkFunction = undefined