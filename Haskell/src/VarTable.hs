module VarTable 
    ( VarTable(..)
    , empty
    , new
    , isVarDefined
    , defineVar
    , getVar
    ) where

import Data.Map (Map)
import qualified Data.Map as Map

import Ast

-- TODO: I don't think I am using the 'new' function and getParent field.
--       Since haskell is immutable we don't need to have this whole child/parent reletation ship.

data VarTable = VarTable 
    { getVars :: Map Name Type
    , getParent :: Maybe VarTable
    }

type Name = String

-- TODO: Rename to emptyVarTable
empty :: VarTable
empty = VarTable Map.empty Nothing

new :: VarTable -> VarTable
new = VarTable Map.empty . Just

isVarDefined :: Name -> VarTable -> Bool
isVarDefined name (VarTable tbl parent)
    | name `Map.member` tbl = True
    | otherwise = case parent of
        Nothing -> False
        Just p -> isVarDefined name p

defineVar :: Name -> Type -> VarTable -> VarTable
defineVar name t tbl = tbl { getVars = Map.insert name t (getVars tbl) }

getVar :: Name -> VarTable -> Maybe Type
getVar name (VarTable tbl parent) = 
    case Map.lookup name tbl of
        Just v -> Just v
        Nothing ->
            case parent of
                Nothing -> Nothing
                Just p -> getVar name p
