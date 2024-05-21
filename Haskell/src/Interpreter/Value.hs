module Interpreter.Value 
    ( Value(..)
    , extractInt
    , extractFloat
    , extractBool
    , extractString
    , extractArray
    ) where

data Value
    = IntValue Int
    | FloatValue Float
    | BoolValue Bool
    | StringValue String
    | ArrayValue [Value]
    | NilValue
    deriving Eq

extractInt :: Value -> Int
extractInt value = case value of
    IntValue v -> v
    _ -> undefined

extractFloat :: Value -> Float
extractFloat value = case value of
    FloatValue v -> v
    _ -> undefined

extractBool :: Value -> Bool
extractBool value = case value of
    BoolValue v -> v
    _ -> undefined

extractString :: Value -> String
extractString value = case value of
    StringValue v -> v
    _ -> undefined

extractArray :: Value -> [Value]
extractArray value = case value of
    ArrayValue v -> v
    _ -> undefined
