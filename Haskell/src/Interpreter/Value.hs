module Interpreter.Value 
    ( Value(..)
    , negateValue
    , notValue
    ) where

data Value
    = IntValue Int
    | FloatValue Float
    | BoolValue Bool
    | StringValue String
    | ArrayValue [Value]
    | NilValue

negateValue :: Value -> Value
negateValue value = case value of
    IntValue v -> IntValue $ negate v
    FloatValue v -> FloatValue $ negate v
    _ -> undefined

notValue :: Value -> Value
notValue value = case value of
    BoolValue v -> BoolValue $ not v
    _ -> undefined

