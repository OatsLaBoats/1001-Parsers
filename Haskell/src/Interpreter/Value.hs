module Interpreter.Value 
    ( Value(..)
    ) where

data Value
    = IntValue Int
    | FloatValue Float
    | BoolValue Bool
    | StringValue String
    | ArrayValue [Value]
