module Interpreter.Value 
    ( Value(..)
    , extractInt
    , extractFloat
    , extractBool
    , extractString
    , extractArray
    , eqValues
    , gtValues
    , ltValues
    , gtEqValues
    , ltEqValues
    , addValues
    , subValues
    , mulValues
    , divValues
    ) where

import GHC.Float

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

divValues :: Value -> Value -> Value
divValues v1 v2 = case v1 of
    IntValue v1' -> case v2 of
        IntValue v2' -> FloatValue $ (int2Float v1') / (int2Float v2')
        FloatValue v2' -> FloatValue $ (int2Float v1') * v2'
        _ -> undefined
    FloatValue v1' -> case v2 of
        FloatValue v2' -> FloatValue $ v1' * v2'
        IntValue v2' -> FloatValue $ v1' * (int2Float v2')
        _ -> undefined
    _ -> undefined

mulValues :: Value -> Value -> Value
mulValues v1 v2 = case v1 of
    IntValue v1' -> case v2 of
        IntValue v2' -> IntValue $ v1' * v2'
        FloatValue v2' -> FloatValue $ (int2Float v1') * v2'
        _ -> undefined
    FloatValue v1' -> case v2 of
        FloatValue v2' -> FloatValue $ v1' * v2'
        IntValue v2' -> FloatValue $ v1' * (int2Float v2')
        _ -> undefined
    _ -> undefined

subValues :: Value -> Value -> Value
subValues v1 v2 = case v1 of
    IntValue v1' -> case v2 of
        IntValue v2' -> IntValue $ v1' - v2'
        FloatValue v2' -> FloatValue $ (int2Float v1') - v2'
        _ -> undefined
    FloatValue v1' -> case v2 of
        FloatValue v2' -> FloatValue $ v1' - v2'
        IntValue v2' -> FloatValue $ v1' - (int2Float v2')
        _ -> undefined
    _ -> undefined

addValues :: Value -> Value -> Value
addValues v1 v2 = case v1 of
    StringValue v1' -> case v2 of
        StringValue v2' -> StringValue $ v1' ++ v2'
        _ -> undefined
    ArrayValue v1' -> case v2 of
        ArrayValue v2' -> ArrayValue $ v1' ++ v2'
        _ -> undefined
    IntValue v1' -> case v2 of
        IntValue v2' -> IntValue $ v1' + v2'
        FloatValue v2' -> FloatValue $ (int2Float v1') + v2'
        _ -> undefined
    FloatValue v1' -> case v2 of
        FloatValue v2' -> FloatValue $ v1' + v2'
        IntValue v2' -> FloatValue $ v1' + (int2Float v2')
        _ -> undefined
    _ -> undefined

ltEqValues :: Value -> Value -> Bool
ltEqValues v1 v2 = case v1 of
    IntValue v1' -> case v2 of
        IntValue v2' -> v1' <= v2'
        FloatValue v2' -> (int2Float v1') <= v2'
        _ -> undefined
    FloatValue v1' -> case v2 of
        FloatValue v2' -> v1' <= v2'
        IntValue v2' -> v1' <= (int2Float v2')
        _ -> undefined
    _ -> undefined

gtEqValues :: Value -> Value -> Bool
gtEqValues v1 v2 = case v1 of
    IntValue v1' -> case v2 of
        IntValue v2' -> v1' >= v2'
        FloatValue v2' -> (int2Float v1') >= v2'
        _ -> undefined
    FloatValue v1' -> case v2 of
        FloatValue v2' -> v1' >= v2'
        IntValue v2' -> v1' >= (int2Float v2')
        _ -> undefined
    _ -> undefined

ltValues :: Value -> Value -> Bool
ltValues v1 v2 = case v1 of
    IntValue v1' -> case v2 of
        IntValue v2' -> v1' < v2'
        FloatValue v2' -> (int2Float v1') < v2'
        _ -> undefined
    FloatValue v1' -> case v2 of
        FloatValue v2' -> v1' < v2'
        IntValue v2' -> v1' < (int2Float v2')
        _ -> undefined
    _ -> undefined

gtValues :: Value -> Value -> Bool
gtValues v1 v2 = case v1 of
    IntValue v1' -> case v2 of
        IntValue v2' -> v1' > v2'
        FloatValue v2' -> (int2Float v1') > v2'
        _ -> undefined
    FloatValue v1' -> case v2 of
        FloatValue v2' -> v1' > v2'
        IntValue v2' -> v1' > (int2Float v2')
        _ -> undefined
    _ -> undefined

eqValues :: Value -> Value -> Bool
eqValues v1 v2 = case v1 of
    IntValue v1' -> case v2 of
        IntValue v2' -> v1' == v2'
        FloatValue v2' -> (int2Float v1') == v2'
        _ -> undefined
    FloatValue v1' -> case v2 of
        IntValue v2' -> v1' == (int2Float v2')
        FloatValue v2' -> v1' == v2'
        _ -> undefined
    BoolValue v1' -> v1' == (extractBool v2)
    StringValue v1' -> v1' == (extractString v2)
    ArrayValue v1' -> v1' == (extractArray v2)
    NilValue -> undefined
