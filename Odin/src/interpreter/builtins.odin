package interpreter

import "core:fmt"
import "../ast"

@private
Builtin_Function :: struct {
    callback: Builtin_Callback,
}

@private
Builtin_Callback :: proc(env: ^Environment, params: Maybe([]Value)) -> Value

@private
get_builtins :: proc(table: ^map[string]Builtin_Function) {
    table["test"] = { test }
}

@private
test :: proc(env: ^Environment, params: Maybe([]Value)) -> Value {
    v := params.([]Value)
    fmt.println("Jello", v[0].(Int_Value).value)

    return nil
}