// TODO: Break this into multiple files
package interpreter

import "core:strings"
import "core:fmt"

import "../ast"

Error_Code :: i64

@private
Environment :: struct {
    functions: map[string]^ast.Function_Decl
}

eval :: proc(tree: ^ast.Ast) -> Error_Code {
    env := Environment {}
    env.functions = make(map[string]^ast.Function_Decl, allocator = context.temp_allocator)
    defer free_all(context.temp_allocator)
    
    for f in tree.functions {
        env.functions[f.id] = f
    }
    
    entry_point := env.functions["main"]
    ret := eval_function(&env, entry_point, nil).(Int_Value)

    return ret.value
}