package interpreter

import "core:strings"
import "core:fmt"

import "../ast"

Error_Code :: i64

@private
Environment :: struct {
    functions: map[string]^ast.Function,
    builtins: map[string]Builtin_Function
}

@private
setup_environment :: proc(tree: ^ast.Ast) -> Environment {
    context.allocator = context.temp_allocator

    env := Environment {
        functions = make(map[string]^ast.Function),
        builtins = make(map[string]Builtin_Function),
    }
    
    for f in tree.functions {
        env.functions[f.id] = f
    }

    get_builtins(&env.builtins)
    
    return env
}

eval :: proc(tree: ^ast.Ast) -> Error_Code {
    env := setup_environment(tree)
    defer free_all(context.temp_allocator)
    
    ret := call(&env, "main", nil).(Int_Value)
    return ret.value
}

@private
call :: proc(env: ^Environment, id: string, params: Maybe([]Value)) -> Value {
    if id in env.functions do return eval_function(env, env.functions[id], params)
    if id in env.builtins do return env.builtins[id].callback(env, params)
    return nil
}