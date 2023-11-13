package analyser

import "../ast"

@private
Builtin_Function :: struct {
    params: [dynamic]ast.Type,
    return_type: ast.Type
}

@private
get_builtins :: proc(table: ^map[string]Builtin_Function) {
    table["test"] = test()
}

@private
test :: proc() -> Builtin_Function {
    context.allocator = context.temp_allocator

    result := Builtin_Function {}
    result.return_type = nil
    append(&result.params, ast.INT_TYPE)

    return result
}