package analyser

import "core:fmt"

import s "../shared"
import ast "../ast"

@private
Analyser :: struct {
    errors: s.Error_List,
    functions: map[string]^ast.Function,
    builtins: map[string]Builtin_Function,
    tree: ^ast.Ast,
}

analyse :: proc(tree: ^ast.Ast, allocator := context.allocator) -> s.Error_List {
    context.allocator = allocator

    an := Analyser {
        errors = make(s.Error_List),
        functions = make(map[string]^ast.Function, allocator = context.temp_allocator),
        tree = tree,
    }

    // Fill the function table
    collect_functions(&an)

    // Run analysers
    check_main(&an)
    duplicate_variable_check(&an)
    missing_return_check(&an)
    typecheck(&an)
    
    free_all(context.allocator)
    return an.errors
}

@private
check_main :: proc(an: ^Analyser) {
    if !("main" in an.functions) {
        s.append_error(&an.errors, "Missing 'main' function")
        return
    }

    f := an.functions["main"]
    
    if len(f.params) > 0 {
        s.append_error(&an.errors, f.info, "'main' should have no parameters")
    }

    if !ast.is_type_equal(f.return_type, ast.INT_TYPE) {
        s.append_error(&an.errors, f.info, "'main' should have a return type of 'Int'")
    }
}

@private
collect_functions :: proc(an: ^Analyser) {
    get_builtins(&an.builtins)

    for f in an.tree.functions {
        if (f.id in an.functions) || (f.id in an.builtins) {
            s.append_error(&an.errors, f.info, "Function %s is already defined", f.id)
        } else {
            an.functions[f.id] = f
        }
    }
}