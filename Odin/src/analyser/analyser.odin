package analyser

import "core:strings"
import "core:fmt"

import "../shared"
import ast "../ast"

Error_List :: [dynamic]Error

// NOTE: In a real compiler this would probably include two error messages. A simple one and a more detailed one with source snipets.
//       In my case I don't want to invest that much time in a test compiler basic error messages are fine.
Error :: struct {
    info: shared.Source_Info,
    msg: strings.Builder,
}

@private
Analyser :: struct {
    errors: Error_List,
    functions: map[string]^ast.Function,
    builtins: map[string]Builtin_Function,
    tree: ^ast.Ast,
}

delete_error_list :: proc(list: Error_List) {
    for e in list {
        delete_error(e)
    }
    
    delete(list)
}

analyse :: proc(tree: ^ast.Ast, allocator := context.allocator) -> Error_List {
    context.allocator = allocator

    an := Analyser {
        errors = make(Error_List),
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
        append(&an.errors, make_error({-1, -1}, "Missing 'main' function"))
        return
    }

    f := an.functions["main"]
    
    if len(f.params) > 0 {
        append(&an.errors, make_error(f.info, "'main' should have no parameters"))
    }

    if !ast.is_type_equal(f.return_type, ast.INT_TYPE) {
        append(&an.errors, make_error(f.info, "'main' should have return type 'Int'"))
    }
}

@private
collect_functions :: proc(an: ^Analyser) {
    get_builtins(&an.builtins)

    for f in an.tree.functions {
        if (f.id in an.functions) || (f.id in an.builtins) {
            append(&an.errors, make_error(f.info, "Function '%s' is already defined", f.id))
        } else {
            an.functions[f.id] = f
        }
    }
}

@private 
make_error :: proc(info: shared.Source_Info, format: string, args: ..any) -> Error {
    result := Error { info = info }
    strings.builder_init(&result.msg)
    fmt.sbprintf(&result.msg, format, ..args)
    return result
}

@private
delete_error :: proc(error: Error) {
    e := error
    strings.builder_destroy(&e.msg)
}