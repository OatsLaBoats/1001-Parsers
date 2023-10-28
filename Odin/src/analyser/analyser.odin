package analyser

import "core:strings"
import "core:fmt"

import "../shared"
import ast "../ast"

// TODO: Create delete function for Error_List
// TODO: Ensure mains exists and has the correct type

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
    functions: map[string]^ast.Function_Decl,
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

    an := Analyser {}
    an.tree = tree
    defer delete(an.functions)

    duplicate_function_check(&an)
    duplicate_variable_check(&an)
    typecheck(&an)
    
    return an.errors
}

@private
duplicate_function_check :: proc(an: ^Analyser) {
    for f in an.tree.functions {
        if f.id in an.functions {
            append(&an.errors, make_error(f.info, "Function '%s' is already defined", f.id))
        } else {
            an.functions[f.id] = f
        }
    }
}

@private
duplicate_variable_check :: proc(an: ^Analyser) {
    for f in an.tree.functions {
        var_table := make(map[string]bool)
        defer delete(var_table)

        for p in f.params {
            if p.id in var_table {
                append(&an.errors, make_error(p.info, "Parameter '%s' is already defined", p.id))
            } else {
                var_table[p.id] = true
            }
        }

        for s, i in f.block.stmts {
            #partial switch v in s {
                case ^ast.Variable_Decl_Stmt: {
                    if v.id in var_table {
                        f.block.stmts[i] = nil
                        append(&an.errors, make_error(v.info, "Variable '%s' is already defined", v.id))
                    } else {
                        var_table[v.id] = true
                    }
                }

                case ^ast.Assignment_Stmt: {
                    if !(v.id in var_table) {
                        f.block.stmts[i] = nil
                        append(&an.errors, make_error(v.info, "Variable '%s' is not defined", v.id))
                    }
                }
                
                case ^ast.Index_Assignment_Stmt: {
                    if !(v.id in var_table) {
                        f.block.stmts[i] = nil
                        append(&an.errors, make_error(v.info, "Variable '%s' is not defined", v.id))
                    }
                }
            }
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