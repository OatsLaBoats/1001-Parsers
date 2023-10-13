package analyser

import ast "../ast"


Error_List :: [dynamic]Error

Error :: struct {
    msg: string,
    id: string,
}

@private
Analyser :: struct {
    errors: Error_List,
    functions: map[string]^ast.Function_Decl,
    tree: ^ast.Ast,
}

analyse :: proc(tree: ^ast.Ast) -> Error_List {
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
            append(&an.errors, Error { "Duplicate function", f.id })
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
                append(&an.errors, Error { "Duplicate variable", p.id })
            } else {
                var_table[p.id] = true
            }
        }

        for s, i in f.block.stmts {
            #partial switch v in s {
                case ^ast.Variable_Decl_Stmt: {
                    if v.id in var_table {
                        f.block.stmts[i] = nil
                        append(&an.errors, Error { "Duplicate variable", v.id })
                    } else {
                        var_table[v.id] = true
                    }
                }

                case ^ast.Assignment_Stmt: {
                    if !(v.id in var_table) {
                        f.block.stmts[i] = nil
                        append(&an.errors, Error { "Variable not found", v.id })
                    }
                }
                
                case ^ast.Index_Assignment_Stmt: {
                    if !(v.id in var_table) {
                        f.block.stmts[i] = nil
                        append(&an.errors, Error { "Variable not found", v.id })
                    }
                }
            }
        }
    }
}