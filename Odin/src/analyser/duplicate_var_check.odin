package analyser

import "../ast"
import shr "../shared"

@private
duplicate_variable_check :: proc(an: ^Analyser) {
    for f in an.tree.functions {
        dvc_function(an, f)
    }
}

@private
dvc_function :: proc(an: ^Analyser, f: ^ast.Function) {
    table := new_var_table()
    new_parameter_list := make([dynamic]^ast.Function_Parameter)

    for p in f.params {
        if is_var_defined(&table, p.id) {
            shr.append_error(&an.errors, p.info, "Parameter '%s' is already defined", p.id)
            ast.delete_function_parameter(p)
        } else {
            define_var(&table, p.id)
            append(&new_parameter_list, p)
        }
    }
    
    delete(f.params)
    f.params = new_parameter_list
    
    dvc_block(an, &table, f.block)
}

@private
dvc_block :: proc(an: ^Analyser, parent_table: ^Var_Table, b: ^ast.Block) {
    table := new_var_table(parent_table)
    new_stmt_list := make([dynamic]^ast.Statement)

    for s in b.stmts {
        #partial switch v in s {
            case ^ast.Variable_Decl_Stmt: {
                if is_var_defined(&table, v.id) {
                    shr.append_error(&an.errors, v.info, "Variable '%s' is already defined", v.id)
                    ast.delete_statement(s)
                } else {
                    define_var(&table, v.id)
                    append(&new_stmt_list, s)
                }
            }
            
            case ^ast.While_Stmt: { 
                dvc_block(an, &table, v.block)
                append(&new_stmt_list, s)
            }

            case ^ast.If_Stmt: {
                dvc_if_stmt(an, &table, v)
                append(&new_stmt_list, s)
            }
            
            case ^ast.Assignment_Stmt: {
                if !is_var_defined(&table, v.id) {
                    shr.append_error(&an.errors, v.info, "Variable '%s' is undefined", v.id)
                    ast.delete_statement(s)
                } else {
                    append(&new_stmt_list, s)
                }
            }
            
            case ^ast.Index_Assignment_Stmt: {
                if !is_var_defined(&table, v.id) {
                    shr.append_error(&an.errors, v.info, "Variable '%s' is undefined", v.id)
                    ast.delete_statement(s)
                } else {
                    append(&new_stmt_list, s)
                }
            }

            case: append(&new_stmt_list, s)
        }
    }

    delete(b.stmts)
    b.stmts = new_stmt_list
}

@private
dvc_if_stmt :: proc(an: ^Analyser, table: ^Var_Table, s: ^ast.If_Stmt) {
    dvc_block(an, table, s.block)
    if s.elif_stmt != nil do dvc_if_stmt(an, table, s.elif_stmt)
    if s.else_block != nil do dvc_block(an, table, s.else_block)
}