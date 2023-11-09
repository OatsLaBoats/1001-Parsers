package analyser

import "../ast"

@private
Var_Table :: struct {
    vars: map[string]bool,
    parent: ^Var_Table,
}

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
            append(&an.errors, make_error(p.info, "Parameter '%s' is already defined", p.id))
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

// TODO: Also handle undefined variable for assignment statements
@private
dvc_block :: proc(an: ^Analyser, parent_table: ^Var_Table, b: ^ast.Block) {
    table := new_var_table(parent_table)
    new_stmt_list := make([dynamic]^ast.Statement)

    for s in b.stmts {
        #partial switch v in s {
            case ^ast.Variable_Decl_Stmt: {
                if is_var_defined(&table, v.id) {
                    append(&an.errors, make_error(v.info, "Variable '%s' is already defined", v.id))
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

@private
is_var_defined :: proc(table: ^Var_Table, id: string) -> bool {
    if id in table.vars do return true
    if table.parent == nil do return false
    return is_var_defined(table.parent, id)
}

@private
define_var :: proc(table: ^Var_Table, id: string) {
    table.vars[id] = true
}

@private
new_var_table :: proc(parent: ^Var_Table = nil) -> Var_Table {
    return Var_Table {
        vars = make(map[string]bool, allocator = context.temp_allocator),
        parent = parent,
    }
}