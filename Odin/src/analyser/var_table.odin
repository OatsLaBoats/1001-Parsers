package analyser

import "../ast"

@private
Var_Table :: struct {
    vars: map[string]ast.Type,
    parent: ^Var_Table,
}

@private
is_var_defined :: proc(table: ^Var_Table, id: string) -> bool {
    if id in table.vars do return true
    if table.parent == nil do return false
    return is_var_defined(table.parent, id)
}

@private
define_var :: proc(table: ^Var_Table, id: string, type: ast.Type = nil) {
    table.vars[id] = type
}

@private
get_var :: proc(table: ^Var_Table, id: string) -> ast.Type {
    if id in table.vars {
        return table.vars[id]
    }
    
    if table.parent == nil do return nil
    return get_var(table.parent, id)
}

@private
new_var_table :: proc(parent: ^Var_Table = nil) -> Var_Table {
    return Var_Table {
        vars = make(map[string]ast.Type, allocator = context.temp_allocator),
        parent = parent,
    }
}