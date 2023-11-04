package interpreter

import "../ast"

@private
eval_function :: proc(env: ^Environment, f: ^ast.Function_Decl, params: Maybe([]Value)) -> Value {
    scope := Scope {}
    defer delete_scope(scope)
    
    if params != nil {
        p := params.([]Value)
        for i in 0..<len(f.params) {
            scope.variables[f.params[i].id] = p[i]
            increment_reference(p[i])
            append_reference(&scope, p[i])
        }
    }

    ret := eval_block(env, &scope, f.block)
    
    drop_references(&scope)

    return ret
}

// Returns non-nil if a return statement was called
@private
eval_block :: proc(env: ^Environment, parent: ^Scope, b: ^ast.Block) -> Value {
    // TODO: Make this so we only create a scope when needed not when we enter a function
    scope := Scope { parent = parent }
    defer delete_scope(scope)
    
    ret: Value = nil

    for s in b.stmts {
        switch v in s {
            case ^ast.Variable_Decl_Stmt: eval_variable_decl_stmt(env, &scope, v)
            case ^ast.Return_Stmt: ret = eval_return_stmt(env, &scope, v)
            case ^ast.Print_Stmt: eval_print_stmt(env, &scope, v)
            case ^ast.While_Stmt: ret = eval_while_stmt(env, &scope, v)
            case ^ast.If_Stmt: ret = eval_if_stmt(env, &scope, v)
            case ^ast.Assignment_Stmt: eval_assignment_stmt(env, &scope, v)
            case ^ast.Index_Assignment_Stmt: eval_index_assignment_stmt(env, &scope, v)
        }

        if ret != nil do break
    }
    
    drop_references(&scope)
    
    return ret
}