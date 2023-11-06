package interpreter

import "../ast"

@private
eval_function :: proc(env: ^Environment, f: ^ast.Function, params: Maybe([]Value)) -> Value {
    scope := new(Scope)
    defer delete_scope(scope)
    
    if params != nil {
        p := params.([]Value)
        for i in 0..<len(f.params) {
            scope.variables[f.params[i].id] = p[i]
            increment_reference(p[i])
            append_reference(scope, p[i])
        }
    }

    ret := eval_block(env, nil, f.block, scope)
    
    drop_references(scope)

    return ret
}

// Returns non-nil if a return statement was called
@private
eval_block :: proc(env: ^Environment, parent: ^Scope, b: ^ast.Block, block_scope: ^Scope = nil) -> Value {
    scope := block_scope
    if scope == nil {
        scope = new(Scope)
    }
    scope.parent = parent
    defer if block_scope == nil do delete_scope(scope)
    
    ret: Value = nil

    for s in b.stmts {
        switch v in s {
            case ^ast.Variable_Decl_Stmt: eval_variable_decl_stmt(env, scope, v)
            case ^ast.Return_Stmt: ret = eval_return_stmt(env, scope, v)
            case ^ast.Print_Stmt: eval_print_stmt(env, scope, v)
            case ^ast.While_Stmt: ret = eval_while_stmt(env, scope, v)
            case ^ast.If_Stmt: ret = eval_if_stmt(env, scope, v)
            case ^ast.Assignment_Stmt: eval_assignment_stmt(env, scope, v)
            case ^ast.Index_Assignment_Stmt: eval_index_assignment_stmt(env, scope, v)
            case ^ast.Raw_Expr_Stmt: eval_raw_expr_stmt(env, scope, v)
        }

        if ret != nil do break
    }
    
    if block_scope == nil do drop_references(scope)
    
    return ret
}