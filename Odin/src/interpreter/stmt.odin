package interpreter

import "../ast"

@private
eval_variable_decl_stmt :: proc(env: ^Environment, scope: ^Scope, s: ^ast.Variable_Decl_Stmt) {
    create_variable(scope, s.id, eval_expression(env, scope, s.expr))
}

// NOTE: There is a small bug with when a function returns its reference parameter. It will create two references in the array. 
//       It still gets freed like normal but it takes a bit more memory in the array to store which I think is fine.
@private
eval_return_stmt :: proc(env: ^Environment, scope: ^Scope, s: ^ast.Return_Stmt) -> Value {
    res := eval_expression(env, scope, s.expr)
    increment_reference(res)
    return res
}

@private
eval_print_stmt :: proc(env: ^Environment, scope: ^Scope, s: ^ast.Print_Stmt) {
    print_value(eval_expression(env, scope, s.expr))
}

@private
eval_raw_expr_stmt :: proc(env: ^Environment, scope: ^Scope, s: ^ast.Raw_Expr_Stmt) {
    eval_expression(env, scope, s.expr)
}

@private
eval_while_stmt :: proc(env: ^Environment, scope: ^Scope, s: ^ast.While_Stmt) -> Value {
    condition := eval_expression(env, scope, s.cond).(Bool_Value)
    
    for condition.value {
        ret := eval_block(env, scope, s.block)
        if ret != nil do return ret
            
        condition = eval_expression(env, scope, s.cond).(Bool_Value)
    }
    
    return nil
}

@private
eval_if_stmt :: proc(env: ^Environment, scope: ^Scope, s: ^ast.If_Stmt) -> Value {
    condition := eval_expression(env, scope, s.cond).(Bool_Value)

    if condition.value {
        return eval_block(env, scope, s.block)
    } else if s.elif_stmt != nil {
        return eval_if_stmt(env, scope, s.elif_stmt)
    } else if s.else_block != nil {
        return eval_block(env, scope, s.else_block)
    }
    
    return nil
}

@private
eval_assignment_stmt :: proc(env: ^Environment, scope: ^Scope, s: ^ast.Assignment_Stmt) {
    set_variable(scope, s.id, eval_expression(env, scope, s.expr))
}

@private
eval_index_assignment_stmt :: proc(env: ^Environment, scope: ^Scope, s: ^ast.Index_Assignment_Stmt) {
    index := eval_expression(env, scope, s.index).(Int_Value)
    value := eval_expression(env, scope, s.expr)
    set_array_value(scope, s.id, index.value, value)
}