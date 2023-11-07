package ast

delete_ast :: proc(ast: ^Ast, allocator := context.allocator) {
    context.allocator = allocator
    for f in ast.functions do delete_function(f)
    delete(ast.functions)
}

delete_function :: proc(f: ^Function, allocator := context.allocator) {
    context.allocator = allocator
    for p in f.params do delete_function_parameter(p)
    delete(f.params)
    delete_block(f.block)
    free(f)
}

delete_function_parameter :: proc(p: ^Function_Parameter, allocator := context.allocator) {
    context.allocator = allocator
    free(p)
}

delete_block :: proc(b: ^Block, allocator := context.allocator) {
    context.allocator = allocator
    for s in b.stmts do delete_statement(s)
    free(b)
}


delete_statement :: proc(s: ^Statement, allocator := context.allocator) {
    context.allocator = allocator
    
    switch stmt in s {
        case ^Variable_Decl_Stmt: delete_varibale_decl_stmt(stmt)
        case ^Return_Stmt: delete_return_stmt(stmt)
        case ^Print_Stmt: delete_print_stmt(stmt)
        case ^While_Stmt: delete_while_stmt(stmt)
        case ^If_Stmt: delete_if_stmt(stmt)
        case ^Assignment_Stmt: delete_assignment_stmt(stmt)
        case ^Index_Assignment_Stmt: delete_index_assignment_stmt(stmt)
        case ^Raw_Expr_Stmt: delete_raw_expr_stmt(stmt)
    }

    free(s)
}

delete_varibale_decl_stmt :: proc(s: ^Variable_Decl_Stmt, allocator := context.allocator) {
    context.allocator = allocator
    delete_expression(s.expr)
    free(s)
}

delete_return_stmt :: proc(s: ^Return_Stmt, allocator := context.allocator) {
    context.allocator = allocator
    delete_expression(s.expr)
    free(s)
}

delete_print_stmt :: proc(s: ^Print_Stmt, allocator := context.allocator) {
    context.allocator = allocator
    delete_expression(s.expr)
    free(s)
}

delete_while_stmt :: proc(s: ^While_Stmt, allocator := context.allocator) {
    context.allocator = allocator
    delete_expression(s.cond)
    delete_block(s.block)
    free(s)
}

delete_if_stmt :: proc(s: ^If_Stmt, allocator := context.allocator) {
    context.allocator = allocator
    delete_expression(s.cond)
    delete_block(s.block)
    if s.elif_stmt != nil do delete_if_stmt(s.elif_stmt)
    if s.else_block != nil do delete_block(s.else_block)
    free(s)
}

delete_assignment_stmt :: proc(s: ^Assignment_Stmt, allocator := context.allocator) {
    context.allocator = allocator
    delete_expression(s.expr)
    free(s)
}

delete_index_assignment_stmt :: proc(s: ^Index_Assignment_Stmt, allocator := context.allocator) {
    context.allocator = allocator
    delete_expression(s.index)
    delete_expression(s.expr)
    free(s)
}

delete_raw_expr_stmt :: proc(s: ^Raw_Expr_Stmt, allocator := context.allocator) {
    context.allocator = allocator
    delete_expression(s.expr)
    free(s)
}

delete_expression :: proc(e: ^Expression, allocator := context.allocator) {
    context.allocator = allocator
    
    switch expr in e {
        case ^Binary_Expr: delete_binary_expr(expr)
        case ^Unary_Expr: delete_unary_expr(expr)
        case ^Primary_Expr: delete_primary_expr(expr)
    }

    free(e)
}

delete_binary_expr :: proc(e: ^Binary_Expr, allocator := context.allocator) {
    context.allocator = allocator
    delete_expression(e.lhs)
    delete_expression(e.rhs)
    free(e)
}

delete_unary_expr :: proc(e: ^Unary_Expr, allocator := context.allocator) {
    context.allocator = allocator
    delete_expression(e.expr)
    free(e)
}

delete_primary_expr :: proc(e: ^Primary_Expr, allocator := context.allocator) {
    context.allocator = allocator
    
    #partial switch v in e {
        case ^Array_Lit: {
            for expr in v.values do delete_expression(expr)
            delete(v.values)
            free(v)
        }

        case ^Expression: delete_expression(v)
        
        case ^Function_Call: {
            for expr in v.params do delete_expression(expr)
            delete(v.params)
            free(v)
        }
    }

    free(e)
}