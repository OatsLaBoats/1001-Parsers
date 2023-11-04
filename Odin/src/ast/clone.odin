package ast

// TODO: Clone source info

// You need to swap the arena and pass it in. Otherwise the previus tree will be overwriten
clone :: proc(ast: ^Ast, allocator := context.allocator) -> Ast {
    context.allocator = allocator

    r := Ast {
        _arena1 = ast._arena1,
        _arena2 = ast._arena2,
        _arena_p = ast._arena_p,
        _to_free = ast._to_free,
        functions = make([dynamic]^Function),
    }
    
    for f in ast.functions {
        append(&r.functions, clone_function_decl(f))
    }

    return r
}

clone_function_decl :: proc(f: ^Function, allocator := context.allocator) -> ^Function {
    context.allocator = allocator

    r := new(Function)
    r.id = f.id
    r.return_type = f.return_type
    r.params = make([dynamic]^Function_Parameter)

    for p1 in f.params {
        p2 := new(Function_Parameter)
        p2.id = p1.id
        p2.param_type = p1.param_type
        append(&r.params, p2)
    }
    
    r.block = clone_block(f.block)

    return r
}

clone_block :: proc(b: ^Block, allocator := context.allocator) -> ^Block {
    context.allocator = allocator
    
    r := new(Block)
    r.stmts = make([dynamic]^Statement)

    for s in b.stmts {
        append(&r.stmts, clone_statement(s))
    }
    
    return r
}

clone_statement :: proc(s: ^Statement, allocator := context.allocator) -> ^Statement {
    context.allocator = allocator

    r := new(Statement)
    
    switch v in s {
        case ^Variable_Decl_Stmt: r^ = clone_variable_decl_stmt(v)
        case ^Return_Stmt: r^ = clone_return_stmt(v)
        case ^Print_Stmt: r^ = clone_print_stmt(v)
        case ^While_Stmt: r^ = clone_while_stmt(v)
        case ^If_Stmt: r^ = clone_if_stmt(v)
        case ^Assignment_Stmt: r^ = clone_assignment_stmt(v)
        case ^Index_Assignment_Stmt: r^ = clone_index_assignment_stmt(v)
        case ^Raw_Expr_Stmt: r^ = clone_raw_expr_stmt(v)
    }

    return r
}

clone_raw_expr_stmt :: proc(s: ^Raw_Expr_Stmt, allocator := context.allocator) -> ^Raw_Expr_Stmt {
    context.allocator = allocator
    
    r := new(Raw_Expr_Stmt)
    r.expr = s.expr

    return r
}

clone_index_assignment_stmt :: proc(s: ^Index_Assignment_Stmt, allocator := context.allocator) -> ^Index_Assignment_Stmt {
    context.allocator = allocator
    
    r := new(Index_Assignment_Stmt)
    r.id = s.id
    r.index = clone_expression(s.index)
    r.expr = clone_expression(s.expr)
    
    return r
}

clone_assignment_stmt :: proc(s: ^Assignment_Stmt, allocator := context.allocator) -> ^Assignment_Stmt {
    context.allocator = allocator

    r := new(Assignment_Stmt)
    r.id = s.id
    r.expr = clone_expression(s.expr)
    
    return r
}

clone_if_stmt :: proc(s: ^If_Stmt, allocator := context.allocator) -> ^If_Stmt {
    context.allocator = allocator
    
    r := new(If_Stmt)
    r.cond = clone_expression(s.cond)
    r.block = clone_block(s.block)
    r.elif_stmt = nil if s.elif_stmt == nil else clone_if_stmt(s.elif_stmt)
    r.else_block = nil if s.else_block == nil else clone_block(s.else_block)

    return r
}

clone_while_stmt :: proc(s: ^While_Stmt, allocator := context.allocator) -> ^While_Stmt {
    context.allocator = allocator
    
    r := new(While_Stmt)
    r.cond = clone_expression(s.cond)
    r.block = clone_block(s.block)

    return r
}

clone_print_stmt :: proc(s: ^Print_Stmt, allocator := context.allocator) -> ^Print_Stmt {
    context.allocator = allocator
    
    r := new(Print_Stmt)
    r.expr = clone_expression(s.expr)
    
    return r
}

clone_return_stmt :: proc(s: ^Return_Stmt, allocator := context.allocator) -> ^Return_Stmt {
    context.allocator = allocator

    r := new(Return_Stmt)
    r.expr = clone_expression(s.expr)

    return r
}

clone_variable_decl_stmt :: proc(s: ^Variable_Decl_Stmt, allocator := context.allocator) -> ^Variable_Decl_Stmt {
    context.allocator = allocator
    
    r := new(Variable_Decl_Stmt)
    r.id = s.id
    r.var_type = s.var_type
    r.expr = clone_expression(s.expr)

    return r
}

clone_expression :: proc(e: ^Expression, allocator := context.allocator) -> ^Expression {
    context.allocator = allocator
    
    r := new(Expression)

    switch v in e {
        case ^Binary_Expr: r^ = clone_binary_expr(v)
        case ^Unary_Expr: r^ = clone_unary_expr(v)
        case ^Primary_Expr: r^ = clone_primary_expr(v)
    }

    return r
}

clone_binary_expr :: proc(e: ^Binary_Expr, allocator := context.allocator) -> ^Binary_Expr {
    context.allocator = allocator

    r := new(Binary_Expr)
    r.op = e.op
    r.lhs = clone_expression(e.lhs)
    r.rhs = clone_expression(e.rhs)

    return r
}

clone_unary_expr :: proc(e: ^Unary_Expr, allocator := context.allocator) -> ^Unary_Expr {
    context.allocator = allocator
    
    r := new(Unary_Expr)
    r.op = e.op
    r.expr = clone_expression(e.expr)

    return r
}

clone_primary_expr :: proc(e: ^Primary_Expr, allocator := context.allocator) -> ^Primary_Expr {
    context.allocator = allocator

    r := new(Primary_Expr)

    switch v in e {
        case Int_Lit: r^ = v
        case Float_Lit: r^ = v
        case String_Lit: r^ = v
        case Bool_Lit: r^ = v
        case Identifier: r^ = v
        case ^Expression: r^ = clone_expression(v)

        case ^Array_Lit: {
            lits := new(Array_Lit)

            for lit in v.values {
                append(&lits.values, clone_expression(lit))
            }
            
            r^ = lits
        }

        case ^Function_Call: {
            fcall := new(Function_Call)
            fcall.id = v.id
            
            for param in v.params {
                append(&fcall.params, clone_expression(param))
            }
            
            r^ = fcall
        }
    }

    return r
}