package ast

import "core:fmt"

print :: proc(ast: ^Ast) {
    for f in ast.functions {
        print_function_decl(f)
    }
}

@private
print_type :: proc(t: Type) {
    switch v in t {
        case Base_Type: fmt.print(v.id)
        case Array_Type: {
            for i in 0..<v.nesting {
                fmt.print("[")
            }
            
            fmt.print(v.internal.id)

            for i in 0..<v.nesting {
                fmt.print("]")
            }
        }
    }
}

@private
print_function_decl :: proc(fn: ^Function) {
    fmt.println("<Function>")
    print_indent(0)
    fmt.println("name:", fn.id)

    if fn.return_type != nil {
        print_indent(0)
        fmt.print("returns: ")
        print_type(fn.return_type)
        fmt.println()
    }

    if len(fn.params) > 0 {
        print_indent(0)
        fmt.print("parameters: [", sep="")

        for p in fn.params {
            fmt.print("(", p.id, ": ", sep="")
            print_type(p.param_type)
            fmt.print(")")
        }

        fmt.println("]")
    }

    print_block(fn.block, 0)
}

@private
print_block :: proc(block: ^Block, indent: int) {
    print_indent(indent)
    fmt.println("<Block>")

    for s in block.stmts {
        switch v in s {
            case ^Variable_Decl_Stmt: print_variable_decl_stmt(v, indent + 1)
            case ^Return_Stmt: print_return_stmt(v, indent + 1)
            case ^Print_Stmt: print_print_stmt(v, indent + 1)
            case ^While_Stmt: print_while_stmt(v, indent + 1)
            case ^If_Stmt: print_if_stmt(v, indent + 1)
            case ^Assignment_Stmt: print_assignment_stmt(v, indent + 1)
            case ^Index_Assignment_Stmt: print_index_assignment_stmt(v, indent + 1)
            case ^Raw_Expr_Stmt: print_raw_expr_stmt(v, indent + 1)
        }
    }
}

@private
print_raw_expr_stmt :: proc(stmt: ^Raw_Expr_Stmt, indent: int) {
    print_indent(indent)
    fmt.println("<Raw_Expr_Stmt>")
    
    print_indent(indent + 1)
    fmt.println("expr:")
    print_expr(stmt.expr, indent + 2)
}

@private
print_index_assignment_stmt :: proc(stmt: ^Index_Assignment_Stmt, indent: int) {
    print_indent(indent)
    fmt.println("<Index_Assignment_Stmt>")
    
    print_indent(indent + 1)
    fmt.println("array:")
    print_expr(stmt.index, indent + 2)
    
    print_indent(indent + 1)
    fmt.println("expr:")
    print_expr(stmt.expr, indent + 2)
}

@private
print_assignment_stmt :: proc(stmt: ^Assignment_Stmt, indent: int) {
    print_indent(indent)
    fmt.println("<Assignment_Stmt>")
    
    print_indent(indent + 1)
    fmt.println("id:", stmt.id)
    
    print_indent(indent + 1)
    fmt.println("expr:")
    
    print_expr(stmt.expr, indent + 2)
}

@private
print_if_stmt :: proc(stmt: ^If_Stmt, indent: int) {
    print_indent(indent)
    fmt.println("<If_Stmt>")
    
    print_indent(indent + 1)
    fmt.println("condition:")
    print_expr(stmt.cond, indent + 2)
    
    print_block(stmt.block, indent + 1)

    if stmt.else_block != nil {
        print_indent(indent)
        fmt.println("<Else_Stmt>")
        print_block(stmt.else_block, indent + 1)
    }

    elif_stmt := stmt.elif_stmt
    for elif_stmt != nil {
        print_indent(indent)
        fmt.println("<Elif_Stmt>")
        
        print_indent(indent + 1)
        fmt.println("condition:")
        print_expr(elif_stmt.cond, indent + 2)
        
        print_block(elif_stmt.block, indent + 1)
        
        if elif_stmt.else_block != nil {
            print_indent(indent)
            fmt.println("<Else_Stmt>")
            print_block(elif_stmt.else_block, indent + 1)
        }
        
        elif_stmt = elif_stmt.elif_stmt
    }
}

@private
print_while_stmt :: proc(stmt: ^While_Stmt, indent: int) {
    print_indent(indent)
    fmt.println("<While_Stmt>")
    
    print_indent(indent + 1)
    fmt.println("condition:")
    print_expr(stmt.cond, indent + 2)

    print_block(stmt.block, indent + 1)
}

@private
print_print_stmt :: proc(stmt: ^Print_Stmt, indent: int) {
    print_indent(indent)
    fmt.println("<Print_Stmt>")
    
    print_indent(indent + 1)
    fmt.println("expr:")
    
    print_expr(stmt.expr, indent + 2)
}

@private
print_return_stmt :: proc(stmt: ^Return_Stmt, indent: int) {
    print_indent(indent)
    fmt.println("<Return_Stmt>")
    
    print_indent(indent + 1)
    fmt.println("expr:")
    
    print_expr(stmt.expr, indent + 2)
}

@private
print_variable_decl_stmt :: proc(stmt: ^Variable_Decl_Stmt, indent: int) {
    print_indent(indent)
    fmt.println("<Variable_Decl_Stmt>")

    print_indent(indent + 1)
    fmt.println("name:", stmt.id)

    print_indent(indent + 1)
    fmt.print("type: ")
    print_type(stmt.var_type)
    fmt.println()
    
    print_indent(indent + 1)
    fmt.println("value:")

    print_expr(stmt.expr, indent + 2)
}

@private
print_expr :: proc(expr: ^Expression, indent: int) {
    switch v in expr {
        case ^Primary_Expr: print_primary_expr(v, indent)
        case ^Binary_Expr: print_binary_expr(v, indent)
        case ^Unary_Expr: print_unary_expr(v, indent)
    }
}

@private
print_unary_expr :: proc(expr: ^Unary_Expr, indent: int) {
    print_indent(indent)
    fmt.println("<Unary_Expr>")

    print_indent(indent + 1)
    fmt.println("operator:", expr.op)
    
    print_indent(indent + 1)
    fmt.println("expr:")
    print_expr(expr.expr, indent + 2)
}

@private
print_binary_expr :: proc(expr: ^Binary_Expr, indent: int) {
    print_indent(indent)
    fmt.println("<Binary_Expr>")

    print_indent(indent + 1)
    fmt.println("operator:", expr.op)

    print_indent(indent + 1)
    fmt.println("lhs:")
    print_expr(expr.lhs, indent + 2)

    print_indent(indent + 1)
    fmt.println("rhs:")
    print_expr(expr.rhs, indent + 2)
}

@private
print_primary_expr :: proc(expr: ^Primary_Expr, indent: int) {
    print_indent(indent)
    fmt.println("<Primary_Expr>")

    switch v in expr {
        case Int_Lit: {
            print_indent(indent + 1)
            fmt.println("literal(Int):", v.value)
        }

        case Float_Lit: {
            print_indent(indent + 1)
            fmt.println("literal(Float):", v.value)
        }

        case String_Lit:  {
            print_indent(indent + 1)
            fmt.println("literal(String): \"", v.value, "\"", sep = "") 
        }

        case Bool_Lit: {
            print_indent(indent + 1)
            fmt.println("literal(Bool):", v.value) 
        }

        case Identifier: {
            print_indent(indent + 1)
            fmt.println("identifier(access):", v.value)
        }

        case ^Function_Call: {
            print_indent(indent + 1)
            fmt.println("identifier(call):", v.id)

            print_indent(indent + 1)
            fmt.println("parameters:")
            
            for param in v.params {
                print_expr(param, indent + 2)
            }
        }
        
        case ^Array_Lit: {
            print_indent(indent + 1)
            fmt.println("literal(Array):")

            for e in v.values {
                print_expr(e, indent + 2)
            }
        }

        case ^Expression: {
            print_expr(v, indent + 1)
        }
    }
}

@private
print_indent :: proc(indent: int) {
    for i in 0..=indent {
        fmt.print("  ")
    }
}