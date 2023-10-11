package parser

import "core:fmt"

Ast :: struct {
    functions: [dynamic]^Function_Decl,
}

Type :: union {
    Scalar_Type,
    Array_Type,
}

Scalar_Type :: string
Array_Type :: ^Type

Function_Decl :: struct {
    id: string,
    return_type: Type,
    params: [dynamic]^Function_Parameter,
    block: ^Block,
}

Function_Parameter :: struct {
    id: string,
    type: Type,
}

Block :: struct {
    // Can be null because we might remove some invalid statements before typechecking.
    // Ideally I would recreate the tree but its really annoying to have to rebuild it because of manual allocation.
    stmts: [dynamic]^Statement,
}

Statement :: union {
    ^Variable_Decl_Stmt,
    ^Return_Stmt,
    ^Print_Stmt,
    ^While_Stmt,
    ^If_Stmt,
    ^Assignment_Stmt,
    ^Index_Assignment_Stmt,
}

Variable_Decl_Stmt :: struct {
    id: string,
    type: Type,
    expr: ^Expression,
}

Return_Stmt :: struct {
    expr: ^Expression,
}

Print_Stmt :: struct {
    expr: ^Expression,
}

While_Stmt :: struct {
    cond: ^Expression,
    block: ^Block,
}

If_Stmt :: struct {
    cond: ^Expression,
    block: ^Block,

    elif_stmt: ^If_Stmt, // optional
    else_block: ^Block,  // optional
}

Assignment_Stmt :: struct {
    id: string,
    expr: ^Expression,
}

Index_Assignment_Stmt :: struct {
    id: string,
    index: ^Expression,
    expr: ^Expression,
}

// Make this a union maybe
Expression :: union {
    ^Binary_Expr,
    ^Unary_Expr,
    ^Primary_Expr,
}

// Ordered in ascending precedence
Binary_Operator :: enum {
    Or, // Bool

    And, // Bool

    Eq, // Any
    Neq, // Any

    Gt, // Int, Float
    Lt, // Int, Float
    Gt_Eq, // Int, Float
    Lt_Eq, // Int, Float

    Add, // Int, Float, Array, String
    Sub, // Int, Float

    Mul, // Int, Float
    Div, // Int, Float
    Mod, // Int, Float

    // []
    Index, // Array, String
}

Binary_Expr :: struct  {
    op: Binary_Operator,
    lhs: ^Expression,
    rhs: ^Expression,
}

Unary_Operator :: enum {
    Negation,
    Not,
}

Unary_Expr :: struct {
    op: Unary_Operator,
    expr: ^Expression,
}

Primary_Expr :: union {
    Number,
    string,
    bool,
    Identifier,
    ^Array_Literal,
    ^Expression,
    ^Function_Call,
}

Function_Call :: struct {
    id: string,
    params: [dynamic]^Expression,
}

Array_Literal :: distinct [dynamic]^Expression

Identifier :: distinct string

Number :: union {
    f64,
    i64,
}

is_type_equal :: proc(a: Type, b: Type) -> bool {
    a_id := get_type_id(a)
    b_id := get_type_id(b)
    
    if a_id != b_id do return false
    if a_id == 1 do return a.(Scalar_Type) == b.(Scalar_Type)
        
    return is_type_equal(a.(Array_Type)^, b.(Array_Type)^)
}

is_array_type :: proc(t: Type) -> bool {
    return get_type_id(t) == 2
}

get_array_type_internal :: proc(t: Type) -> Type {
    switch v in t {
        case Scalar_Type: return nil
        case Array_Type: return v^
    }
    
    return nil
}

// Can't think of a better name
@private
get_type_id :: proc(t: Type) -> int {
    switch v in t {
        case Scalar_Type: return 1
        case Array_Type: return 2
    }

    return 0
}

print_ast :: proc(ast: ^Ast) {
    for f in ast.functions {
        print_function_decl(f)
    }
}

@private
print_type :: proc(t: Type) {
    switch v in t {
        case Scalar_Type: fmt.print(v)
        case Array_Type: {
            fmt.print("[")
            
            print_type(v^)

            fmt.print("]")
        }
    }
}

@private
print_function_decl :: proc(fn: ^Function_Decl) {
    fmt.println("<Function_Decl>")
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
            print_type(p.type)
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
        }
    }
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
    print_type(stmt.type)
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

    switch v1 in expr {
        case Number: {
            print_indent(indent + 1)
            switch v2 in v1  {
                case f64: fmt.println("literal(f64):", v2)
                case i64: fmt.println("literal(i64):", v2)
            }           
        }

        case string:  {
            print_indent(indent + 1)
            fmt.println("literal(string): \"", v1, "\"", sep = "") 
        }

        case bool: {
            print_indent(indent + 1)
            fmt.println("literal(bool):", v1) 
        }

        case Identifier: {
            print_indent(indent + 1)
            fmt.println("identifier:", v1)
        }

        case ^Function_Call: {
            print_indent(indent + 1)
            fmt.println("identifier(function):", v1.id)

            print_indent(indent + 1)
            fmt.println("parameters:")
            
            for param in v1.params {
                print_expr(param, indent + 2)
            }
        }
        
        case ^Array_Literal: {
            print_indent(indent + 1)
            fmt.println("literal(array):")

            for e in v1 {
                print_expr(e, indent + 2)
            }
        }

        case ^Expression: {
            print_expr(v1, indent + 1)
        }
    }
}

@private
print_indent :: proc(indent: int) {
    for i in 0..=indent {
        fmt.print("  ")
    }
}