package parser

import "core:fmt"
import "core:os"

import "../ast"

@private
parse_statement :: proc(parser: ^Parser) -> ^ast.Statement {
    result := new(ast.Statement)

    if match(parser, .Var) {
        result^ = parse_var_decl_stmt(parser)
    } else if match(parser, .Return) {
        result^ = parse_return_stmt(parser)
    } else if match(parser, .Print) {
        result^ = parse_print_stmt(parser)
    } else if match(parser, .While) {
        result^ = parse_while_stmt(parser)
    } else if match(parser, .If) {
        result^ = parse_if_stmt(parser)
    } else if match(parser, .Identifier) {
        if peek(parser, 1).kind != .L_Bracket {
            result^ = parse_assignment_stmt(parser)
        } else {
            result^ = parse_index_assignment_stmt(parser)
        }
    } else {
        fmt.println("Invalid statement", peek(parser))
        os.exit(-1)
    }

    return result
}

@private
parse_index_assignment_stmt :: proc(parser: ^Parser) -> ^ast.Index_Assignment_Stmt {
    id := peek(parser).lexeme
    idx := parse_index_expr(parser)
    expect(parser, .Equal, "Expected '=' operator after index operation")
    expr := parse_expression(parser)
    
    iastmt := new(ast.Index_Assignment_Stmt)
    iastmt.id = id
    iastmt.index = idx
    iastmt.expr = expr
    
    return iastmt
}

@private
parse_assignment_stmt :: proc(parser: ^Parser) -> ^ast.Assignment_Stmt {
    id := advance(parser).lexeme
    expect(parser, .Equal, "Expected '=' operator after variable name")
    expr := parse_expression(parser)
    
    astmt := new(ast.Assignment_Stmt)
    astmt.id = id
    astmt.expr = expr
    
    return astmt
}

@private
parse_if_stmt :: proc(parser: ^Parser) -> ^ast.If_Stmt {
    advance(parser) // skip if and elif
    cond := parse_expression(parser)
    block := parse_block(parser)

    istmt := new(ast.If_Stmt)
    istmt.cond = cond
    istmt.block = block
    istmt.elif_stmt = nil
    istmt.else_block = nil

    skip_while(parser, .Line_End)
        
    if match(parser, .Elif) {
        istmt.elif_stmt = parse_if_stmt(parser)
    } else if match(parser, .Else) {
        advance(parser) // consume else
        istmt.else_block = parse_block(parser)
    }
    
    return istmt
}

@private
parse_while_stmt :: proc(parser: ^Parser) -> ^ast.While_Stmt {
    expect(parser, .While, "Expected 'while'")
    cond := parse_expression(parser)
    block := parse_block(parser)
    
    wstmt := new(ast.While_Stmt)
    wstmt.cond = cond
    wstmt.block = block
    
    return wstmt
}

@private
parse_print_stmt :: proc(parser: ^Parser) -> ^ast.Print_Stmt {
    expect(parser, .Print, "Expected 'print'")
    expr := parse_expression(parser)

    rstmt := new(ast.Print_Stmt)
    rstmt.expr = expr
    
    return rstmt
}

@private
parse_return_stmt :: proc(parser: ^Parser) -> ^ast.Return_Stmt {
    expect(parser, .Return, "Expected 'return'")
    expr := parse_expression(parser)

    rstmt := new(ast.Return_Stmt)
    rstmt.expr = expr
    
    return rstmt
}

@private
parse_var_decl_stmt :: proc(parser: ^Parser) -> ^ast.Variable_Decl_Stmt {
    expect(parser, .Var, "Expected 'var'")

    id := expect(parser, .Identifier, "Expected variable name.").lexeme
    expect(parser, .Colon, "Expected ':' after variable name.")
    var_type := parse_type_annotation(parser)
    expect(parser, .Equal, "Expected '=' after type annotation.")

    expr := parse_expression(parser)
    
    result := new(ast.Variable_Decl_Stmt)
    result.id = id
    result.var_type = var_type
    result.expr = expr

    return result
}