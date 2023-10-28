package parser

import "core:fmt"
import "core:os"

import "../ast"

// TODO: Fix bug where raw expression are illegal
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
    id := advance(parser)

    expect(parser, .L_Bracket, "Expected '[' after identifier")
    idx := parse_expression(parser)
    expect(parser, .R_Bracket, "Expected ']' after index expression")

    expect(parser, .Equal, "Expected '=' operator after index operation")
    expr := parse_expression(parser)
    
    iastmt := new(ast.Index_Assignment_Stmt)
    iastmt.info = { id.line, id.column }
    iastmt.id = id.lexeme
    iastmt.index = idx
    iastmt.expr = expr
    
    return iastmt
}

@private
parse_assignment_stmt :: proc(parser: ^Parser) -> ^ast.Assignment_Stmt {
    id := advance(parser)
    expect(parser, .Equal, "Expected '=' operator after variable name")
    expr := parse_expression(parser)
    
    astmt := new(ast.Assignment_Stmt)
    astmt.info = { id.line, id.column }
    astmt.id = id.lexeme
    astmt.expr = expr
    
    return astmt
}

@private
parse_if_stmt :: proc(parser: ^Parser) -> ^ast.If_Stmt {
    info := advance(parser) // skip if and elif
    cond := parse_expression(parser)
    block := parse_block(parser)

    istmt := new(ast.If_Stmt)
    istmt.info = { info.line, info.column }
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
    info := expect(parser, .While, "Expected 'while'")
    cond := parse_expression(parser)
    block := parse_block(parser)
    
    wstmt := new(ast.While_Stmt)
    wstmt.info = { info.line, info.column }
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
    info := expect(parser, .Return, "Expected 'return'")
    expr := parse_expression(parser)

    rstmt := new(ast.Return_Stmt)
    rstmt.info = { info.line, info.column }
    rstmt.expr = expr
    
    return rstmt
}

@private
parse_var_decl_stmt :: proc(parser: ^Parser) -> ^ast.Variable_Decl_Stmt {
    expect(parser, .Var, "Expected 'var'")

    id := expect(parser, .Identifier, "Expected variable name.")
    expect(parser, .Colon, "Expected ':' after variable name.")
    var_type := parse_type_annotation(parser)
    expect(parser, .Equal, "Expected '=' after type annotation.")

    expr := parse_expression(parser)
    
    result := new(ast.Variable_Decl_Stmt)
    result.info = { id.line, id.column }
    result.id = id.lexeme
    result.var_type = var_type
    result.expr = expr

    return result
}