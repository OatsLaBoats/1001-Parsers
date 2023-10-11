// Error recovery is very hard not sure what can be recovered and what can't
// You probaly want to abort ship if a synatax error occurs. With the exception of some common cases.
// The rest can be mostly recovered
// Semantic errors are usually recoverable. Syntactic errors vary, some can be rcovered and others can not in most cases.
// Skip error handling for later
// Found a video that shows some error recovery techniques
//
// Dont over do it, Don't do things you dont have to. Do 1.5 hour a day at most
// 
// Deal with out of bound access in scanning functions
// 
// I am stuck at how to parse expressions and statements. I need to see how this is done elsewhere before proceeding
// https://github.com/munificent/craftinginterpreters/tree/master/java/com/craftinginterpreters/lox

// TODO: Store line information in the ast

package parser

import "core:fmt"
import "core:os"
import "core:strconv"
import "core:strings"

import lx "../lexer"

@private
Parser :: struct {
    tokens: []lx.Token,
    pos: int,
}

parse :: proc(tokens: []lx.Token) -> Ast {
    parser := Parser {
        tokens = tokens,
    }

    ast := Ast {}

    for {
        // Skips top level new lines
        skip_while(&parser, .Line_End)

        if is_at_end(&parser) { break }

        f := parse_function_decl(&parser)
        append(&ast.functions, f) 
    }

    return ast
}

@private 
parse_function_decl :: proc(parser: ^Parser) -> ^Function_Decl {
    expect(parser, .Fun, "Expected 'fun' keyword.")
    id := expect(parser, .Identifier, "Expected identifier after 'fun' keyword.")

    params := parse_function_paramters(parser)

    return_type: Type = nil
    if match (parser, .Colon) {
        advance(parser)
        return_type = parse_type_annotation(parser)
    }

    block := parse_block(parser)
    
    result := new(Function_Decl)
    result.id = id.lexeme
    result.return_type = return_type
    result.params = params
    result.block = block

    return result
}

@private
parse_type_annotation :: proc(parser: ^Parser) -> Type {
    if match(parser, .L_Bracket) {
        advance(parser)

        arr_type := new(Type)
        t := parse_type_annotation(parser)
        arr_type^ = t
        
        expect(parser, .R_Bracket, "Expected closing ']' for array type annotation")

        return arr_type
    }
    else {
        return expect(parser, .Identifier, "Expected type annotation").lexeme
    }
}

@private
parse_function_paramters :: proc(parser: ^Parser) -> [dynamic]^Function_Parameter {
    expect(parser, .L_Paren, "Expected '(' after function name.")

    result: [dynamic]^Function_Parameter

    for {
        if match(parser, .R_Paren) { break }

        param := parse_parameter(parser)
        append(&result, param)

        if match(parser, .Comma) { advance(parser) }
    }

    expect(parser, .R_Paren, "Expected ')' after parameters.")

    return result
}

@private
parse_parameter :: proc(parser: ^Parser) -> ^Function_Parameter {
    id := expect(parser, .Identifier, "Expected parameter name after '('.").lexeme
    expect(parser, .Colon, "Expected ':' after parameter name.")
    type := parse_type_annotation(parser)

    result := new(Function_Parameter)
    result.id = id
    result.type = type

    return result
}

@private
parse_block :: proc(parser: ^Parser) -> ^Block {
    expect(parser, .L_Brace, "Missing '{'")

    stmts: [dynamic]^Statement

    for !is_at_end(parser) {
        skip_while(parser, .Line_End)

        if match(parser, .R_Brace) { break }

        stmt := parse_statement(parser)
        append(&stmts, stmt)
    }
    
    skip_until(parser, .R_Brace)
    expect(parser, .R_Brace, "Missing '}'")

    result := new(Block)
    result.stmts = stmts

    return result
}

@private
parse_statement :: proc(parser: ^Parser) -> ^Statement {
    result := new(Statement)

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
parse_index_assignment_stmt :: proc(parser: ^Parser) -> ^Index_Assignment_Stmt {
    id := peek(parser).lexeme
    idx := parse_index_expr(parser)
    expect(parser, .Equal, "Expected '=' operator after index operation")
    expr := parse_expression(parser)
    
    iastmt := new(Index_Assignment_Stmt)
    iastmt.id = id
    iastmt.index = idx
    iastmt.expr = expr
    
    return iastmt
}

@private
parse_assignment_stmt :: proc(parser: ^Parser) -> ^Assignment_Stmt {
    id := advance(parser).lexeme
    expect(parser, .Equal, "Expected '=' operator after variable name")
    expr := parse_expression(parser)
    
    astmt := new(Assignment_Stmt)
    astmt.id = id
    astmt.expr = expr
    
    return astmt
}

@private
parse_if_stmt :: proc(parser: ^Parser) -> ^If_Stmt {
    advance(parser) // skip if and elif
    cond := parse_expression(parser)
    block := parse_block(parser)

    istmt := new(If_Stmt)
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
parse_while_stmt :: proc(parser: ^Parser) -> ^While_Stmt {
    expect(parser, .While, "Expected 'while'")
    cond := parse_expression(parser)
    block := parse_block(parser)
    
    wstmt := new(While_Stmt)
    wstmt.cond = cond
    wstmt.block = block
    
    return wstmt
}

@private
parse_print_stmt :: proc(parser: ^Parser) -> ^Print_Stmt {
    expect(parser, .Print, "Expected 'print'")
    expr := parse_expression(parser)

    rstmt := new(Print_Stmt)
    rstmt.expr = expr
    
    return rstmt
}

@private
parse_return_stmt :: proc(parser: ^Parser) -> ^Return_Stmt {
    expect(parser, .Return, "Expected 'return'")
    expr := parse_expression(parser)

    rstmt := new(Return_Stmt)
    rstmt.expr = expr
    
    return rstmt
}

@private
parse_var_decl_stmt :: proc(parser: ^Parser) -> ^Variable_Decl_Stmt {
    expect(parser, .Var, "Expected 'var'")

    id := expect(parser, .Identifier, "Expected variable name.").lexeme
    expect(parser, .Colon, "Expected ':' after variable name.")
    type := parse_type_annotation(parser)
    expect(parser, .Equal, "Expected '=' after type annotation.")

    expr := parse_expression(parser)
    
    result := new(Variable_Decl_Stmt)
    result.id = id
    result.type = type
    result.expr = expr

    return result
}

@private
parse_expression :: proc(parser: ^Parser) -> ^Expression {
    return parse_or_expr(parser)
}

@private
parse_or_expr :: proc(parser: ^Parser) -> ^Expression {
    expr := parse_and_expr(parser)
    
    for {
        if match(parser, .Or) {
            advance(parser)

            nexpr := new(Expression)
            bexpr := new(Binary_Expr)
            
            rhs := parse_and_expr(parser)            
            
            bexpr.op = Binary_Operator.Or
            bexpr.lhs = expr
            bexpr.rhs = rhs
            
            nexpr^ = bexpr
            expr = nexpr
        } else {
            return expr
        }
    }
}

@private
parse_and_expr :: proc(parser: ^Parser) -> ^Expression {
    expr := parse_equality_expr(parser)
    
    for {
        if match(parser, .And) {
            advance(parser)

            nexpr := new(Expression)
            bexpr := new(Binary_Expr)
            
            rhs := parse_equality_expr(parser)            
            
            bexpr.op = Binary_Operator.And
            bexpr.lhs = expr
            bexpr.rhs = rhs
            
            nexpr^ = bexpr
            expr = nexpr
        } else {
            return expr
        }
    }
}

@private
parse_equality_expr :: proc(parser: ^Parser) -> ^Expression {
    expr := parse_comparison_expr(parser)

    // This is how we deal with the left recursion problem while maintaining correct precedence
    // Initially it was designed to parse E -> P (==|!=) E
    // This is wrong because it would mess up precedence
    // If transformed to E -> E (==|!=) P
    // This works but we run into an infinite recursion when implemented
    // Instead we transform it to E -> P {()==|!=) P}
    // This gets around both problems
    // This is also called a pratt parser
    // I'm curius how this can be done with pure recursion
    for {
        if match(parser, .Eq) || match(parser, .Neq) {
            op := Binary_Operator.Eq if advance(parser).kind == .Eq else Binary_Operator.Neq

            nexpr := new(Expression)
            bexpr := new(Binary_Expr)
            
            rhs := parse_comparison_expr(parser)            
            
            bexpr.op = op
            bexpr.lhs = expr
            bexpr.rhs = rhs
            
            nexpr^ = bexpr
            expr = nexpr
        } else {
            return expr
        }
    }
}

@private
parse_comparison_expr :: proc(parser: ^Parser) -> ^Expression {
    expr := parse_term_expr(parser)
    
    for {
        if match(parser, .Gt) || match(parser, .Lt) || match(parser, .Gt_Eq) || match(parser, .Lt_Eq) {
            tok := advance(parser)
            
            op := Binary_Operator.Gt
            if tok.kind == .Lt do op = Binary_Operator.Lt
            else if tok.kind == .Gt_Eq do op = Binary_Operator.Gt_Eq
            else if tok.kind == .Lt_Eq do op = Binary_Operator.Lt_Eq

            nexpr := new(Expression)
            bexpr := new(Binary_Expr)
            
            rhs := parse_term_expr(parser)            
            
            bexpr.op = op
            bexpr.lhs = expr
            bexpr.rhs = rhs
            
            nexpr^ = bexpr
            expr = nexpr
        } else {
            return expr
        }
    }
}

@private
parse_term_expr :: proc(parser: ^Parser) -> ^Expression {
    expr := parse_factor_expr(parser)
    
    for {
        if match(parser, .Plus) || match(parser, .Minus) {
            op := Binary_Operator.Add if advance(parser).kind == .Plus else Binary_Operator.Sub

            nexpr := new(Expression)
            bexpr := new(Binary_Expr)
            
            rhs := parse_factor_expr(parser)            
            
            bexpr.op = op
            bexpr.lhs = expr
            bexpr.rhs = rhs
            
            nexpr^ = bexpr
            expr = nexpr
        } else {
            return expr
        }
    }
}

@private
parse_factor_expr :: proc(parser: ^Parser) -> ^Expression {
    expr := parse_unary_expr(parser)
    
    for {
        if match(parser, .Mul) || match(parser, .Div) || match(parser, .Mod) {
            tok := advance(parser)
            
            op := Binary_Operator.Mul
            if tok.kind == .Div do op = Binary_Operator.Div
            else if tok.kind == .Mod do op = Binary_Operator.Mod

            nexpr := new(Expression)
            bexpr := new(Binary_Expr)
            
            rhs := parse_unary_expr(parser)            
            
            bexpr.op = op
            bexpr.lhs = expr
            bexpr.rhs = rhs
            
            nexpr^ = bexpr
            expr = nexpr
        } else {
            return expr
        }
    }
}

@private
parse_unary_expr :: proc(parser: ^Parser) -> ^Expression {
    if match(parser, .Minus) || match(parser, .Not) {
        op := Unary_Operator.Negation if advance(parser).kind == .Minus else Unary_Operator.Not

        expr := new(Expression)
        uexpr := new(Unary_Expr)
        
        // Unlike binary operators we can do this nice and easy with recursion
        uexpr.op = op
        uexpr.expr = parse_unary_expr(parser)
        
        expr^ = uexpr
        
        return expr
    }
    
    return parse_index_expr(parser)
}

// This can be treated as syntactic sugar
// a[1 + 2] -> a [] (1 + 2)
// Unfortunately we cant use parse_primary_expression to parse []
@private
parse_index_expr :: proc(parser: ^Parser) -> ^Expression {
    expr := parse_primary_expr(parser)
    
    for {
        if match(parser, .L_Bracket) {
            expect(parser, .L_Bracket, "Expected '['")
            idx := parse_expression(parser)
            expect(parser, .R_Bracket, "Expected ']'")
            
            nexpr := new(Expression)
            bexpr := new(Binary_Expr)

            bexpr.op = Binary_Operator.Index
            bexpr.lhs = expr
            bexpr.rhs = idx
            
            nexpr^ = bexpr
            expr = nexpr
        }
        else {
            return expr
        }
    }
}

@private
parse_primary_expr :: proc(parser: ^Parser) -> ^Expression {
    if match(parser, .L_Paren) {
        expect(parser, .L_Paren, "Expected '('")
        expr := parse_expression(parser)
        expect(parser, .R_Paren, "Expected ')'")
        return expr
    }

    expr := new(Expression)
    pexpr := new(Primary_Expr)

    if match(parser, .Number_Lit) {
        l := advance(parser).lexeme
        if strings.contains(l, ".") {
            value, ok := strconv.parse_f64(l)
            pexpr^ = Number(value)
            expr^ = pexpr
            return expr
        }

        value, ok := strconv.parse_i64_of_base(l, 10)
        pexpr^ = Number(value)
        expr^ = pexpr
        return expr
    }
    
    if match(parser, .String_Lit) {
        pexpr^ = advance(parser).lexeme
        expr^ = pexpr
        return expr
    }
    
    if match(parser, .Bool_Lit) {
        value, ok := strconv.parse_bool(advance(parser).lexeme)
        pexpr^ = value
        expr^ = pexpr
        return expr
    }
    
    // Variables and function calls
    if match(parser, .Identifier) {
        id := advance(parser).lexeme

        if match(parser, .L_Paren) {
            fcall := new(Function_Call)
            fcall.id = id
            
            advance(parser)
            for {
                if match(parser, .R_Paren) {
                    advance(parser)
                    pexpr^ = fcall
                    expr^ = pexpr
                    return expr
                }
                
                param_expr := parse_expression(parser)
                append(&fcall.params, param_expr)

                if !match(parser, .R_Paren) {
                    expect(parser, .Comma, "Expected ',' after parameter expression")
                }
            }
        } else {
            pexpr^ = Identifier(id)
            expr^ = pexpr
            return expr
        }
    }
    
    // Array literals
    if match(parser, .L_Bracket) {
        advance(parser)
        
        al := new(Array_Literal)
        
        for {
            if match(parser, .R_Bracket) {
                advance(parser)
                pexpr^ = al
                expr^ = pexpr
                return expr
            }
            
            lit_expr := parse_expression(parser)
            append(al, lit_expr)
            
            if !match(parser, .R_Bracket) do expect(parser, .Comma, "Expected ',' after expression")
        }
    }

    fmt.println("Expected expression", peek(parser))
    os.exit(-1)
}

@private
skip_until :: proc(parser: ^Parser, token_kind: lx.Token_Kind) {
    for !is_at_end(parser) && peek(parser).kind != token_kind {
        advance(parser)
    }
}

@private
skip_while :: proc(parser: ^Parser, token_kind: lx.Token_Kind) {
    for !is_at_end(parser) && peek(parser).kind == token_kind {
        advance(parser)
    }
}

@private
expect :: proc(parser: ^Parser, token_kind: lx.Token_Kind, error_message: string) -> lx.Token {
    token := advance(parser)

    if token.kind != token_kind {
        fmt.printf("Error [line:%d col:%d token:%s]: %s", token.line, token.column, token.lexeme, error_message)
        os.exit(-1)
    }

    return token
}

@private
match :: proc(parser: ^Parser, token_kind: lx.Token_Kind) -> bool {
    return peek(parser).kind == token_kind
}

@private
advance :: proc(parser: ^Parser) -> lx.Token {
    if is_at_end(parser) {
        return lx.Token {
            kind = .EOF,
            lexeme = "",
            line = -1,
            column = -1,
        }
    }

    parser.pos += 1
    return parser.tokens[parser.pos - 1]
}

@private
peek :: proc(parser: ^Parser, ahead: int = 0) -> lx.Token {
    if is_at_end(parser, ahead) {
        return lx.Token {
            kind = .EOF,
            lexeme = "", 
            line = -1, 
            column = -1,
        }
    }

    return parser.tokens[parser.pos + ahead]
}

@private
is_at_end :: proc(parser: ^Parser, ahead: int = 0) -> bool {
    return parser.pos + ahead >= len(parser.tokens)
}