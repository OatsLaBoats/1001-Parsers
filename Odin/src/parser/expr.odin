package parser

import "core:os"
import "core:fmt"
import "core:strconv"
import "core:strings"

import "../ast"

@private
parse_expression :: proc(parser: ^Parser) -> ^ast.Expression {
    return parse_or_expr(parser)
}

@private
parse_or_expr :: proc(parser: ^Parser) -> ^ast.Expression {
    expr := parse_and_expr(parser)
    
    for {
        if match(parser, .Or) {
            advance(parser)

            nexpr := new(ast.Expression)
            bexpr := new(ast.Binary_Expr)
            
            rhs := parse_and_expr(parser)            
            
            bexpr.op = ast.Binary_Operator.Or
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
parse_and_expr :: proc(parser: ^Parser) -> ^ast.Expression {
    expr := parse_equality_expr(parser)
    
    for {
        if match(parser, .And) {
            advance(parser)

            nexpr := new(ast.Expression)
            bexpr := new(ast.Binary_Expr)
            
            rhs := parse_equality_expr(parser)            
            
            bexpr.op = ast.Binary_Operator.And
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
parse_equality_expr :: proc(parser: ^Parser) -> ^ast.Expression {
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
            op := ast.Binary_Operator.Eq if advance(parser).kind == .Eq else ast.Binary_Operator.Neq

            nexpr := new(ast.Expression)
            bexpr := new(ast.Binary_Expr)
            
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
parse_comparison_expr :: proc(parser: ^Parser) -> ^ast.Expression {
    expr := parse_term_expr(parser)
    
    for {
        if match(parser, .Gt) || match(parser, .Lt) || match(parser, .Gt_Eq) || match(parser, .Lt_Eq) {
            tok := advance(parser)
            
            op := ast.Binary_Operator.Gt
            if tok.kind == .Lt do op = ast.Binary_Operator.Lt
            else if tok.kind == .Gt_Eq do op = ast.Binary_Operator.Gt_Eq
            else if tok.kind == .Lt_Eq do op = ast.Binary_Operator.Lt_Eq

            nexpr := new(ast.Expression)
            bexpr := new(ast.Binary_Expr)
            
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
parse_term_expr :: proc(parser: ^Parser) -> ^ast.Expression {
    expr := parse_factor_expr(parser)
    
    for {
        if match(parser, .Plus) || match(parser, .Minus) {
            op := ast.Binary_Operator.Add if advance(parser).kind == .Plus else ast.Binary_Operator.Sub

            nexpr := new(ast.Expression)
            bexpr := new(ast.Binary_Expr)
            
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
parse_factor_expr :: proc(parser: ^Parser) -> ^ast.Expression {
    expr := parse_unary_expr(parser)
    
    for {
        if match(parser, .Mul) || match(parser, .Div) || match(parser, .Mod) {
            tok := advance(parser)
            
            op := ast.Binary_Operator.Mul
            if tok.kind == .Div do op = ast.Binary_Operator.Div
            else if tok.kind == .Mod do op = ast.Binary_Operator.Mod

            nexpr := new(ast.Expression)
            bexpr := new(ast.Binary_Expr)
            
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
parse_unary_expr :: proc(parser: ^Parser) -> ^ast.Expression {
    if match(parser, .Minus) || match(parser, .Not) {
        op := ast.Unary_Operator.Negation if advance(parser).kind == .Minus else ast.Unary_Operator.Not

        expr := new(ast.Expression)
        uexpr := new(ast.Unary_Expr)
        
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
parse_index_expr :: proc(parser: ^Parser) -> ^ast.Expression {
    expr := parse_primary_expr(parser)
    
    for {
        if match(parser, .L_Bracket) {
            expect(parser, .L_Bracket, "Expected '['")
            idx := parse_expression(parser)
            expect(parser, .R_Bracket, "Expected ']'")
            
            nexpr := new(ast.Expression)
            bexpr := new(ast.Binary_Expr)

            bexpr.op = ast.Binary_Operator.Index
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
parse_primary_expr :: proc(parser: ^Parser) -> ^ast.Expression {
    if match(parser, .L_Paren) {
        expect(parser, .L_Paren, "Expected '('")
        expr := parse_expression(parser)
        expect(parser, .R_Paren, "Expected ')'")
        return expr
    }

    expr := new(ast.Expression)
    pexpr := new(ast.Primary_Expr)

    if match(parser, .Number_Lit) {
        l := advance(parser).lexeme
        if strings.contains(l, ".") {
            value, ok := strconv.parse_f64(l)
            pexpr^ = ast.Number(value)
            expr^ = pexpr
            return expr
        }

        value, ok := strconv.parse_i64_of_base(l, 10)
        pexpr^ = ast.Number(value)
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
            fcall := new(ast.Function_Call)
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
            pexpr^ = ast.Identifier(id)
            expr^ = pexpr
            return expr
        }
    }
    
    // Array literals
    if match(parser, .L_Bracket) {
        advance(parser)
        
        al := new(ast.Array_Literal)
        
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