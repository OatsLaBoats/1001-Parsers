package parser

import "../ast"

@private 
parse_function_decl :: proc(parser: ^Parser) -> ^ast.Function_Decl {
    expect(parser, .Fun, "Expected 'fun' keyword.")
    id := expect(parser, .Identifier, "Expected identifier after 'fun' keyword.")

    params := parse_function_paramters(parser)

    return_type: ast.Type = nil
    if match (parser, .Colon) {
        advance(parser)
        return_type = parse_type_annotation(parser)
    }

    block := parse_block(parser)
    
    result := new(ast.Function_Decl)
    result.id = id.lexeme
    result.return_type = return_type
    result.params = params
    result.block = block

    return result
}

@private
parse_type_annotation :: proc(parser: ^Parser) -> ast.Type {
    if match(parser, .L_Bracket) {
        advance(parser)

        arr_type := new(ast.Type)
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
parse_function_paramters :: proc(parser: ^Parser) -> [dynamic]^ast.Function_Parameter {
    expect(parser, .L_Paren, "Expected '(' after function name.")

    result: [dynamic]^ast.Function_Parameter

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
parse_parameter :: proc(parser: ^Parser) -> ^ast.Function_Parameter {
    id := expect(parser, .Identifier, "Expected parameter name after '('.").lexeme
    expect(parser, .Colon, "Expected ':' after parameter name.")
    type := parse_type_annotation(parser)

    result := new(ast.Function_Parameter)
    result.id = id
    result.type = type

    return result
}

@private
parse_block :: proc(parser: ^Parser) -> ^ast.Block {
    expect(parser, .L_Brace, "Missing '{'")

    stmts: [dynamic]^ast.Statement

    for !is_at_end(parser) {
        skip_while(parser, .Line_End)

        if match(parser, .R_Brace) { break }

        stmt := parse_statement(parser)
        append(&stmts, stmt)
    }
    
    skip_until(parser, .R_Brace)
    expect(parser, .R_Brace, "Missing '}'")

    result := new(ast.Block)
    result.stmts = stmts

    return result
}