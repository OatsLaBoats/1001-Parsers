package parser

import "core:fmt"
import "core:os"

import lx "../lexer"

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
        fmt.printf("Error(%d:%d): %s", token.info.line, token.info.column, error_message)
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
            info = {-1, -1},
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
            info = {-1, -1},
        }
    }

    return parser.tokens[parser.pos + ahead]
}

@private
is_at_end :: proc(parser: ^Parser, ahead: int = 0) -> bool {
    return parser.pos + ahead >= len(parser.tokens)
}