package lexer

import "core:fmt"

Token_Kind :: enum {
    // Keywords
    Fun, Return, Print, Var, 
    If, Else, Elif, While,

    // Literals
    Identifier, 
    Number_Lit,
    String_Lit,
    Bool_Lit,

    // Arithmetic operators
    Plus, Minus, Mul, Div, Mod, 
    
    // Comparison operators
    Eq, Neq, Gt, Lt, Gt_Eq, Lt_Eq, 
    
    // Logic operators
    And, Or, Not,

    // Symbols
    L_Paren, R_Paren,
    L_Brace, R_Brace, // {}
    L_Bracket, R_Bracket, // []
    Equal, Colon, Comma,

    // Other
    Line_End, EOF,
}

Token :: struct {
    kind: Token_Kind,
    lexeme: string,
    line: int,
    column: int,
}

Lexer :: struct {
    tokens: [dynamic]Token,
    errors: [dynamic]Error,
}

Error :: struct {
    msg: string,
    line: int, 
    column: int,
}

Result :: union #no_nil {
    Token,
    Error,
}

print_tokens :: proc(lex: Lexer) {
    for t in lex.tokens {
        fmt.println(t)
    }
}

delete_lexer :: proc(lex: Lexer) {
    delete(lex.tokens)
    delete(lex.errors)
}

scan :: proc(source: string) -> Lexer {
    scanner := make_scanner(source)
    lex := Lexer {}

    for !is_at_end(&scanner) {
        res := scan_token(&scanner)
        switch v in res {
            case Token: append(&lex.tokens, v)
            case Error: append(&lex.errors, v)
        }
    }

    return lex
}

@private
scan_token :: proc(scanner: ^Scanner) -> Result {
    skip_whitespace(scanner)

    // Advance the start of the token
    scanner.start = scanner.current

    ch := advance(scanner)

    line := scanner.line
    column := scanner.prev_column

    switch ch {
        case '+': return make_simple_token(.Plus, line, column)
        case '-': return make_simple_token(.Minus, line, column)
        case '*': return make_simple_token(.Mul, line, column)
        case '/': return make_simple_token(.Div, line, column)
        case '%': return make_simple_token(.Mod, line, column)

        case '(': return make_simple_token(.L_Paren, line, column)
        case ')': return make_simple_token(.R_Paren, line, column)
        case '{': return make_simple_token(.L_Brace, line, column)
        case '}': return make_simple_token(.R_Brace, line, column)
        case '[': return make_simple_token(.L_Bracket, line, column)
        case ']': return make_simple_token(.R_Bracket, line, column)
        case ':': return make_simple_token(.Colon, line, column)
        case ',': return make_simple_token(.Comma, line, column)

        case '=': return scan_equal(scanner, line, column)
        case '!': return scan_bang(scanner, line, column)
        case '>': return scan_greater_than(scanner, line, column)
        case '<': return scan_less_than(scanner, line, column)
        
        case '"': return scan_string(scanner, line, column)

        case 'a'..='z', 'A'..='Z', '_': return scan_word(scanner, line, column)
        case '0'..='9': return scan_digit(scanner, line, column)

        case '\n': return make_simple_token(.Line_End, line - 1, column)
    }

    return Error { "Unexpected character", line, column }
}

@private
scan_string :: proc(scanner: ^Scanner, line, column: int) -> Result {
    for {
        ch := peek(scanner)

        switch ch {
            case '"': {
                advance(scanner)
                // Remove the quotes
                lexeme := get_lexeme(scanner, scanner.start + 1, scanner.current - 1)
                return make_token(.String_Lit, lexeme, line, column)
            }
            
            case '\n', EOF: return Error { "String isn't closed", line, column }
        }

        advance(scanner)
    }
}

@private
scan_less_than :: proc(scanner: ^Scanner, line, column: int) -> Result {
    kind := match(scanner, '=') ? Token_Kind.Lt_Eq : Token_Kind.Lt
    return make_simple_token(kind, line, column)
}

@private
scan_greater_than :: proc(scanner: ^Scanner, line, column: int) -> Result {
    kind := match(scanner, '=') ? Token_Kind.Gt_Eq : Token_Kind.Gt
    return make_simple_token(kind, line, column)
}

@private
scan_bang :: proc(scanner: ^Scanner, line, column: int) -> Result {
    if match(scanner, '=') {
        return make_simple_token(.Neq, line, column)
    }

    return Error { "Incomplete '!=' operator", line, column }
}

@private
scan_equal :: proc(scanner: ^Scanner, line, column: int) -> Result {
    kind := match(scanner, '=') ? Token_Kind.Eq : Token_Kind.Equal
    return make_simple_token(kind, line, column)
}

@private
scan_digit :: proc(scanner: ^Scanner, line, column: int) -> Result {
    for is_digit(peek(scanner)) {
        advance(scanner)
    }

    if match(scanner, '.') {
        // Ensure that there are followup numbers
        if is_digit(peek(scanner)) {
            for is_digit(peek(scanner)) {
                advance(scanner)
            }
        }
        else {
            return Error { "Invalid float syntax.", line, column }
        }
    }

    lexeme := get_lexeme(scanner)
    return make_token(.Number_Lit, lexeme, line, column)
}

@private
scan_word :: proc(scanner: ^Scanner, line, column: int) -> Result {
    for is_alpha(peek(scanner)) || is_digit(peek(scanner)) {
        advance(scanner)
    }

    lexeme := get_lexeme(scanner)
    return make_word_token(lexeme, line, column)
}

@private
make_word_token :: proc(lexeme: string, line, column: int) -> Token {
    switch lexeme {
        case "fun": return make_simple_token(.Fun, line, column)
        case "return": return make_simple_token(.Return, line, column)
        case "print": return make_simple_token(.Print, line, column)
        case "var": return make_simple_token(.Var, line, column)
        case "if": return make_simple_token(.If, line, column)
        case "else": return make_simple_token(.Else, line, column)
        case "elif": return make_simple_token(.Elif, line, column)
        case "while": return make_simple_token(.While, line, column)

        case "and": return make_simple_token(.And, line, column)
        case "or": return make_simple_token(.Or, line, column)
        case "not": return make_simple_token(.Not, line, column)

        case "true", "false": return make_token(.Bool_Lit, lexeme, line, column)

        case: return make_token(.Identifier, lexeme, line, column)
    }
}

@private
skip_whitespace :: proc(scanner: ^Scanner) {
    loop: for {
        ch := peek(scanner)

        switch ch {
            case ' ', '\t', '\r': advance(scanner)
            
            case '#': {
                for ; peek(scanner) != '\n' && !is_at_end(scanner) ; {
                    advance(scanner)
                }
            }

            case: break loop
        }
    }
}

@private
make_token :: proc(kind: Token_Kind, lexeme: string, line, column: int) -> Token {
    return Token {
        kind = kind,
        lexeme = lexeme,
        line = line,
        column = column,
    }
}

@private
make_simple_token :: proc(kind: Token_Kind, line, column: int) -> Token {
    return make_token(kind, "", line, column)
}

@private
is_alpha :: proc(ch: rune) -> bool {
    return (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z') || ch == '_'
}

@private
is_digit :: proc(ch: rune) -> bool {
    return ch >= '0' && ch <= '9'
}