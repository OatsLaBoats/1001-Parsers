package lexer

@private
Scanner :: struct {
    src: []u8,

    start: int, // User field
    current: int,

    line: int,
    column: int,

    prev_column: int,
}

@private
EOF :: -1

@private
make_scanner :: proc(src: string) -> Scanner {
    return Scanner {
        src = transmute([]u8)src,
        start = 0,
        current = 0,
        line = 1,
        column = 1,
    }
}

@private
peek :: proc(scanner: ^Scanner, look_ahead: int = 0) -> rune {
    if (scanner.current + look_ahead) >= len(scanner.src) {
        return EOF
    }

    return rune(scanner.src[scanner.current + look_ahead])
}

@private
advance :: proc(scanner: ^Scanner) -> rune {
    if (scanner.current) >= len(scanner.src) {
        return EOF
    }

    result := rune(scanner.src[scanner.current])
    scanner.current += 1

    scanner.prev_column = scanner.column
    scanner.column += 1

    if result == '\n' {
        scanner.column = 1
        scanner.line += 1
    }

    return result
}

@private
match :: proc(scanner: ^Scanner, ch: rune) -> bool {
    if peek(scanner) == ch {
        advance(scanner)
        return true
    }

    return false
}

@private
is_at_end :: proc(scanner: ^Scanner) -> bool {
    assert(scanner.current <= len(scanner.src))
    return scanner.current == len(scanner.src)
}

@private
get_lexeme :: proc {
    get_lexeme_ex,
    get_lexeme_simple,
}

@private
get_lexeme_ex :: proc(scanner: ^Scanner, start, end: int) -> string {
    return string(scanner.src[start:end])
}

@private
get_lexeme_simple :: proc(scanner: ^Scanner) -> string {
    return string(scanner.src[scanner.start:scanner.current])
}