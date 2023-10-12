package main

import "core:fmt"
import "core:os"
import "core:mem/virtual"
import "core:mem"

import "lexer"
import "parser"
import "analyser"
import "ast"

// TODO: Make a repl
// TODO: Make commandline options: select file, on/off token printer, on/off ast printer, on/off only compiling
// TODO: Redo the error system for the whole project
// TODO: Add structs using data keyword

main :: proc() {
    print_tokens := false
    print_ast := true

    contents, success := os.read_entire_file_from_filename("../test.sigma")
    defer delete(contents)

    if !success {
        fmt.println("Failed to read file.")
        os.exit(-1)
    }
    
    source := string(contents)

    lex := lexer.scan(source)
    defer lexer.delete_lexer(lex)
    
    if len(lex.errors) > 0 {
        for error in lex.errors {
            fmt.println("Lexing Error:", error)
        }
        
        os.exit(-1)
    }
    
    if print_tokens {
        lexer.print_tokens(lex)
        fmt.println()
    }

    tree: ast.Ast
    err := ast.init(&tree)
    defer ast.destroy(&tree)
    if err {
        fmt.println("Failed to initialize AST")
        os.exit(-1)
    }

    parser.parse(&tree, lex.tokens[:])

    if print_ast {
        ast.print(&tree)
        fmt.println()
    }

    fmt.println("Memory used(bytes): ", tree._arena_p.total_used, "/", tree._arena_p.total_reserved, sep="")
    fmt.println("Memory used(megabytes): ", cast(f64)tree._arena_p.total_used / mem.Megabyte, "/", cast(f64)tree._arena_p.total_reserved / mem.Megabyte, sep="")
    
    analyser_errors := analyser.analyse(&tree)

    for e in analyser_errors {
        fmt.println(e)
    }
}