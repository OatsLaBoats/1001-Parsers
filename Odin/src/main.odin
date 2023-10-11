package main

import "core:fmt"
import "core:os"
import "core:mem/virtual"
import "core:mem"

import "lexer"
import "parser"
import "analyser"

// TODO: Make a repl
// TODO: Make commandline options: select file, on/off token printer, on/off ast printer, on/off only compiling
// TODO: Redo the error system for the whole project

main :: proc() {
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
    
    lexer.print_tokens(lex)

    fmt.println()
    
    arena: virtual.Arena
    err := virtual.arena_init_growing(&arena)
    defer virtual.arena_destroy(&arena)
    if err == .Out_Of_Memory {
        fmt.println("Failed to create arena")
        os.exit(-1)
    }

    old_ctx := context
    context.allocator = virtual.arena_allocator(&arena)
    ast := parser.parse(lex.tokens[:])
    context = old_ctx

    //parser.print_ast(&ast)

    fmt.println("\nMemory used(bytes): ", arena.total_used, "/", arena.total_reserved, sep="")
    fmt.println("Memory used(megabytes): ", cast(f64)arena.total_used / mem.Megabyte, "/", cast(f64)arena.total_reserved / mem.Megabyte, sep="")
    
    errs := analyser.analyse(&ast)
    
    for e in errs {
        fmt.println(e)
    }
}