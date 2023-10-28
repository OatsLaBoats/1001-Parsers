package main

import "core:fmt"
import "core:os"
import "core:mem"
import "core:strings"

import "lexer"
import "parser"
import "analyser"
import "ast"
import "interpreter"

// TODO: Make a repl
// TODO: Make commandline options: select file, on/off token printer, on/off ast printer, on/off only compiling
// TODO: Add structs using data keyword
// TODO: Cleanup the ast to remove nil statements
// TODO: Rename function_decl to def because its not a declaration
// TODO: Rethink the lexer and parser error system maybe unify them all.
// TODO: Use temp_allocator more
// TODO: Use normal allocator instead of arena
// TODO: Cleanup code and change to better variable names

main :: proc() {
    print_tokens := false
    print_ast := false
    optimise := false
    only_compile := false
    filename := "../benchmark.sigma"

    for arg in os.args[1:] {
        switch arg {
            case "--help": {
                fmt.println("Usage: sigma [options...] \"source file\"")
                fmt.println("Options:")
                fmt.println("         --help           Displays this message.")
                fmt.println("         --print-ast      Prints out the synatax tree.")
                fmt.println("         --print-tokens   Prints out the stream of lexer tokens.")
                fmt.println("         --print-all      Enables all printing functionality.")
                fmt.println("         --fast           Enables optimization.")
                fmt.println("         --only-compile   Compiles the script without running it.")
                os.exit(0)
            }

            case "--print-ast": print_ast = true
            case "--print-tokens": print_tokens = true

            case "--print-all": {
                print_ast = true
                print_tokens = true
            }

            case "--fast": optimise = true
            case "--only-compile": only_compile = true

            case: {
                if strings.has_suffix(arg, ".sigma") {
                    if filename == "" do filename = arg
                } else {
                    fmt.println("Error: Unknown option \"", arg, "\"", sep="")
                    os.exit(-1)
                }
            }
        }
    }
    
    if filename == "" {
        fmt.println("Error: No source file provided")
        os.exit(-1)
    }
    
    contents, success := os.read_entire_file_from_filename(filename)
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
            fmt.printf("Error(%d:%d):", error.line, error.column, error)
        }
        
        os.exit(-1)
    }

    tree: ast.Ast
    err := ast.init(&tree)
    defer ast.destroy(&tree)
    if err {
        fmt.println("Error: Out of memory")
        os.exit(-1)
    }

    parser.parse(&tree, lex.tokens[:])
    
    if print_tokens {
        lexer.print_tokens(lex)
        fmt.println()
    }

    if print_ast {
        ast.print(&tree)
        fmt.println()
    }

    analyser_errors := analyser.analyse(&tree)
    defer analyser.delete_error_list(analyser_errors)

    if len(analyser_errors) > 0 {
        for e in analyser_errors {
            fmt.printf("Error(%d:%d): %s\n", e.info.line, e.info.column, strings.to_string(e.msg))
        }       
        
        os.exit(-1)
    }
    
    err_code := interpreter.eval(&tree)
}