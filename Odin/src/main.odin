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
import "optimizer"
import "shared"

main :: proc() {
    print_tokens := false
    print_ast := false
    optimise := false
    only_compile := false
    filename := ""

    for arg in os.args[1:] {
        switch arg {
            case "--help": {
                fmt.println("Usage: sigma [options...] \"source file\"")
                fmt.println("Options:")
                fmt.println("         --help          Displays this message.")
                fmt.println("         --print-ast     Prints out the synatax tree.")
                fmt.println("         --print-tokens  Prints out the stream of lexer tokens.")
                fmt.println("         --print-all     Enables all printing functionality.")
                fmt.println("         --fast          Enables optimization.")
                fmt.println("         --only-compile  Compiles the script without running it.")
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
    
    if !os.exists(filename) {
        fmt.println("Error: File \"", filename, "\" doesn't exist", sep="")
        os.exit(-1)
    }
    
    contents, success := os.read_entire_file_from_filename(filename)
    defer delete(contents)

    if !success {
        fmt.println("Error: Failed to read file \"", filename, "\"", sep="")
        os.exit(-1)
    }
    
    source := string(contents)
    lex := lexer.scan(source)
    defer lexer.delete_lexer(lex)
    
    if len(lex.errors) > 0 {
        shared.print_error_list(&lex.errors)
        os.exit(-1)
    }

    tree := ast.Ast {}
    defer ast.delete_ast(&tree)

    parser.parse(&tree, lex.tokens[:])
    
    analyser_errors := analyser.analyse(&tree)
    defer shared.delete_error_list(analyser_errors)

    if len(analyser_errors) > 0 {
        shared.print_error_list(&analyser_errors)
        os.exit(-1)
    }
    
    if optimise {
        optimizer.tailcall_pass(&tree)
    }

    if print_tokens {
        lexer.print_tokens(lex)
        fmt.println()
    }

    if print_ast {
        ast.print(&tree)
        fmt.println()
    }

    if !only_compile {
        err_code := interpreter.eval(&tree)
        if err_code != 0 do os.exit(int(err_code))
    }
}
