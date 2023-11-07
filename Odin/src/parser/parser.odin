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

package parser

import lx "../lexer"
import "../ast"

@private
Parser :: struct {
    tokens: []lx.Token,
    pos: int,
}

parse :: proc(tree: ^ast.Ast, tokens: []lx.Token) {
    context.allocator = ast.get_arena(tree)

    parser := Parser {
        tokens = tokens,
    }

    for {
        // Skips top level new lines
        skip_while(&parser, .Line_End)

        if is_at_end(&parser) { break }

        f := parse_function(&parser)
        append(&tree.functions, f) 
    }
}