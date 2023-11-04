package ast

import vrt "core:mem/virtual"
import "core:mem"
import "core:fmt"

import "../shared"

Ast :: struct {
    functions: [dynamic]^Function,

    _arena1: ^vrt.Arena,
    _arena2: ^vrt.Arena,
    
    _arena_p: ^vrt.Arena,
    _to_free: ^vrt.Arena,
}

Function :: struct {
    info: shared.Source_Info,
    id: string,
    return_type: Type,
    params: [dynamic]^Function_Parameter,
    block: ^Block,
}

Function_Parameter :: struct {
    info: shared.Source_Info,
    id: string,
    param_type: Type,
}

Block :: struct {
    // Can be null because we might remove some invalid statements before typechecking.
    // Ideally I would recreate the tree but its really annoying to have to rebuild it because of manual allocation.
    stmts: [dynamic]^Statement,
}

Statement :: union {
    ^Variable_Decl_Stmt,
    ^Return_Stmt,
    ^Print_Stmt,
    ^While_Stmt,
    ^If_Stmt,
    ^Assignment_Stmt,
    ^Index_Assignment_Stmt,
    ^Raw_Expr_Stmt,
}

Variable_Decl_Stmt :: struct {
    info: shared.Source_Info,
    id: string,
    var_type: Type,
    expr: ^Expression,
}

Return_Stmt :: struct {
    info: shared.Source_Info,
    expr: ^Expression,
}

Print_Stmt :: struct {
    expr: ^Expression,
}

While_Stmt :: struct {
    info: shared.Source_Info,
    cond: ^Expression,
    block: ^Block,
}

If_Stmt :: struct {
    info: shared.Source_Info,

    cond: ^Expression,
    block: ^Block,

    elif_stmt: ^If_Stmt, // optional
    else_block: ^Block,  // optional
}

Assignment_Stmt :: struct {
    info: shared.Source_Info,
    id: string,
    expr: ^Expression,
}

Index_Assignment_Stmt :: struct {
    info: shared.Source_Info,
    id: string,
    index: ^Expression,
    expr: ^Expression,
}

Raw_Expr_Stmt :: struct {
    expr: ^Expression,
}

// Make this a union maybe
Expression :: union {
    ^Binary_Expr,
    ^Unary_Expr,
    ^Primary_Expr,
}

// Ordered in ascending precedence
Binary_Operator :: enum {
    Or, // Bool

    And, // Bool

    Eq, // Any
    Neq, // Any

    Gt, // Int, Float
    Lt, // Int, Float
    Gt_Eq, // Int, Float
    Lt_Eq, // Int, Float

    Add, // Int, Float, Array, String
    Sub, // Int, Float

    Mul, // Int, Float
    Div, // Int, Float
    Mod, // Int, Float

    // []
    Index, // Array, String
}

Binary_Expr :: struct  {
    info: shared.Source_Info,
    op: Binary_Operator,
    lhs: ^Expression,
    rhs: ^Expression,
}

Unary_Operator :: enum {
    Negation,
    Not,
}

Unary_Expr :: struct {
    info: shared.Source_Info,
    op: Unary_Operator,
    expr: ^Expression,
}

Primary_Expr :: union {
    Int_Lit,
    Float_Lit,
    String_Lit,
    Bool_Lit,
    Identifier,
    ^Array_Lit,
    ^Expression,
    ^Function_Call,
}

Int_Lit :: struct {
    value: i64
}

Float_Lit :: struct {
    value: f64
}

String_Lit :: struct {
    value: string
}

Bool_Lit :: struct {
    value: bool
}

Array_Lit :: struct {
    info: shared.Source_Info,
    values: [dynamic]^Expression,
}

Identifier :: struct {
    info: shared.Source_Info,
    value: string,
}

Function_Call :: struct {
    info: shared.Source_Info,
    id: string,
    params: [dynamic]^Expression,
}

swap_arena :: proc(ast: ^Ast) -> mem.Allocator {
    if ast._to_free != nil {
        free_all(vrt.arena_allocator(ast._to_free))
    }

    ast._to_free = ast._arena_p

    if ast._arena_p == ast._arena1 {
        ast._arena_p = ast._arena2
    } else {
        ast._arena_p = ast._arena1
    }
    
    return vrt.arena_allocator(ast._arena_p)
}

get_arena :: proc(ast: ^Ast) -> mem.Allocator {
    return vrt.arena_allocator(ast._arena_p)
}

init :: proc(ast: ^Ast, allocator := context.allocator) -> bool {
    ast._arena1 = new(vrt.Arena, allocator)
    ast._arena2 = new(vrt.Arena, allocator)

    // Arenas use the page allocator of the os so we can't pass in the context
    e1 := vrt.arena_init_growing(ast._arena1)
    e2 := vrt.arena_init_growing(ast._arena2)
    
    if e1 == .Out_Of_Memory || e2 == .Out_Of_Memory {
        return true
    }

    ast._arena_p = ast._arena1
    ast._to_free = nil

    ast.functions = make([dynamic]^Function, vrt.arena_allocator(ast._arena_p))

    return false
}

destroy :: proc(ast: ^Ast) {
    vrt.arena_destroy(ast._arena1)
    vrt.arena_destroy(ast._arena2)
    
    free(ast._arena1)
    free(ast._arena2)
}