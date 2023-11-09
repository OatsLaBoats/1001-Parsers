package analyser

import "../ast"
import "core:fmt"

// TODO: Fix bug where an error is thrown if the return statement exists at the end of the function but not inside the if.
@private
missing_return_check :: proc(an: ^Analyser) {
    for _, f in an.functions {
        if f.return_type != nil {
            if !mrc_block(an, f.block) {
                append(&an.errors, make_error(f.info, "Function \"%s\" is missing a 'return' statement", f.id))
            }
        }
    }
}

@private
mrc_block :: proc(an: ^Analyser, block: ^ast.Block) -> bool {
    for s in block.stmts {
        #partial switch stmt in s {
            case ^ast.Return_Stmt: return true
            case ^ast.If_Stmt: if mrc_if_stmt(an, stmt) do return true
            case ^ast.While_Stmt: return mrc_block(an, stmt.block)
        }
    }
    
    return false
}

@private
mrc_if_stmt :: proc(an: ^Analyser, stmt: ^ast.If_Stmt) -> bool {
    // If there no elif or else then there is no way we can gurantee that there is a return
    if stmt.elif_stmt == nil && stmt.else_block == nil do return false

    if_res := mrc_block(an, stmt.block)
    
    // If the elif is nil but the else is not that means we reached the end of the chain and we can set it to true
    elif_res := stmt.elif_stmt == nil && stmt.else_block != nil
    if stmt.elif_stmt != nil {
        elif_res = mrc_if_stmt(an, stmt.elif_stmt)
    }

    // If an elif is true that means there must be an else block somewhere along the chain
    else_res := elif_res
    if stmt.else_block != nil {
        else_res = mrc_block(an, stmt.else_block)
    }
    
    return if_res && elif_res && else_res
}