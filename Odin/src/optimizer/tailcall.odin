package optimizer

import "../ast"
// This is not meant to be well implemented. Its just here to test out ast manipulation

tailcall_pass :: proc(tree: ^ast.Ast) {
    for f in tree.functions {
        if has_tailcall(f) {
            new_body := make([dynamic]^ast.Statement)
            loop_s := new(ast.Statement)
            loop := new(ast.While_Stmt)
            loop_e := new(ast.Expression)
            loop_pe := new(ast.Primary_Expr)
            loop_b := new(ast.Block)

            loop_pe^ = ast.Bool_Lit { true }
            loop_e^ = loop_pe
            loop.cond = loop_e
            loop.block = loop_b
            loop_s^ = loop

            // Move over the statements excluding the return
            for i in 0..<(len(f.block.stmts) - 1) {
                append(&loop_b.stmts, f.block.stmts[i])
            }
            
            // Make the passed parameters into assignment statements
            last_stmt := f.block.stmts[len(f.block.stmts) - 1]
            params := last_stmt.(^ast.Return_Stmt).expr.(^ast.Primary_Expr).(^ast.Function_Call).params
            for e, i in params {
                var_s := new(ast.Statement)
                var := new(ast.Assignment_Stmt)
                var.id = f.params[i].id
                var.expr = e
                var_s^ = var

                append(&loop_b.stmts, var_s)
            }
            
            // We don't typecheck any of these generated functions so no need to add a return statement,
            // since we know it will never be reached.
            append(&new_body, loop_s)
            
            // Free old stuff
            free(last_stmt.(^ast.Return_Stmt).expr.(^ast.Primary_Expr).(^ast.Function_Call))
            free(last_stmt.(^ast.Return_Stmt).expr.(^ast.Primary_Expr))
            free(last_stmt.(^ast.Return_Stmt).expr)
            free(last_stmt.(^ast.Return_Stmt))
            free(last_stmt)
            delete(f.block.stmts)
            
            f.block.stmts = new_body
        }
    }
}

@private
has_tailcall :: proc(f: ^ast.Function) -> bool {
    last_stmt := f.block.stmts[len(f.block.stmts) - 1]
    if ast.is_return_stmt(last_stmt) {
        stmt := last_stmt.(^ast.Return_Stmt)
        if ast.is_primary_expr(stmt.expr) {
            expr := stmt.expr.(^ast.Primary_Expr)
            if ast.is_function_call(expr) {
                fn := expr.(^ast.Function_Call)
                return fn.id == f.id
            }
        }
    }

    return false
}