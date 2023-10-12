package analyser

// TODO: Clean this up cause holy shit its a mess

import "core:fmt"

import "../ast"

Vars :: map[string]ast.Type

@private
typecheck :: proc(an: ^Analyser) {
    for _, f in an.functions {
        vars := make(Vars)
        defer delete(vars)
        
        for p in f.params {
            vars[p.id] = p.type            
        }

        for s in f.block.stmts {
            if s != nil do tc_statement(an, &vars, s, f.id)
        }
    
        // We may need to allocate space for an array type in that case we should free the temporary allocator
        free_all(context.temp_allocator)
    }
}

@private
tc_statement :: proc(an: ^Analyser, vars: ^Vars, stmt: ^ast.Statement, func_id: string) {
    switch s in stmt {
        case ^ast.Variable_Decl_Stmt: tc_variable_decl_stmt(an, vars, s)
        case ^ast.Return_Stmt: tc_return_stmt(an, vars, s, func_id)
        case ^ast.Print_Stmt: tc_print_stmt(an, vars, s)
        case ^ast.While_Stmt: tc_while_stmt(an, vars, s, func_id)
        case ^ast.If_Stmt: tc_if_stmt(an, vars, s, func_id)
        case ^ast.Assignment_Stmt: tc_assignment_stmt(an, vars, s)
        case ^ast.Index_Assignment_Stmt: tc_index_stmt(an, vars, s)
    }
}

@private
tc_index_stmt :: proc(an: ^Analyser, vars: ^Vars, stmt: ^ast.Index_Assignment_Stmt) {
    var := ast.get_array_type_internal(vars[stmt.id])
    etype := tc_expression(an, vars, stmt.expr)
    
    tc_expression(an, vars, stmt.index)

    if etype != nil && !ast.is_type_equal(etype, var) {
        append(&an.errors, Error { "Type mismatch", stmt.id })
    }
}

@private
tc_assignment_stmt :: proc(an: ^Analyser, vars: ^Vars, stmt: ^ast.Assignment_Stmt) {
    var := vars[stmt.id]
    etype := tc_expression(an, vars, stmt.expr)
    
    if etype != nil && !ast.is_type_equal(var, etype) {
        append(&an.errors, Error { "Type mismatch", stmt.id })
    }
}

@private
tc_if_stmt :: proc(an: ^Analyser, vars: ^Vars, stmt: ^ast.If_Stmt, func_id: string) {
    etype := tc_expression(an, vars, stmt.cond)
    if etype != nil && !ast.is_type_equal(etype, "Bool") {
        fmt.println(etype)
        append(&an.errors, Error { "Conditional expression does not return 'Bool'", "" })
    }

    for s in stmt.block.stmts {
        if s != nil do tc_statement(an, vars, s, func_id)
    }
    
    if stmt.elif_stmt != nil {
        tc_if_stmt(an, vars, stmt.elif_stmt, func_id)
    } else {
        for s in stmt.else_block.stmts {
            if s != nil do tc_statement(an, vars, s, func_id)
        }
    }
}

@private
tc_while_stmt :: proc(an: ^Analyser, vars: ^Vars, stmt: ^ast.While_Stmt, func_id: string) {
    etype := tc_expression(an, vars, stmt.cond)
    if etype != nil && !ast.is_type_equal(etype, "Bool") {
        fmt.println(etype)
        append(&an.errors, Error { "Conditional expression does not return 'Bool'", "" })
    }
    
    for s in stmt.block.stmts {
        if s != nil do tc_statement(an, vars, s, func_id)
    }
}

@private
tc_print_stmt :: proc(an: ^Analyser, vars: ^Vars, stmt: ^ast.Print_Stmt) {
    tc_expression(an, vars, stmt.expr)
}

@private
tc_return_stmt :: proc(an: ^Analyser, vars: ^Vars, stmt:  ^ast.Return_Stmt, func_id: string) {
    func := an.functions[func_id]
    etype := tc_expression(an, vars, stmt.expr)
    
    if etype != nil && !ast.is_type_equal(func.return_type, etype) {
        append(&an.errors, Error { "return statement does not match function return type", "" })
    }
}

@private
tc_variable_decl_stmt :: proc(an: ^Analyser, vars: ^Vars, stmt: ^ast.Variable_Decl_Stmt) {
    vars[stmt.id] = stmt.type
    
    etype := tc_expression(an, vars, stmt.expr)
    
    if etype != nil && !ast.is_type_equal(etype, stmt.type) {
        append(&an.errors, Error { "Mismatched type", stmt.id })
    }
}

@private
tc_expression :: proc(an: ^Analyser, vars: ^Vars, expr: ^ast.Expression) -> ast.Type {
    switch e in expr {
        case ^ast.Primary_Expr: return tc_primary_expr(an, vars, e)
        case ^ast.Unary_Expr: return tc_unary_expr(an, vars, e)
        case ^ast.Binary_Expr: return tc_binary_expr(an, vars, e)
    }

    return "Unreachable code"
}

@private
tc_binary_expr :: proc(an: ^Analyser, vars: ^Vars, expr: ^ast.Binary_Expr) -> ast.Type {
    ltype := tc_expression(an, vars, expr.lhs)
    rtype := tc_expression(an, vars, expr.rhs)
    
    switch expr.op {
        case .Or, .And: {
            b1 := ast.is_type_equal(ltype, "Bool")
            b2 := ast.is_type_equal(rtype, "Bool")
            
            if !b1 || !b2 {
                append(&an.errors, Error { "'or' and 'and' only work with 'Bool'", "" })
            } else {
                return "Bool"
            }
        }
        
        case .Eq, .Neq: {
            if !ast.is_type_equal(ltype, rtype) {
                append(&an.errors, Error { "'==' and '!=' need the same type on both sides", "" })
            } else {
                return "Bool"
            }
        }

        case .Gt, .Lt, .Gt_Eq, .Lt_Eq: {
            f1 := ast.is_type_equal(ltype, "Float")
            f2 := ast.is_type_equal(rtype, "Float")
            
            i1 := ast.is_type_equal(ltype, "Int")
            i2 := ast.is_type_equal(rtype, "Int")

            n1 := f1 || i1
            n2 := f2 || i2
            
            if !n1 || !n2 {
                append(&an.errors, Error { "'>', '<', '>=', '<=', '-', '*', '/' and '%' only work on 'Float' and 'Int'", "" })
            } else {
                return "Bool"
            }
        }
        
        case .Sub, .Mul, .Div, .Mod: {
            f1 := ast.is_type_equal(ltype, "Float")
            f2 := ast.is_type_equal(rtype, "Float")

            i1 := ast.is_type_equal(ltype, "Int")
            i2 := ast.is_type_equal(rtype, "Int")

            n1 := f1 || i1
            n2 := f2 || i2
            
            if !n1 || !n2 {
                append(&an.errors, Error { "'>', '<', '>=', '<=', '-', '*', '/' and '%' only work on 'Float' and 'Int'", "" })
            } else {
                if f1 || f2 {
                    return "Float"
                } else {
                    return "Int"
                }
            }
        }

        case .Add: {
            f1 := ast.is_type_equal(ltype, "Float")
            f2 := ast.is_type_equal(rtype, "Float")

            i1 := ast.is_type_equal(ltype, "Int")
            i2 := ast.is_type_equal(rtype, "Int")
            
            n1 := f1 || i1
            n2 := f2 || i2

            if !n1 || !n2 {
                if ast.is_type_equal(ltype, "String") && ast.is_type_equal(rtype, "String") {
                    return "String"
                } else {
                    if !ast.is_array_type(ltype) && !ast.is_array_type(rtype) {
                        append(&an.errors, Error { "'+' only works with 'String', 'Array', 'Int' and 'Float'", "" })
                    } else if !ast.is_type_equal(ltype, rtype) {
                        append(&an.errors, Error { "Arrays are different types", "" })
                    } else {
                        return ltype
                    }
                }
            } else {
                if f1 || f2 {
                    return "Float"
                } else {
                    return "Int"
                }
            }
        }
        
        case .Index: {
            if ast.is_type_equal(ltype, "String") || ast.is_array_type(ltype) {
                if ast.is_type_equal(rtype, "Int") {
                    return ast.get_array_type_internal(ltype)
                } else {
                    append(&an.errors, Error { "You can only use 'Int' to index a collection", "" })
                }
            } else {
                append(&an.errors, Error { "You can only index Strings and Arrays", "" })
            }
        }
    }

    return nil
}

@private
tc_unary_expr :: proc(an: ^Analyser, vars: ^Vars, expr: ^ast.Unary_Expr) -> ast.Type {
    etype := tc_expression(an, vars, expr.expr)
    
    if expr.op == .Negation {
        if !ast.is_type_equal(etype, "Int") && !ast.is_type_equal(etype, "Float") {
            append(&an.errors, Error { "'-' only works with 'Int' or 'Float'", "" })
            return nil
        }
    } else {
        if !ast.is_type_equal(etype, "Bool") {
            append(&an.errors, Error { "'not' only works with 'Bool'", "" })
            return nil
        }
    }
    
    return etype
}

@private
tc_primary_expr :: proc(an: ^Analyser, vars: ^Vars, expr: ^ast.Primary_Expr) -> ast.Type {
    #partial switch v in expr {
        case ast.Number: {
            switch n in v {
                case i64: return "Int"
                case f64: return "Float"
            }
        }
        
        case string: return "String"
        case bool: return "Bool"
        
        case ast.Identifier: {
            if string(v) in vars {
                var := vars[string(v)]
                return var
            } else {
                append(&an.errors, Error { "Variable does not exist", string(v) })
            }
        }
        
        case ^ast.Array_Literal: {
            missmatch := false
            first_loop := true
            last_type: ast.Type = nil

            for e in v {
                lit_type := tc_expression(an, vars, e)
                if first_loop {
                    first_loop = false
                    last_type = lit_type
                } else if !missmatch && lit_type != nil {
                    if !ast.is_type_equal(last_type, lit_type) {
                        missmatch = true
                    }
                }
            }
            
            if missmatch {
                append(&an.errors, Error { "Multiple types in array literal", "" })
                return nil
            }

            array_type := new(ast.Type, context.temp_allocator)
            array_type^ = last_type

            return array_type
        }
        
        case ^ast.Expression: return tc_expression(an, vars, v)

        case ^ast.Function_Call: {
            if v.id in an.functions {
                func := an.functions[v.id]

                if len(v.params) > len(func.params) {
                    append(&an.errors, Error { "Too many parameters passed into function", func.id })
                } else {
                    for i in 0 ..< len(v.params) {
                        etype := tc_expression(an, vars, v.params[i])
                        if !ast.is_type_equal(etype, func.params[i].type) {
                            append(&an.errors, Error { "Invalid type for function call", func.id })
                        } 
                    }
                }

                return func.return_type
            } else {
                append(&an.errors, Error { "Function does not exist", v.id })
            }
        }
    }

    return nil
}