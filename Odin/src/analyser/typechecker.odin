package analyser

import "core:fmt"

import "../ast"
import s "../shared"

@private
typecheck :: proc(an: ^Analyser) {
    for _, f in an.functions {
        table := new_var_table()
        
        for p in f.params {
            define_var(&table, p.id, p.param_type)
        }

        for s in f.block.stmts {
            tc_statement(an, &table, s, f.id)
        }
    }
}

@private
tc_statement :: proc(an: ^Analyser, table: ^Var_Table, stmt: ^ast.Statement, func_id: string) {
    switch s in stmt {
        case ^ast.Variable_Decl_Stmt: tc_variable_decl_stmt(an, table, s)
        case ^ast.Return_Stmt: tc_return_stmt(an, table, s, func_id)
        case ^ast.Print_Stmt: tc_print_stmt(an, table, s)
        case ^ast.While_Stmt: tc_while_stmt(an, table, s, func_id)
        case ^ast.If_Stmt: tc_if_stmt(an, table, s, func_id)
        case ^ast.Assignment_Stmt: tc_assignment_stmt(an, table, s)
        case ^ast.Index_Assignment_Stmt: tc_index_assignment_stmt(an, table, s)
        case ^ast.Raw_Expr_Stmt: tc_raw_expr_stmt(an, table, s)
    }
}

@private
tc_raw_expr_stmt :: proc(an: ^Analyser, table: ^Var_Table, stmt: ^ast.Raw_Expr_Stmt) {
    tc_expression(an, table, stmt.expr)
}

@private
tc_index_assignment_stmt :: proc(an: ^Analyser, table: ^Var_Table, stmt: ^ast.Index_Assignment_Stmt) {
    var := ast.get_array_type_internal(get_var(table, stmt.id))
    itype := tc_expression(an, table, stmt.index)
    etype := tc_expression(an, table, stmt.expr)
    
    if itype != nil && !ast.is_type_equal(itype, ast.INT_TYPE) {
        s.append_error(&an.errors, stmt.info, "Indexes can only be of type 'Int'")
    }

    if etype != nil && !ast.is_type_equal(etype, var) {
        s.append_error(&an.errors, stmt.info, "Variable '%s' doesn't match the assignment expression type", stmt.id)
    }
}

@private
tc_assignment_stmt :: proc(an: ^Analyser, table: ^Var_Table, stmt: ^ast.Assignment_Stmt) {
    var := get_var(table, stmt.id)
    etype := tc_expression(an, table, stmt.expr)
    
    if etype != nil && !ast.is_type_equal(var, etype) {
        s.append_error(&an.errors, stmt.info, "Variable '%s' doesn't match the assignment expression type", stmt.id)
    }
}

@private
tc_if_stmt :: proc(an: ^Analyser, parent: ^Var_Table, stmt: ^ast.If_Stmt, func_id: string) {
    etype := tc_expression(an, parent, stmt.cond)
    if etype != nil && !ast.is_type_equal(etype, ast.BOOL_TYPE) {
        s.append_error(&an.errors, stmt.info, "'if' conditional must be of type 'Bool'")
    }

    if_table := new_var_table(parent)
    for s in stmt.block.stmts {
        tc_statement(an, &if_table, s, func_id)
    }
    
    if stmt.elif_stmt != nil {
        tc_if_stmt(an, parent, stmt.elif_stmt, func_id)
    } else if stmt.else_block != nil {
        else_table := new_var_table(parent)
        for s in stmt.else_block.stmts {
            tc_statement(an, &else_table, s, func_id)
        }
    }
}

@private
tc_while_stmt :: proc(an: ^Analyser, parent: ^Var_Table, stmt: ^ast.While_Stmt, func_id: string) {
    etype := tc_expression(an, parent, stmt.cond)
    if etype != nil && !ast.is_type_equal(etype, ast.BOOL_TYPE) {
        fmt.println(etype)
        s.append_error(&an.errors, stmt.info, "'while' conditional must be of type 'Bool'")
    }
    
    table := new_var_table(parent)
    for s in stmt.block.stmts {
        tc_statement(an, &table, s, func_id)
    }
}

@private
tc_print_stmt :: proc(an: ^Analyser, table: ^Var_Table, stmt: ^ast.Print_Stmt) {
    tc_expression(an, table, stmt.expr)
}

@private
tc_return_stmt :: proc(an: ^Analyser, table: ^Var_Table, stmt:  ^ast.Return_Stmt, func_id: string) {
    func := an.functions[func_id]

    if func.return_type == nil {
        s.append_error(&an.errors, func.info, "Function '%s' shouldn't have a 'return' statment")
    } else {
        etype := tc_expression(an, table, stmt.expr)
        if etype != nil && !ast.is_type_equal(func.return_type, etype) {
            s.append_error(&an.errors, stmt.info, "'return' doesn't match the function '%s' return type", func.id)
        }
    }        
}

@private
tc_variable_decl_stmt :: proc(an: ^Analyser, table: ^Var_Table, stmt: ^ast.Variable_Decl_Stmt) {
    etype := tc_expression(an, table, stmt.expr)
    
    if etype != nil && !ast.is_type_equal(etype, stmt.var_type) {
        s.append_error(&an.errors, stmt.info, "Variable '%s' doesn't match the assignment expression type", stmt.id)
    }

    define_var(table, stmt.id, stmt.var_type)
}

@private
tc_expression :: proc(an: ^Analyser, table: ^Var_Table, expr: ^ast.Expression) -> ast.Type {
    switch e in expr {
        case ^ast.Primary_Expr: return tc_primary_expr(an, table, e)
        case ^ast.Unary_Expr: return tc_unary_expr(an, table, e)
        case ^ast.Binary_Expr: return tc_binary_expr(an, table, e)
    }
    
    return nil
}

@private
tc_binary_expr :: proc(an: ^Analyser, table: ^Var_Table, expr: ^ast.Binary_Expr) -> ast.Type {
    ltype := tc_expression(an, table, expr.lhs)
    rtype := tc_expression(an, table, expr.rhs)
    
    if ltype == nil || rtype == nil do return nil
    
    switch expr.op {
        case .Or, .And: {
            b1 := ast.is_type_equal(ltype, ast.BOOL_TYPE)
            b2 := ast.is_type_equal(rtype, ast.BOOL_TYPE)
            
            if !b1 || !b2 {
                op := "or" if expr.op == .Or else "and"
                s.append_error(&an.errors, expr.info, "'%s' requires operands of type 'Bool'", op)
            } else {
                return ast.BOOL_TYPE
            }
        }

        case .Eq, .Neq: {
            f1 := ast.is_type_equal(ltype, ast.FLOAT_TYPE)
            f2 := ast.is_type_equal(rtype, ast.FLOAT_TYPE)
            
            i1 := ast.is_type_equal(ltype, ast.INT_TYPE)
            i2 := ast.is_type_equal(rtype, ast.INT_TYPE)

            n1 := f1 || i1
            n2 := f2 || i2

            is_number := n1 && n2

            if is_number || ast.is_type_equal(ltype, rtype) {
                return ast.BOOL_TYPE
            } else {
                op := "==" if expr.op == .Eq else "!="
                s.append_error(&an.errors, expr.info, "'%s' requires operands of the same type", op)
            }
        }

        case .Gt, .Lt, .Gt_Eq, .Lt_Eq: {
            f1 := ast.is_type_equal(ltype, ast.FLOAT_TYPE)
            f2 := ast.is_type_equal(rtype, ast.FLOAT_TYPE)
            
            i1 := ast.is_type_equal(ltype, ast.INT_TYPE)
            i2 := ast.is_type_equal(rtype, ast.INT_TYPE)

            n1 := f1 || i1
            n2 := f2 || i2
            
            if !n1 || !n2 {
                op := ">"
                if expr.op == .Lt do op = "<"
                else if expr.op == .Gt_Eq do op = ">="
                else if expr.op == .Lt_Eq do op = "<="
                
                s.append_error(&an.errors, expr.info, "'%s' requires operands either of type 'Float' or 'Int'", op)
            } else {
                return ast.BOOL_TYPE
            }
        }
        
        case .Sub, .Mul, .Div: {
            f1 := ast.is_type_equal(ltype, ast.FLOAT_TYPE)
            f2 := ast.is_type_equal(rtype, ast.FLOAT_TYPE)
            
            i1 := ast.is_type_equal(ltype, ast.INT_TYPE)
            i2 := ast.is_type_equal(rtype, ast.INT_TYPE)

            n1 := f1 || i1
            n2 := f2 || i2
            
            if !n1 || !n2 {
                op := "-"
                if expr.op == .Mul do op = "*"
                else if expr.op == .Div do op = "/"

                s.append_error(&an.errors, expr.info, "'%s' requires operands either of type 'Float' or 'Int'", op)
            } else {
                if f1 || f2 {
                    return ast.FLOAT_TYPE
                } else {
                    return ast.INT_TYPE
                }
            }
        }
        
        case .Mod: {
            if ast.is_type_equal(ltype, ast.INT_TYPE) && ast.is_type_equal(rtype, ast.INT_TYPE) {
                return ast.INT_TYPE
            } else {
                s.append_error(&an.errors, expr.info, "'%%' requires operands of type 'Int'")
            }
        }

        case .Add: {
            f1 := ast.is_type_equal(ltype, ast.FLOAT_TYPE)
            f2 := ast.is_type_equal(rtype, ast.FLOAT_TYPE)
            
            i1 := ast.is_type_equal(ltype, ast.INT_TYPE)
            i2 := ast.is_type_equal(rtype, ast.INT_TYPE)
            
            n1 := f1 || i1
            n2 := f2 || i2

            if !n1 || !n2 {
                if ast.is_type_equal(ltype, ast.STRING_TYPE) && ast.is_type_equal(rtype, ast.STRING_TYPE) {
                    return ast.STRING_TYPE
                } else {
                    if !ast.is_array_type(ltype) && !ast.is_array_type(rtype) {
                        s.append_error(&an.errors, expr.info, "'+' requires operands of type 'String', 'Array', 'Int' or 'Float'")
                    } else if !ast.is_type_equal(ltype, rtype) {
                        s.append_error(&an.errors, expr.info, "Arrays contain different types")
                    } else {
                        return ltype
                    }
                }
            } else {
                if f1 || f2 {
                    return ast.FLOAT_TYPE
                } else {
                    return ast.INT_TYPE
                }
            }
        }
        
        case .Index: {
            if ast.is_array_type(ltype) {
                if ast.is_type_equal(rtype, ast.INT_TYPE) {
                    return ast.get_array_type_internal(ltype)
                } else {
                    s.append_error(&an.errors, expr.info, "Index must be of type 'Int'")
                }
            } else {
                s.append_error(&an.errors, expr.info, "Attempt to index data that is not an array")
            }
        }
    }

    return nil
}

@private
tc_unary_expr :: proc(an: ^Analyser, table: ^Var_Table, expr: ^ast.Unary_Expr) -> ast.Type {
    etype := tc_expression(an, table, expr.expr)
    if etype == nil do return nil
    
    if expr.op == .Negation {
        if !ast.is_type_equal(etype, ast.INT_TYPE) && !ast.is_type_equal(etype, ast.FLOAT_TYPE) {
            s.append_error(&an.errors, expr.info, "'-' requires both operands to be of type 'Float' or 'Int'")
            return nil
        }
    } else {
        if !ast.is_type_equal(etype, ast.BOOL_TYPE) {
            s.append_error(&an.errors, expr.info, "'not' requires both operands to be of type 'Bool'")
            return nil
        }
    }
    
    return etype
}

@private
tc_primary_expr :: proc(an: ^Analyser, table: ^Var_Table, expr: ^ast.Primary_Expr) -> ast.Type {
    switch v in expr {
        case ast.Int_Lit: return ast.INT_TYPE
        case ast.Float_Lit: return ast.FLOAT_TYPE
        case ast.String_Lit: return ast.STRING_TYPE
        case ast.Bool_Lit: return ast.BOOL_TYPE
        
        case ast.Identifier: {
            if is_var_defined(table, v.value) {
                return get_var(table, v.value)
            } else {
                s.append_error(&an.errors, v.info, "Variable '%s' doesn't exist", v.value)
            }
        }
        
        case ^ast.Array_Lit: {
            first_type: ast.Type = nil

            result := ast.Array_Type {}
            result.nesting = 1

            for e in v.values {
                lit_type := tc_expression(an, table, e)

                if lit_type == nil do return nil
                
                if first_type == nil {
                    first_type = lit_type

                    switch t in lit_type {
                        case ast.Base_Type: result.internal = t
                        case ast.Array_Type: {
                            result.nesting += t.nesting
                            result.internal = t.internal
                        }
                    }
                }
                
                if !ast.is_type_equal(first_type, lit_type) {
                    s.append_error(&an.errors, v.info, "Multiple different types in array literal")
                    return nil
                }
            }
            
            return result
        }
        
        case ^ast.Expression: return tc_expression(an, table, v)

        case ^ast.Function_Call: {
            if v.id in an.functions {
                func := an.functions[v.id]

                if len(v.params) > len(func.params) {
                    s.append_error(&an.errors, v.info, "Too many arguments passed into function '%s'", v.id)
                } else if len(v.params) < len(func.params) {
                    s.append_error(&an.errors, v.info, "Too few arguments passed into function '%s'", v.id)
                } else {
                    for i in 0 ..< len(v.params) {
                        etype := tc_expression(an, table, v.params[i])
                        if !ast.is_type_equal(etype, func.params[i].param_type) {
                            s.append_error(&an.errors, v.info, "Type mismatch with with argument %d", i + 1)
                        }
                    }
                }

                return func.return_type
            } else if v.id in an.builtins {
                func := an.builtins[v.id]

                if len(v.params) > len(func.params) {
                    s.append_error(&an.errors, v.info, "Too many arguments passed into function '%s'", v.id)
                } else if len(v.params) < len(func.params) {
                    s.append_error(&an.errors, v.info, "Too few arguments passed into function '%s'", v.id)
                } else {
                    for i in 0 ..< len(v.params) {
                        etype := tc_expression(an, table, v.params[i])
                        if !ast.is_type_equal(etype, func.params[i]) {
                            s.append_error(&an.errors, v.info, "Type mismatch with with argument %d", i + 1)
                        }
                    }
                }

                return func.return_type

            } else {
                s.append_error(&an.errors, v.info, "Function '%s' doesn't exist", v.id)
            }
        }
    }

    return nil
}
