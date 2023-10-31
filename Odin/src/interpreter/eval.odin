// TODO: Break this into multiple files
package interpreter

import "core:strings"
import "core:fmt"

import "../ast"

Error_Code :: i64

// TODO: Rename to Environment
Interpreter :: struct {
    functions: map[string]^ast.Function_Decl
}

Scope :: struct {
    parent: ^Scope,
    variables: map[string]Value,
    references: [dynamic]Value,
}

// Used for upcasting of Int to Float
Value_Type :: enum {
    Int, Float
}

Value :: union {
    Int_Value,
    Float_Value,
    Bool_Value,
    ^String_Value,
    ^Array_Value,
}

Int_Value :: struct {
    value: i64
}

Float_Value :: struct {
    value: f64
}

Bool_Value :: struct {
    value: bool
}

String_Value :: struct {
    buf: strings.Builder,
    rc: int,
}

Array_Value :: struct {
    buf: [dynamic]Value,
    rc: int,
}

eval :: proc(tree: ^ast.Ast) -> Error_Code {
    env := Interpreter {}
    env.functions = make(map[string]^ast.Function_Decl, allocator = context.temp_allocator)
    defer free_all(context.temp_allocator)
    
    for f in tree.functions {
        env.functions[f.id] = f
    }
    
    entry_point := env.functions["main"]
    ret := eval_function(&env, entry_point, nil).(Int_Value)

    return ret.value
}

@private
eval_function :: proc(env: ^Interpreter, f: ^ast.Function_Decl, params: Maybe([]Value)) -> Value {
    scope := Scope {}
    defer delete_scope(scope)
    
    if params != nil {
        p := params.([]Value)
        for i in 0..<len(f.params) {
            scope.variables[f.params[i].id] = p[i]
            increment_reference(p[i])
            append_reference(&scope, p[i])
        }
    }

    ret := eval_block(env, &scope, f.block)
    
    drop_references(&scope)

    return ret
}

// Returns non-nil if a return statement was called
@private
eval_block :: proc(env: ^Interpreter, parent: ^Scope, b: ^ast.Block) -> Value {
    // TODO: Make this so we only create a scope when needed not when we enter a function
    scope := Scope { parent = parent }
    defer delete_scope(scope)
    
    ret: Value = nil

    for s in b.stmts {
        switch v in s {
            case ^ast.Variable_Decl_Stmt: eval_variable_decl_stmt(env, &scope, v)
            case ^ast.Return_Stmt: ret = eval_return_stmt(env, &scope, v)
            case ^ast.Print_Stmt: eval_print_stmt(env, &scope, v)
            case ^ast.While_Stmt: ret = eval_while_stmt(env, &scope, v)
            case ^ast.If_Stmt: ret = eval_if_stmt(env, &scope, v)
            case ^ast.Assignment_Stmt: eval_assignment_stmt(env, &scope, v)
            case ^ast.Index_Assignment_Stmt: eval_index_assignment_stmt(env, &scope, v)
        }

        if ret != nil do break
    }
    
    drop_references(&scope)
    
    return ret
}

@private
eval_variable_decl_stmt :: proc(env: ^Interpreter, scope: ^Scope, s: ^ast.Variable_Decl_Stmt) {
    create_variable(scope, s.id, eval_expression(env, scope, s.expr))
}

// NOTE: There is a small bug with when a function returns its reference parameter. It will create two references in the array. 
//       It still gets freed like normal but it takes a bit more memory in the array to store which I think is fine.
@private
eval_return_stmt :: proc(env: ^Interpreter, scope: ^Scope, s: ^ast.Return_Stmt) -> Value {
    res := eval_expression(env, scope, s.expr)
    increment_reference(res)
    return res
}

@private
eval_print_stmt :: proc(env: ^Interpreter, scope: ^Scope, s: ^ast.Print_Stmt) {
    print_value(eval_expression(env, scope, s.expr))
}

@private
eval_while_stmt :: proc(env: ^Interpreter, scope: ^Scope, s: ^ast.While_Stmt) -> Value {
    condition := eval_expression(env, scope, s.cond).(Bool_Value)
    
    for condition.value {
        ret := eval_block(env, scope, s.block)
        if ret != nil do return ret
            
        condition = eval_expression(env, scope, s.cond).(Bool_Value)
    }
    
    return nil
}

@private
eval_if_stmt :: proc(env: ^Interpreter, scope: ^Scope, s: ^ast.If_Stmt) -> Value {
    condition := eval_expression(env, scope, s.cond).(Bool_Value)

    if condition.value {
        return eval_block(env, scope, s.block)
    } else if s.elif_stmt != nil {
        return eval_if_stmt(env, scope, s.elif_stmt)
    } else if s.else_block != nil {
        return eval_block(env, scope, s.else_block)
    }
    
    return nil
}

@private
eval_assignment_stmt :: proc(env: ^Interpreter, scope: ^Scope, s: ^ast.Assignment_Stmt) {
    set_variable(scope, s.id, eval_expression(env, scope, s.expr))
}

@private
eval_index_assignment_stmt :: proc(env: ^Interpreter, scope: ^Scope, s: ^ast.Index_Assignment_Stmt) {
    index := eval_expression(env, scope, s.index).(Int_Value)
    value := eval_expression(env, scope, s.expr)
    set_array_value(scope, s.id, index.value, value)
}

@private
eval_expression :: proc(env: ^Interpreter, scope: ^Scope, e: ^ast.Expression, rc_start_state: int = 1) -> Value {
    switch v in e {
        case ^ast.Primary_Expr: return eval_primary_expr(env, scope, v, rc_start_state)
        case ^ast.Binary_Expr: return eval_binary_expr(env, scope, v)
        case ^ast.Unary_Expr: return eval_unary_expr(env, scope, v)
    }
    
    return nil
}

@private
eval_unary_expr :: proc(env: ^Interpreter, scope: ^Scope, e: ^ast.Unary_Expr) -> Value {
    switch e.op {
        case .Negation: {
            value := eval_expression(env, scope, e.expr)
            #partial switch v in value {
                case Int_Value: return Int_Value { -v.value }
                case Float_Value: return Float_Value { -v.value }
            }
        }
        
        case .Not: {
            value := eval_expression(env, scope, e.expr).(Bool_Value)
            return Bool_Value { !value.value }
        }
    }

    return nil
}

@private
eval_binary_expr :: proc(env: ^Interpreter, scope: ^Scope, e: ^ast.Binary_Expr) -> Value {
    #partial switch e.op {
        case .Or: {
            lhs := eval_expression(env, scope, e.lhs).(Bool_Value)
            rhs := eval_expression(env, scope, e.rhs).(Bool_Value)
            return Bool_Value { lhs.value || rhs.value }
        }
        
        case .And: {
            lhs := eval_expression(env, scope, e.lhs).(Bool_Value)
            rhs := eval_expression(env, scope, e.rhs).(Bool_Value)
            return Bool_Value { lhs.value && rhs.value }
        }
        
        case .Eq: {
            lhs := eval_expression(env, scope, e.lhs)
            rhs := eval_expression(env, scope, e.rhs)
            return compare_values(lhs, rhs)
        }
        
        case .Neq: {
            lhs := eval_expression(env, scope, e.lhs)
            rhs := eval_expression(env, scope, e.rhs)
            return Bool_Value { !compare_values(lhs, rhs).value }
        }
        
        case .Gt: {
            lhs := eval_expression(env, scope, e.lhs)
            rhs := eval_expression(env, scope, e.rhs)
            
            tl := get_value_type(lhs)
            tr := get_value_type(rhs)
            
            if tl == tr {
                switch tl {
                    case .Int: {
                        v1 := lhs.(Int_Value)
                        v2 := rhs.(Int_Value)
                        return Bool_Value { v1.value > v2.value }
                    }
                    
                    case .Float: {
                        v1 := lhs.(Float_Value)
                        v2 := rhs.(Float_Value)
                        return Bool_Value { v1.value > v2.value }
                    }
                }
            } else {
                if tl == .Float {
                    v1 := lhs.(Float_Value)
                    v2 := rhs.(Int_Value)
                    return Bool_Value { v1.value > f64(v2.value) }
                } else if tr == .Float {
                    v1 := lhs.(Int_Value)
                    v2 := rhs.(Float_Value)
                    return Bool_Value { f64(v1.value) > v2.value }
                }
            }
        }
        
        case .Lt: {
            lhs := eval_expression(env, scope, e.lhs)
            rhs := eval_expression(env, scope, e.rhs)
            
            tl := get_value_type(lhs)
            tr := get_value_type(rhs)
            
            if tl == tr {
                switch tl {
                    case .Int: {
                        v1 := lhs.(Int_Value)
                        v2 := rhs.(Int_Value)
                        return Bool_Value { v1.value < v2.value }
                    }
                    
                    case .Float: {
                        v1 := lhs.(Float_Value)
                        v2 := rhs.(Float_Value)
                        return Bool_Value { v1.value < v2.value }
                    }
                }
            } else {
                if tl == .Float {
                    v1 := lhs.(Float_Value)
                    v2 := rhs.(Int_Value)
                    return Bool_Value { v1.value < f64(v2.value) }
                } else if tr == .Float {
                    v1 := lhs.(Int_Value)
                    v2 := rhs.(Float_Value)
                    return Bool_Value { f64(v1.value) < v2.value }
                }
            }
        }
        
        case .Gt_Eq: {
            lhs := eval_expression(env, scope, e.lhs)
            rhs := eval_expression(env, scope, e.rhs)
            
            tl := get_value_type(lhs)
            tr := get_value_type(rhs)
            
            if tl == tr {
                switch tl {
                    case .Int: {
                        v1 := lhs.(Int_Value)
                        v2 := rhs.(Int_Value)
                        return Bool_Value { v1.value >= v2.value }
                    }
                    
                    case .Float: {
                        v1 := lhs.(Float_Value)
                        v2 := rhs.(Float_Value)
                        return Bool_Value { v1.value >= v2.value }
                    }
                }
            } else {
                if tl == .Float {
                    v1 := lhs.(Float_Value)
                    v2 := rhs.(Int_Value)
                    return Bool_Value { v1.value >= f64(v2.value) }
                } else if tr == .Float {
                    v1 := lhs.(Int_Value)
                    v2 := rhs.(Float_Value)
                    return Bool_Value { f64(v1.value) >= v2.value }
                }
            }
        }
        
        case .Lt_Eq: {
            lhs := eval_expression(env, scope, e.lhs)
            rhs := eval_expression(env, scope, e.rhs)
            
            tl := get_value_type(lhs)
            tr := get_value_type(rhs)
            
            if tl == tr {
                switch tl {
                    case .Int: {
                        v1 := lhs.(Int_Value)
                        v2 := rhs.(Int_Value)
                        return Bool_Value { v1.value <= v2.value }
                    }
                    
                    case .Float: {
                        v1 := lhs.(Float_Value)
                        v2 := rhs.(Float_Value)
                        return Bool_Value { v1.value <= v2.value }
                    }
                }
            } else {
                if tl == .Float {
                    v1 := lhs.(Float_Value)
                    v2 := rhs.(Int_Value)
                    return Bool_Value { v1.value <= f64(v2.value) }
                } else if tr == .Float {
                    v1 := lhs.(Int_Value)
                    v2 := rhs.(Float_Value)
                    return Bool_Value { f64(v1.value) <= v2.value }
                }
            }
        }
        
        case .Add: {
            lhs := eval_expression(env, scope, e.lhs)
            rhs := eval_expression(env, scope, e.rhs)

            #partial switch v in lhs {
                case ^String_Value: {
                    r := rhs.(^String_Value)
                    new_str := new(String_Value)
                    new_str.rc = 1
                    strings.builder_init(&new_str.buf)
                    strings.write_string(&new_str.buf, strings.to_string(v.buf))
                    strings.write_string(&new_str.buf, strings.to_string(r.buf))
                    
                    append_reference(scope, new_str)
                    return new_str
                }

                case ^Array_Value: {
                    r := rhs.(^Array_Value)
                    new_arr := new(Array_Value)
                    new_arr.rc = 1
                    
                    for elem in v.buf {
                        append(&new_arr.buf, elem)
                    }
                    
                    for elem in r.buf {
                        append(&new_arr.buf, elem)
                    }
                    
                    append_reference(scope, new_arr)
                    return new_arr
                }
            }
            
            tl := get_value_type(lhs)
            tr := get_value_type(rhs)
            
            if tl == tr {
                switch tl {
                    case .Int: {
                        v1 := lhs.(Int_Value)
                        v2 := rhs.(Int_Value)
                        return Int_Value { v1.value + v2.value }
                    }
                    
                    case .Float: {
                        v1 := lhs.(Float_Value)
                        v2 := rhs.(Float_Value)
                        return Float_Value { v1.value + v2.value }
                    }
                }
            } else {
                if tl == .Float {
                    v1 := lhs.(Float_Value)
                    v2 := rhs.(Int_Value)
                    return Float_Value { v1.value + f64(v2.value) }
                } else if tr == .Float {
                    v1 := lhs.(Int_Value)
                    v2 := rhs.(Float_Value)
                    return Float_Value { f64(v1.value) + v2.value }
                }
            }
        }
        
        case .Sub: {
            lhs := eval_expression(env, scope, e.lhs)
            rhs := eval_expression(env, scope, e.rhs)

            tl := get_value_type(lhs)
            tr := get_value_type(rhs)

            if tl == tr {
                switch tl {
                    case .Int: {
                        v1 := lhs.(Int_Value)
                        v2 := rhs.(Int_Value)
                        return Int_Value { v1.value - v2.value }
                    }
                    
                    case .Float: {
                        v1 := lhs.(Float_Value)
                        v2 := rhs.(Float_Value)
                        return Float_Value { v1.value - v2.value }
                    }
                }
            } else {
                if tl == .Float {
                    v1 := lhs.(Float_Value)
                    v2 := rhs.(Int_Value)
                    return Float_Value { v1.value - f64(v2.value) }
                } else if tr == .Float {
                    v1 := lhs.(Int_Value)
                    v2 := rhs.(Float_Value)
                    return Float_Value { f64(v1.value) - v2.value }
                }
            }
        }
        
        case .Mul: {
            lhs := eval_expression(env, scope, e.lhs)
            rhs := eval_expression(env, scope, e.rhs)
            
            tl := get_value_type(lhs)
            tr := get_value_type(rhs)
            
            if tl == tr {
                switch tl {
                    case .Int: {
                        v1 := lhs.(Int_Value)
                        v2 := rhs.(Int_Value)
                        return Int_Value { v1.value * v2.value }
                    }
                    
                    case .Float: {
                        v1 := lhs.(Float_Value)
                        v2 := rhs.(Float_Value)
                        return Float_Value { v1.value * v2.value }
                    }
                }
            } else {
                if tl == .Float {
                    v1 := lhs.(Float_Value)
                    v2 := rhs.(Int_Value)
                    return Float_Value { v1.value * f64(v2.value) }
                } else if tr == .Float {
                    v1 := lhs.(Int_Value)
                    v2 := rhs.(Float_Value)
                    return Float_Value { f64(v1.value) * v2.value }
                }
            }
        }
        
        case .Div: {
            lhs := eval_expression(env, scope, e.lhs)
            rhs := eval_expression(env, scope, e.rhs)
            
            tl := get_value_type(lhs)
            tr := get_value_type(rhs)
            
            if tl == tr {
                switch tl {
                    case .Int: {
                        v1 := lhs.(Int_Value)
                        v2 := rhs.(Int_Value)
                        return Int_Value { v1.value / v2.value }
                    }
                    
                    case .Float: {
                        v1 := lhs.(Float_Value)
                        v2 := rhs.(Float_Value)
                        return Float_Value { v1.value / v2.value }
                    }
                }
            } else {
                if tl == .Float {
                    v1 := lhs.(Float_Value)
                    v2 := rhs.(Int_Value)
                    return Float_Value { v1.value / f64(v2.value) }
                } else if tr == .Float {
                    v1 := lhs.(Int_Value)
                    v2 := rhs.(Float_Value)
                    return Float_Value { f64(v1.value) / v2.value }
                }
            }
        }
        
        case .Mod: {
            lhs := eval_expression(env, scope, e.lhs)
            rhs := eval_expression(env, scope, e.rhs)
            
            v1 := lhs.(Int_Value)
            v2 := rhs.(Int_Value)
            return Int_Value { v1.value % v2.value }
        }
        
        case .Index: {
            array := eval_expression(env, scope, e.lhs).(^Array_Value)
            index := eval_expression(env, scope, e.rhs).(Int_Value)
            return array.buf[index.value]
        }
    }

    return nil
}

// rc_start_state is used when passing rc parameters to functions so the called function owns non variable objects
@private
eval_primary_expr :: proc(env: ^Interpreter, scope: ^Scope, e: ^ast.Primary_Expr, rc_start_state: int = 1) -> Value {
    switch v in e {
        case ast.Int_Lit: return Int_Value { v.value }
        case ast.Float_Lit: return Float_Value { v.value }
        case ast.Bool_Lit: return Bool_Value { v.value }
        
        case ast.String_Lit: {
            s := new(String_Value)
            s.rc = rc_start_state
            strings.builder_init(&s.buf)
            strings.write_string(&s.buf, v.value)
            
            if s.rc != 0 do append_reference(scope, s)
            return s
        }

        case ^ast.Array_Lit: {
            arr := new(Array_Value)
            arr.rc = rc_start_state
            
            for l in v.values {
                append(&arr.buf, eval_expression(env, scope, l))
            }

            if arr.rc != 0 do append_reference(scope, arr)
            return arr
        }

        case ast.Identifier: return get_variable(scope, v.value)
        case ^ast.Expression: return eval_expression(env, scope, v)
        
        case ^ast.Function_Call: {
            func := env.functions[v.id]
            params := make([dynamic]Value)
            defer delete(params)

            for p in v.params {
                append(&params, eval_expression(env, scope, p, 0))
            }
            
            res := eval_function(env, func, params[:])
            
            // Add the reference to the calling functions scope if it is a stiring or an array
            append_reference(scope, res)
            return res
        }
    }
    
    return nil
}

@private
delete_scope :: proc(scope: Scope) {
    delete(scope.variables)
    delete(scope.references)
}

@private
drop_references :: proc(scope: ^Scope) {
    for v in scope.references {
        #partial switch var in v {
            case ^String_Value: {
                var.rc -= 1
                //fmt.println("decrementing reference", var.rc,":", strings.to_string(var.buf))
                if var.rc <= 0 {
                    //fmt.println("dropping string:", strings.to_string(var.buf))
                    strings.builder_destroy(&var.buf)
                    free(var)
                }
            }
            
            case ^Array_Value: {
                var.rc -= 1
                //fmt.println("decrementing reference", var.rc,":", var.buf)
                if var.rc <= 0 {
                    //fmt.println("dropping array:", var.buf)
                    delete(var.buf)
                    free(var)
                }
            }
        }
    }   
}

@private
append_reference :: proc(scope: ^Scope, val: Value) {
    #partial switch v in val {
        case ^String_Value: append(&scope.references, v)
        case ^Array_Value: append(&scope.references, v)
    }
}

@private
increment_reference :: proc(val: Value) {
    #partial switch v in val {
        case ^String_Value: {
            v.rc += 1
            //fmt.println("Incremented reference of", v.rc,":", strings.to_string(v.buf))
        }
        
        case ^Array_Value: {
            v.rc += 1
            //fmt.println("Incremented reference of", v.rc,":", v.buf)
        }
    }
}

@private
create_variable :: proc(scope: ^Scope, id: string, value: Value) {
    scope.variables[id] = value
}

@private
get_variable :: proc(scope: ^Scope, id: string) -> Value {
    if !(id in scope.variables) {
        return get_variable(scope.parent, id)
    }

    return scope.variables[id]
}

@private
set_variable :: proc(scope: ^Scope, id: string, value: Value) {
    if !(id in scope.variables) {
        set_variable(scope.parent, id, value)
    }

    scope.variables[id] = value
}

@private
set_array_value :: proc(scope: ^Scope, id: string, index: i64, value: Value) {
    if !(id in scope.variables) {
        set_array_value(scope.parent, id, index, value)
        return
    }

    arr := scope.variables[id].(^Array_Value)
    arr.buf[index] = value
}

@private
print_value :: proc(value: Value, newline := true) {
    switch v in value {
        case Int_Value: fmt.print(v.value)
        case Float_Value: fmt.print(v.value)
        case Bool_Value: fmt.print(v.value)
        case ^String_Value: fmt.print(strings.to_string(v.buf))
        case ^Array_Value: {
            fmt.print("[")

            print_comma := false
            for i in v.buf {
                if print_comma do fmt.print(", ")
                
                is_string := false
                #partial switch v1 in i {
                    case ^String_Value: is_string = true
                }
                
                if is_string do fmt.print("\"")
                print_value(i, false)
                if is_string do fmt.print("\"")

                print_comma = true
            }

            fmt.print("]")
        }
    }
    
    if newline do fmt.println()
}

@private
compare_values :: proc(v1, v2: Value) -> Bool_Value {
    switch v in v1 {
        case Int_Value: {
            r := v2.(Int_Value)
            return Bool_Value { v.value == r.value }
        }
        
        case Float_Value: {
            r := v2.(Float_Value)
            return Bool_Value { v.value == r.value }
        }
        
        case Bool_Value: {
            r := v2.(Bool_Value)
            return Bool_Value { v.value == r.value }
        }
        
        case ^String_Value: {
            r := v2.(^String_Value)
            return Bool_Value { strings.compare(strings.to_string(v.buf), strings.to_string(r.buf)) == 0 }
        }
        
        case ^Array_Value: {
            r := v2.(^Array_Value)

            if len(v.buf) != len(r.buf) do return Bool_Value { false }

            for i in 0..<len(r.buf) {
                if !compare_values(v.buf[i], r.buf[i]).value do return Bool_Value { false }
            }

            return Bool_Value { true }
        }
    }
    
    return Bool_Value { false }
}

get_value_type :: proc(value: Value) -> Value_Type {
    #partial switch v in value {
        case Int_Value: return .Int
        case Float_Value: return .Float
    }
    
    // Unreachable
    return .Int
}