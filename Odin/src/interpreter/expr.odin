package interpreter

import "core:strings"
import "../ast"

@private
eval_expression :: proc(env: ^Environment, scope: ^Scope, e: ^ast.Expression, rc_start_state: int = 1) -> Value {
    switch v in e {
        case ^ast.Primary_Expr: return eval_primary_expr(env, scope, v, rc_start_state)
        case ^ast.Binary_Expr: return eval_binary_expr(env, scope, v)
        case ^ast.Unary_Expr: return eval_unary_expr(env, scope, v)
    }
    
    return nil
}

@private
eval_unary_expr :: proc(env: ^Environment, scope: ^Scope, e: ^ast.Unary_Expr) -> Value {
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
eval_binary_expr :: proc(env: ^Environment, scope: ^Scope, e: ^ast.Binary_Expr) -> Value {
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
eval_primary_expr :: proc(env: ^Environment, scope: ^Scope, e: ^ast.Primary_Expr, rc_start_state: int = 1) -> Value {
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