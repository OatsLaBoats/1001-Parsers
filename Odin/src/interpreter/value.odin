package interpreter

import "core:strings"
import "core:fmt"

// Used for upcasting of Int to Float
@private
Value_Type :: enum {
    Int, Float
}

@private
Value :: union {
    Int_Value,
    Float_Value,
    Bool_Value,
    ^String_Value,
    ^Array_Value,
}

@private
Int_Value :: struct {
    value: i64
}

@private
Float_Value :: struct {
    value: f64
}

@private
Bool_Value :: struct {
    value: bool
}

@private
String_Value :: struct {
    buf: strings.Builder,
    rc: int,
}

@private
Array_Value :: struct {
    buf: [dynamic]Value,
    rc: int,
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
            #partial switch n in v2 {
                case Int_Value: return Bool_Value { v.value == n.value }
                case Float_Value: return Bool_Value { f64(v.value) == n.value }
            }
        }
        
        case Float_Value: {
            #partial switch n in v2 {
                case Int_Value: return Bool_Value { v.value == f64(n.value) }
                case Float_Value: return Bool_Value { v.value == n.value }
            }
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

@private
get_value_type :: proc(value: Value) -> Value_Type {
    #partial switch v in value {
        case Int_Value: return .Int
        case Float_Value: return .Float
    }
    
    // Unreachable
    return .Int
}