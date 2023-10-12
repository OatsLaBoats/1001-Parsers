package ast

Type :: union {
    Scalar_Type,
    Array_Type,
}

Scalar_Type :: string
Array_Type :: ^Type

is_type_equal :: proc(a: Type, b: Type) -> bool {
    a_id := get_type_id(a)
    b_id := get_type_id(b)
    
    if a_id != b_id do return false
    if a_id == 1 do return a.(Scalar_Type) == b.(Scalar_Type)
        
    return is_type_equal(a.(Array_Type)^, b.(Array_Type)^)
}

is_array_type :: proc(t: Type) -> bool {
    return get_type_id(t) == 2
}

get_array_type_internal :: proc(t: Type) -> Type {
    switch v in t {
        case Scalar_Type: return nil
        case Array_Type: return v^
    }
    
    return nil
}

// Can't think of a better name
@private
get_type_id :: proc(t: Type) -> int {
    switch v in t {
        case Scalar_Type: return 1
        case Array_Type: return 2
    }

    return 0
}