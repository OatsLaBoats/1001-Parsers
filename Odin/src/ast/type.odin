package ast

Type :: union {
    Base_Type,
    Array_Type,
}

Base_Type :: struct {
    id: string,
}

Array_Type :: struct {
    internal: Base_Type,
    nesting: uint, // default is 1
}

User_Type :: struct {
    fields: [dynamic]Type
}

BOOL_TYPE :: Base_Type { "Bool" }
INT_TYPE :: Base_Type { "Int" }
FLOAT_TYPE :: Base_Type { "Float" }
STRING_TYPE :: Base_Type { "String" }

is_type_equal :: proc(a: Type, b: Type) -> bool {
    a_id := get_type_kind(a)
    b_id := get_type_kind(b)
    
    if a_id != b_id do return false
    
    if a_id == 1 {
        ba := a.(Base_Type)
        bb := b.(Base_Type)
        
        if ba.id == bb.id do return true
    }
    
    if a_id == 2 {
        aa := a.(Array_Type)
        ab := b.(Array_Type)

        if (aa.internal.id == ab.internal.id) && (aa.nesting == ab.nesting) do return true
    }
        
    return false
}

is_array_type :: proc(t: Type) -> bool {
    return get_type_kind(t) == 2
}

is_base_type :: proc(t: Type) -> bool {
    return get_type_kind(t) == 1
}

get_array_type_internal :: proc(t: Type) -> Base_Type {
    a := t.(Array_Type)
    return a.internal
}

@private
get_type_kind :: proc(t: Type) -> int {
    switch v in t {
        case Base_Type: return 1
        case Array_Type: return 2
    }

    return 0
}