package interpreter

import "core:fmt"
import "core:strings"

@private
Scope :: struct {
    parent: ^Scope,
    variables: map[string]Value,
    references: [dynamic]Value,
}

@private
delete_scope :: proc(scope: ^Scope) {
    delete(scope.variables)
    delete(scope.references)
    free(scope)
}

@private
drop_references :: proc(scope: ^Scope) {
    for v in scope.references {
        #partial switch var in v {
            case ^String_Value: {
                var.rc -= 1
                fmt.println("decrementing reference", var.rc,":", strings.to_string(var.buf))
                if var.rc <= 0 {
                    //fmt.println("dropping string:", strings.to_string(var.buf))
                    strings.builder_destroy(&var.buf)
                    free(var)
                }
            }
            
            case ^Array_Value: {
                var.rc -= 1
                fmt.println("decrementing reference", var.rc,":", var.buf)
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
            fmt.println("Incremented reference of", v.rc,":", strings.to_string(v.buf))
        }
        
        case ^Array_Value: {
            v.rc += 1
            fmt.println("Incremented reference of", v.rc,":", v.buf)
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