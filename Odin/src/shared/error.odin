package shared

import s "core:strings"
import "core:fmt"

Error :: struct {
    info: Source_Info,
    msg: s.Builder
}

Error_List :: [dynamic]Error

make_detailed_error :: proc(info: Source_Info, format: string, args: ..any) -> Error {
    result := Error { info = info }
    s.builder_init(&result.msg)
    fmt.sbprintf(&result.msg, format, ..args)
    return result
}

make_simple_error :: proc(format: string, args: ..any) -> Error {
    return make_detailed_error({-1, -1}, format, ..args)
}

make_error :: proc {
    make_detailed_error,
    make_simple_error,
}

append_detailed_error :: proc(list: ^Error_List, info: Source_Info, format: string, args: ..any) {
    append(list, make_detailed_error(info, format, ..args))
}

append_simple_error :: proc(list: ^Error_List, format: string, args: ..any) {
    append(list, make_simple_error(format, ..args))
}

append_error :: proc {
    append_detailed_error,
    append_simple_error,
}

print_error :: proc(error: Error) {
    if error.info.line == -1 {
        fmt.printf("Error: %s\n", s.to_string(error.msg))
    } else if error.info.column == -1 {
        fmt.printf("Error(%d): %s\n", error.info.line, s.to_string(error.msg))
    } else {
        fmt.printf("Error(%d:%d): %s\n", error.info.line, error.info.column, s.to_string(error.msg))
    }
}

print_error_list :: proc(error_list: ^Error_List)  {
    for e in error_list {
        print_error(e)
    }
}

delete_error :: proc(error: Error) {
    e := error
    s.builder_destroy(&e.msg)
}

delete_error_list :: proc(error_list: Error_List) {
    for e in error_list {
        delete_error(e)
    }

    delete(error_list)
}