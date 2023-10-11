import sys

def gen_function(n: int) -> str:
    s =     f"fun func_{n}(a: Int, b: String, c: Float, d: Bool, e: [Int]): Int {{\n"
    s = s + f"    var v1: Int = a + 100\n"
    s = s + f"    var v2: String = b + \"a string\"\n"
    s = s + f"    var v3: Float = c + 9.8\n"
    s = s + f"    var v4: Bool = d and true\n"
    s = s + f"    e[0] = 1\n"
    s = s + f"    e[1] = 2\n"
    s = s + f"    e[2] = 3\n"
    s = s + f"    e[3] = 4\n"
    s = s + f"    e[4] = 5\n"
    s = s + f"    if d {{\n"
    s = s + f"        d = false\n"
    s = s + f"    }}\n"
    s = s + f"    return v1\n"
    s = s + f"}}\n\n"
    return s

def gen_main(n: int) -> str:
    s =     f"fun main(): Int {{\n"
    s = s + f"    var i: Int = 100\n"
    s = s + f"    var result: Int = 0\n"
    for i in range(n):
        s = s + f"    result = func_{i}(i, \"hello world\", 8.12, true, [1, 2, 3, 4])\n"
    s = s + f"    return 0\n"
    s = s + f"}}\n"
    return s

def gen(n: int) -> str:
    functions: str = ""
    for i in range(n):
        functions = functions + gen_function(i)
    main = gen_main(n)
    return functions + main
    

if __name__ == "__main__":
    if len(sys.argv) > 1:
        n = int(sys.argv[1])
        code = gen(n)
        with open("benchmark.sigma", "w") as f:
            f.write(code)