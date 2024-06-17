let main: int of () do
    x: test(3, 4)
    print x
end

let test: int of (a: int, b: int) do
    return a * b
end

