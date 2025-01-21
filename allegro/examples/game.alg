-- with "examples/player"

type Wrapper = struct of
    value: int
end

let new for Wrapper of v: int -> Wrapper = 
    return Wrapper {value: v}

let dump for Wrapper of x: int -> int =
    return x

let main = 
    w = Wrapper.new(3)
    w.dump()
