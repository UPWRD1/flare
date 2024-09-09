type Result<?T, ?E> = enum of
    Ok(?T),
    Err(?E)

def Result =
    let is_ok of x -> bool =
        x == Ok

    let is_err of x -> bool =
        x == Err
end