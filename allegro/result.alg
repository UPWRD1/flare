type Result<?T, ?E> = enum of
    Ok(?T),
    Err(?E)

def Result<?T, ?E> =
    let is_ok of x =
        x == Ok

    let is_err of x =
        x == Err
end