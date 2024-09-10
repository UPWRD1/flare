type Result = enum of
    Ok(int),
    Err(int)

def Result =
    let is_ok of x -> bool =
        x == Ok

    let is_err of x -> bool =
        x == Err
end

let main  = 
	a = Ok(3)