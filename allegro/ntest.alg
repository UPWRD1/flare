type Result = enum of
    Ok(int),
    Err(int)

def Result =
    let is_ok of x: Result -> bool =
        true

    let is_err of x -> bool =
        false
end

let main  = 
	a = Ok(3)