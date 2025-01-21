type Option<?T> = enum of
    Some(?T),
    None

def Option<?T> = 
    let is_some of self =
        if self is None then
            return false
        else return true
    
end