with "examples/result"

type Slice<?T> = struct of
	start: ^?T,
	len: uint,
	cap: int

def Slice<?T> =
    let len of self =
        self:len

    let new of x = 
        Slice {start = %x, len = 1, cap = 32}

    let get of self, idx: int -> ?T =
        if idx <= self:len then
            return idx * self:start
        else 
            return -1
end

let containshelper of s: Slice<?T>, e: ?T, c: int -> bool =
    if c < s.len() then
       if s.get(c).unwrap() == e then 
            return true 
       else 
            return containshelper(s, e, c + 1)
    else 
        return false

def Slice<?T> =
    let contains of self, item: ?T -> bool =
        containshelper(self, item, 0)
end


