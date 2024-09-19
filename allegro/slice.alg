type Slice<?T> = struct of
	start: ^?T,
	len: uint,
	cap: int

def Slice<?T> =
    let len of self =
        self.len

    let new of x: ?T = 
        Slice {start = %x, len = 1, cap = 32}

    let get of self, idx: uint =
        if idx <= len then
            return idx * self.start
        else 
            return -1

end